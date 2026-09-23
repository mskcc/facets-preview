#!/usr/bin/env Rscript
### Fixture tests for the VM samples-table side table (per-class best fit,
### reviewer, review state, purity/ploidy), the VM repository registry and
### DMP-id loader, and 2n pair-dir acceptance.
###
### Run: Rscript tests/test_manifest_vm.R
### Synthetic ids only -- no patient data.

suppressPackageStartupMessages({
  suppressWarnings({
    library(data.table)
    library(plyr)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(tibble)
    library(parallel)
    library(R.utils)
  })
})

repo <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))
suppressWarnings(suppressMessages({
  source(file.path(repo, "R", "global.R"))
  source(file.path(repo, "R", "global_2n.R"))
  source(file.path(repo, "R", "repositories.R"))
}))

AUTOQC <- autoqc_reviewer_id_2n()
HUMAN  <- "reviewer_a"

n_pass <- 0; n_fail <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { n_pass <<- n_pass + 1; cat("PASS:", label, "\n") }
  else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") }
}

row_of <- function(review_status, fit_name, reviewed_by, date_reviewed, sample = "S") {
  data.frame(sample = sample, path = "/synth/path/",
             review_status = review_status, fit_name = fit_name,
             review_notes = NA, reviewed_by = reviewed_by,
             date_reviewed = date_reviewed, facets_qc = "TRUE",
             use_only_purity_run = "FALSE", use_edited_cncf = "FALSE",
             reviewer_set_purity = NA,
             facets_qc_version = "1.0", facets_suite_version = "3.0.0",
             stringsAsFactors = FALSE)
}

# A sample dir with a manifest and a facets_qc.txt carrying purity/ploidy.
make_sample <- function(dir, rows, fits = c("default", "alt_dipLogR_-0.20"), mark_2n = FALSE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  qc <- data.frame(fit_name = fits, is_best_fit = FALSE,
                   purity_run_Purity = c(0.42, 0.61)[seq_along(fits)],
                   purity_run_Ploidy = c(2.1, 3.4)[seq_along(fits)],
                   hisens_run_Purity = c(0.40, 0.60)[seq_along(fits)],
                   hisens_run_Ploidy = c(2.0, 3.3)[seq_along(fits)],
                   facets_qc_version = "1.0", facets_suite_version = "3.0.0",
                   stringsAsFactors = FALSE)
  write.table(qc, file.path(dir, "facets_qc.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  if (!is.null(rows)) {
    con <- file(file.path(dir, "facets_review.manifest"), open = "wt")
    writeLines(manifest_header_2n(), con)
    write.table(rows[, manifest_columns_2n(), drop = FALSE], con,
                sep = "\t", row.names = FALSE, quote = FALSE)
    close(con)
  }
  for (f in fits) dir.create(file.path(dir, f), showWarnings = FALSE)
  if (mark_2n) file.create(file.path(dir, "default", "S.facets2n_normal_selection.txt"))
  dir
}
squash <- function(p) gsub('(?<!:)//+', '/', p, perl = TRUE)
root <- squash(tempfile("vmtab_"))

## ---------------------------------------------------------------------------
## 1. review_summary_for_dir -- standard rule
## ---------------------------------------------------------------------------

s1 <- make_sample(file.path(root, "impact", "facets", "all", "P-00000", "S1"),
                  rbind(row_of("reviewed_best_fit", "default", HUMAN, "2026-01-02 10:00:00"),
                        row_of("reviewed_acceptable_fit", "alt_dipLogR_-0.20", HUMAN, "2026-01-01 10:00:00")))
r <- review_summary_for_dir("S1", s1, is_2n = FALSE)
check("standard: human best fit wins", r$best_fit == "default" && r$state == "Human best")
check("standard: reviewer is the human", r$reviewed_by == HUMAN)
check("standard: purity/ploidy come from the best fit's purity run",
      isTRUE(all.equal(r$purity, 0.42)) && isTRUE(all.equal(r$ploidy, 2.1)))

s2 <- make_sample(file.path(root, "impact", "facets", "all", "P-00000", "S2"),
                  rbind(row_of("not_reviewed", "default", NA, NA),
                        row_of("not_reviewed", "alt_dipLogR_-0.20", NA, NA)))
r2 <- review_summary_for_dir("S2", s2, is_2n = FALSE)
check("standard: not_reviewed rows only -> Unreviewed, no best fit",
      is.na(r2$best_fit) && r2$state == "Unreviewed" && is.na(r2$purity))

s3 <- make_sample(file.path(root, "other", "S3"),
                  rbind(row_of("reviewed_acceptable_fit", "alt_dipLogR_-0.20", HUMAN, "2026-01-01 10:00:00")))
r3 <- review_summary_for_dir("S3", s3, is_2n = FALSE)
check("standard: acceptable-only is the best fit and says so",
      r3$best_fit == "alt_dipLogR_-0.20" && r3$state == "Acceptable only" &&
        isTRUE(all.equal(r3$purity, 0.61)))

s4 <- make_sample(file.path(root, "other", "S4"),
                  rbind(row_of("reviewed_no_fit", "Not selected", HUMAN, "2026-01-01 10:00:00")))
r4 <- review_summary_for_dir("S4", s4, is_2n = FALSE)
check("standard: reviewed_no_fit -> No fit", is.na(r4$best_fit) && r4$state == "No fit")

s5 <- make_sample(file.path(root, "other", "S5"), rows = NULL)
r5 <- review_summary_for_dir("S5", s5, is_2n = FALSE)
check("no manifest at all -> Unreviewed", r5$state == "Unreviewed")
check("missing dir -> Unreviewed, no error",
      review_summary_for_dir("X", file.path(root, "nope"), FALSE)$state == "Unreviewed")

# The samples table and update_best_fit_status must agree.
update_best_fit_status("S1", s1)
qc1 <- fread(file.path(s1, "facets_qc.txt"))
check("resolve_best_fit_standard matches what update_best_fit_status writes",
      identical(qc1$fit_name[qc1$is_best_fit], "default"))

## ---------------------------------------------------------------------------
## 2. review_summary_for_dir -- 2n rule (13/14/15)
## ---------------------------------------------------------------------------

pair <- file.path(root, "impact_2n", "facets", "all", "P-00000", "P-0000000-T01-IM6_P-0000000-N01-IM6")
rs_dir <- make_sample(file.path(pair, "research"),
                      rbind(row_of("auto_qc_best_fit", "default", AUTOQC, "2026-02-01 01:00:00"),
                            row_of("auto_qc_pass", "alt_dipLogR_-0.20", AUTOQC, "2026-02-01 01:00:00"),
                            row_of("not_reviewed", "default/ultra", NA, NA),
                            row_of("reviewed_no_fit", "Not selected", HUMAN, "2026-02-02 01:00:00")),
                      mark_2n = TRUE)
dir.create(file.path(rs_dir, "default", "ultra"), showWarnings = FALSE)
cl_dir <- make_sample(file.path(pair, "clinical"),
                      rbind(row_of("reviewed_acceptable_fit", "alt_dipLogR_-0.20", HUMAN, "2026-02-03 01:00:00")),
                      mark_2n = TRUE)

rr <- review_summary_for_dir("P", rs_dir, is_2n = TRUE)
check("2n: autoQC best wins; a human no-fit does not veto it (rule 14)",
      rr$best_fit == "default" && rr$state == "AutoQC best")
check("2n: reviewer is the autoQC identity", rr$reviewed_by == AUTOQC)
rc <- review_summary_for_dir("P", cl_dir, is_2n = TRUE)
check("2n: acceptable-only is NOT a best fit under rule 13, but is reported as a state",
      is.na(rc$best_fit) && rc$state == "Acceptable only")

## ---------------------------------------------------------------------------
## 3. Registry, labels, and the DMP-id loader
## ---------------------------------------------------------------------------

reg <- vm_repository_registry()
check("registry: the four IMPACT repositories, in order",
      identical(reg$label, c("IMPACT Standard", "IMPACT 2n", "IMPACT-Heme Standard", "IMPACT-Heme 2n")))
check("registry: bases end in the bucket root",
      all(grepl("/facets/all$", reg$base)))
reg_ov <- vm_repository_registry(list(repo_base_impact_2n = "/elsewhere/2n/"))
check("registry: a global.config override replaces one base (trailing slash dropped)",
      reg_ov$base[reg_ov$key == "impact_2n"] == "/elsewhere/2n" &&
        reg_ov$base[reg_ov$key == "impact"] == reg$base[reg$key == "impact"])

# A registry rooted at the fixture tree.
treg <- reg
treg$base <- file.path(root, sub("^/data1/core006/ccs/shared/resources/", "", reg$base))
check("label: standard path -> IMPACT Standard",
      repository_label_for_path(s1, treg) == "IMPACT Standard")
check("label: 2n class dir -> IMPACT 2n",
      repository_label_for_path(rs_dir, treg) == "IMPACT 2n")
check("label: heme 2n is not confused with heme standard",
      repository_label_for_path(file.path(root, "impact_heme_2n/facets/all/P-00000/X"), treg) == "IMPACT-Heme 2n" &&
        repository_label_for_path(file.path(root, "impact_heme/facets/all/P-00000/X"), treg) == "IMPACT-Heme Standard")
check("label: outside every base -> Other",
      repository_label_for_path(s3, treg) == "Other" && repository_label_for_path(NA, treg) == "Other")
check("label: doubled slashes are tolerated",
      repository_label_for_path(paste0(root, "//impact/facets/all//P-00000/S1/"), treg) == "IMPACT Standard")

pat <- dmp_id_patterns()
check("ids: heme short and full forms are accepted",
      grepl(pat$short, "P-0000002-T01-IH4") &&
        grepl(pat$full, "P-0000002-T01-IH4_P-0000002-N01-IH4") &&
        !grepl(pat$short, "P-0000002-T01-XX4"))

base <- file.path(root, "impact_heme", "facets", "all")
dir.create(file.path(base, "P-00000", "P-0000001-T01-IM6_P-0000001-N01-IM6"), recursive = TRUE)
dir.create(file.path(base, "P-00000", "P-0000002-T01-IH4_P-0000002-N01-IH4"), recursive = TRUE)
check("loader: short IM id resolves to its pair dir",
      resolve_repo_sample_path("P-0000001-T01-IM6", base)$path ==
        file.path(base, "P-00000", "P-0000001-T01-IM6_P-0000001-N01-IM6"))
check("loader: short heme id resolves too",
      resolve_repo_sample_path(" P-0000002-T01-IH4 ", base)$path ==
        file.path(base, "P-00000", "P-0000002-T01-IH4_P-0000002-N01-IH4"))
check("loader: full id resolves directly",
      resolve_repo_sample_path("P-0000002-T01-IH4_P-0000002-N01-IH4", base)$path ==
        file.path(base, "P-00000", "P-0000002-T01-IH4_P-0000002-N01-IH4"))
check("loader: a bad format is an error, not a path",
      is.null(resolve_repo_sample_path("bogus", base)$path) &&
        grepl("Invalid format", resolve_repo_sample_path("bogus", base)$error))
check("loader: an unknown id names the bucket it searched",
      grepl(file.path(base, "P-00000"), resolve_repo_sample_path("P-0000009-T01-IM6", base)$error, fixed = TRUE))
check("loader: a missing bucket is an error",
      !is.null(resolve_repo_sample_path("P-0099999-T01-IM6", base)$error))
check("loader: a missing full-form dir is an error",
      !is.null(resolve_repo_sample_path("P-0000001-T02-IM6_P-0000001-N01-IM6", base)$error))

## ---------------------------------------------------------------------------
## 4. Pair-dir acceptance
## ---------------------------------------------------------------------------

check("pair dir -> its research class dir",
      resolve_2n_pair_dir(pair) == squash(file.path(pair, "research")))
check("class dir is returned unchanged", resolve_2n_pair_dir(rs_dir) == rs_dir)
check("standard dir is returned unchanged", resolve_2n_pair_dir(s1) == s1)
check("missing dir is returned unchanged",
      resolve_2n_pair_dir(file.path(root, "nope")) == file.path(root, "nope"))
pair_c <- file.path(root, "impact_2n", "facets", "all", "P-00001", "P-0000001-T01-IM6_P-0000001-N01-IM6")
make_sample(file.path(pair_c, "clinical"), rows = NULL, mark_2n = TRUE)
check("clinical-only pair -> its clinical class dir",
      resolve_2n_pair_dir(pair_c) == file.path(pair_c, "clinical"))

## ---------------------------------------------------------------------------
## 5. manifest_extra_vm: one row per sample, same order, per-class cells
## ---------------------------------------------------------------------------

# The standard autoQC signs "auto-qc-script" and uses the HUMAN vocabulary.
s6 <- make_sample(file.path(root, "impact", "facets", "all", "P-00000", basename(pair)),
                  rbind(row_of("reviewed_best_fit", "default", "auto-qc-script", "2026-07-31 16:27:43"),
                        row_of("reviewed_no_fit", "Not selected", "auto-qc-script", "2023-11-16 20:43:02")))
r6 <- review_summary_for_dir(basename(pair), s6, is_2n = FALSE)
check("standard: auto-qc-script best is labelled AutoQC best, not Human best",
      r6$best_fit == "default" && r6$state == "AutoQC best" && r6$reviewed_by == "auto-qc-script")
check("standard: an auto-qc-script no-fit alone is an automated no-fit",
      review_summary_for_dir("X", make_sample(file.path(root, "other", "S7"),
        rbind(row_of("reviewed_no_fit", "Not selected", "auto-qc-script", "2023-11-16 20:43:02"))),
        FALSE)$state == "No fit (auto-qc)")

check("reviewer: anything with auto-qc in the name is automation",
      all(is_autoqc_reviewer_2n(c("auto-qc", "auto-qc-script", "richara4/auto-qc-script", "AutoQC-v2"))) &&
        !any(is_autoqc_reviewer_2n(c("richara4", NA, "reviewer_a"))))

# Same pair tag loaded from BOTH the standard and the 2n repository.
mm <- data.frame(sample_id = c("S1", basename(pair), basename(pair), "S3"),
                 path = c(s1, s6, rs_dir, s3), stringsAsFactors = FALSE)
ex <- manifest_extra_vm(mm, treg)
check("extra: keyed by path -- the standard copy of a pair tag is NOT summarised as 2n",
      ex$repository[2] == "IMPACT Standard" && is.na(ex$clinical_best_fit[2]) &&
        ex$research_reviewed_by[2] == "auto-qc-script" && ex$review_state[2] == "AutoQC best")
check("extra: ...and the 2n copy of the same tag is",
      ex$repository[3] == "IMPACT 2n" && ex$research_reviewed_by[3] == AUTOQC &&
        ex$review_state[3] == "AutoQC best")
mm <- mm[c(1, 3, 4), ]
ex <- manifest_extra_vm(mm, treg)
check("extra: one row per metadata row, same order", identical(ex$sample_id, mm$sample_id))
check("extra: standard row has research best fit and NA clinical",
      ex$research_best_fit[1] == "default" && is.na(ex$clinical_best_fit[1]) &&
        ex$research_reviewed_by[1] == HUMAN && ex$review_state[1] == "Human best")
check("extra: 2n row carries both classes",
      ex$research_best_fit[2] == "default" && ex$research_reviewed_by[2] == AUTOQC &&
        is.na(ex$clinical_best_fit[2]) && is.na(ex$clinical_reviewed_by[2]) &&
        ex$review_state[2] == "AutoQC best")
check("extra: repository labels", identical(ex$repository, c("IMPACT Standard", "IMPACT 2n", "Other")))
check("extra: purity/ploidy from the primary (research) class",
      isTRUE(all.equal(ex$purity[2], 0.42)) && isTRUE(all.equal(ex$ploidy[3], 3.4)))
check("extra: empty input -> empty frame with the right columns",
      nrow(manifest_extra_vm(mm[0, ], treg)) == 0 &&
        "review_state" %in% names(manifest_extra_vm(NULL, treg)))

## ---------------------------------------------------------------------------
## 6. pick_manifest_row_2n: one row for a tag loaded from two repositories
## ---------------------------------------------------------------------------

mm2 <- data.frame(sample_id = c("S1", basename(pair), basename(pair)),
                  path = c(s1, s6, rs_dir), default_fit_name = c("default", "default", "alt"),
                  stringsAsFactors = FALSE)
check("pick: unique tag -> its row", pick_manifest_row_2n(mm2, "S1")$path == s1)
check("pick: absent tag -> NULL", is.null(pick_manifest_row_2n(mm2, "nope")))
check("pick: ambiguous tag, no preference -> first row (standard copy)",
      pick_manifest_row_2n(mm2, basename(pair))$path == s6)
check("pick: ambiguous tag, loaded path is the 2n research dir -> the 2n row",
      pick_manifest_row_2n(mm2, basename(pair), rs_dir)$path == rs_dir)
check("pick: ambiguous tag, loaded path is the 2n CLINICAL dir (after a class swap) -> still the 2n row",
      pick_manifest_row_2n(mm2, basename(pair), cl_dir)$path == rs_dir)
check("pick: ambiguous tag, loaded path is the standard dir -> the standard row",
      pick_manifest_row_2n(mm2, basename(pair), s6)$path == s6)
check("pick: nothing to look up -> NULL", is.null(pick_manifest_row_2n(mm2[0, ], "S1")))

## ---------------------------------------------------------------------------
## 7. sample_choices_2n: unique dropdown entries, bare when the tag is unique
## ---------------------------------------------------------------------------

ch <- sample_choices_2n(mm2, treg)
check("choices: one entry per row, same order", identical(ch$path, mm2$path))
check("choices: a unique tag stays bare", ch$key[1] == "S1")
check("choices: duplicated tags carry the repository label",
      ch$key[2] == paste0(basename(pair), " (IMPACT Standard)") &&
        ch$key[3] == paste0(basename(pair), " (IMPACT 2n)"))
ch0 <- sample_choices_2n(mm2, NULL)
check("choices: without a registry, duplicates are told apart by 2n-ness",
      ch0$key[2] == paste0(basename(pair), " (Standard)") &&
        ch0$key[3] == paste0(basename(pair), " (2n)"))
mm3 <- rbind(mm2, mm2[3, ])
check("choices: keys are always unique", !any(duplicated(sample_choices_2n(mm3, treg)$key)))
check("choices: empty input -> empty frame", nrow(sample_choices_2n(mm2[0, ], treg)) == 0)

cat("\n", n_pass, "passed,", n_fail, "failed\n")
if (n_fail > 0) quit(status = 1)
