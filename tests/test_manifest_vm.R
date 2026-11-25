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
  source(file.path(repo, "R", "wes_mapping.R"))
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
check("registry: the four IMPACT repositories then Whole Exome, in order",
      identical(reg$label, c("IMPACT Standard", "IMPACT 2n", "IMPACT-Heme Standard", "IMPACT-Heme 2n",
                             "Whole Exome")))
check("registry: IMPACT bases end in the bucket root",
      all(grepl("/facets/all$", reg$base[reg$layout == "dmp_bucket"])))
check("registry: only Whole Exome is read-only, and it uses the tempo layout",
      identical(reg$key[reg$read_only], "wes") && identical(reg$key[reg$layout == "tempo"], "wes"))
reg_ov <- vm_repository_registry(list(repo_base_impact_2n = "/elsewhere/2n/"))
check("registry: a global.config override replaces one base (trailing slash dropped)",
      reg_ov$base[reg_ov$key == "impact_2n"] == "/elsewhere/2n" &&
        reg_ov$base[reg_ov$key == "impact"] == reg$base[reg$key == "impact"])

# A registry rooted at the fixture tree.
treg <- reg
treg$base <- file.path(root, sub("^/data1/core006/ccs/shared/resources/", "", reg$base))
treg$base[treg$key == "wes"] <- file.path(root, "wes", "somatic")
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

## ---------------------------------------------------------------------------
## 8. PNG sizing from the image header
## ---------------------------------------------------------------------------

write_png <- function(path, w, h) {
  # A minimal valid-enough PNG: signature + IHDR (only the header is read).
  be <- function(n) as.raw(c(n %/% 256^3, (n %/% 256^2) %% 256, (n %/% 256) %% 256, n %% 256))
  con <- file(path, "wb"); on.exit(close(con))
  writeBin(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)), con)
  writeBin(be(13), con); writeBin(charToRaw("IHDR"), con)
  writeBin(be(w), con); writeBin(be(h), con)
  writeBin(as.raw(c(8, 2, 0, 0, 0)), con); writeBin(as.raw(rep(0, 4)), con)
  path
}
std <- write_png(tempfile(fileext = ".png"), 3218, 3960)   # 9.75x12 in @ 330 dpi
twn <- write_png(tempfile(fileext = ".png"), 2700, 2400)   # 9x8 in @ 300 dpi
check("png: dimensions come from the IHDR chunk", identical(png_dimensions(std), c(3218, 3960)))
check("png: the standard suite's aspect keeps the classic 650x800",
      identical(png_display_size(std), list(width = 650L, height = 800L)))
old <- write_png(tempfile(fileext = ".png"), 850, 999)     # older suite output
check("png: the older suite's 850x999 also keeps the classic size",
      identical(png_display_size(old), list(width = 650L, height = 800L)))
check("png: the 2n wrapper's aspect keeps the height and widens",
      identical(png_display_size(twn), list(width = 900L, height = 800L)))
check("png: an unreadable file falls back to the classic size",
      identical(png_display_size(file.path(root, "nope.png")), list(width = 650L, height = 800L)) &&
        all(is.na(png_dimensions(file.path(root, "nope.png")))))
notpng <- tempfile(fileext = ".png"); writeLines("hello", notpng)
check("png: a non-PNG is not parsed", all(is.na(png_dimensions(notpng))))

## ---------------------------------------------------------------------------
## Whole Exome: id expansion, the sample tracker, associations, read-only
## ---------------------------------------------------------------------------

wes_base <- treg$base[treg$key == "wes"]
wes_pair <- function(tumor, normal, facets = TRUE) {
  pair <- paste0(tumor, "__", normal)
  d <- file.path(wes_base, pair, "facets", pair)
  if (facets) dir.create(file.path(d, "facets0.5.14c100pc500"), recursive = TRUE)
  else dir.create(file.path(wes_base, pair, "combined_mutations"), recursive = TRUE)
  d
}
w1  <- wes_pair("C-000001-P001-d", "C-000001-N001-d")
w1b <- wes_pair("C-000001-P002-d01", "C-000001-N001-d")
w11 <- wes_pair("C-000001-P0011-d", "C-000001-N001-d")
w2  <- wes_pair("C-000002-T001-d", "C-000002-N001-d")
wnf <- wes_pair("C-000003-P001-d", "C-000003-N001-d", facets = FALSE)

rp <- function(id, tracker = NULL) sort(resolve_wes_sample_paths(id, wes_base, tracker)$paths)
check("wes: a patient-level CMO id returns every tumor of that patient",
      identical(rp("C-000001"), sort(c(w1, w1b, w11))))
check("wes: a sample id stops at the token boundary (P001 is not P0011)",
      identical(rp("C-000001-P001"), w1))
check("wes: the full tumor id resolves", identical(rp(" C-000001-P001-d "), w1))
check("wes: a partial last token still matches (-d -> -d01)",
      identical(rp("C-000001-P002-d"), w1b))
check("wes: the TUMOR__NORMAL pair resolves directly",
      identical(rp("C-000001-P001-d__C-000001-N001-d"), w1))
check("wes: a pair without a facets dir is an error, not a path",
      length(rp("C-000003")) == 0 &&
        grepl("No matching folder", resolve_wes_sample_paths("C-000003", wes_base)$error))
check("wes: an unknown id and a malformed id are errors",
      !is.null(resolve_wes_sample_paths("C-999999", wes_base)$error) &&
        grepl("Invalid format", resolve_wes_sample_paths("../etc", wes_base)$error))

tracker_file <- file.path(root, "sample_tracker.txt")
write_tracker <- function(rows) {
  df <- do.call(rbind, lapply(rows, function(r)
    data.frame(CMO_Sample_ID = r[1], Other = "x", tumorOrNormal = r[2],
               investigatorSampleId = r[3], cmoSampleName = r[1], stringsAsFactors = FALSE)))
  write.table(df, tracker_file, sep = "\t", row.names = FALSE, quote = FALSE)
}
write_tracker(list(c("C-000001-P001-d", "Tumor", "P-0000001-T01-WES"),
                   c("C-000001-P002-d01", "Tumor", "P-0000001-T02-WES_2"),
                   c("C-000001-N001-d", "Normal", "P-0000001-N01-WES"),
                   c("C-000002-T001-d", "Tumor", "P-0000007-T01-WES,P-0000008-T03-WES"),
                   c("C-000004-T001-d", "Tumor", "some_collaborator_id")))
trk <- load_wes_tracker(tracker_file)
check("tracker: DMP-shaped rows only, comma-joined ids split, T/N classified",
      nrow(trk) == 5 && !("C-000004-T001-d" %in% trk$cmo_id) &&
        sum(trk$cmo_id == "C-000002-T001-d") == 2 &&
        trk$class[trk$cmo_id == "C-000001-N001-d"] == "normal" &&
        trk$tnum[trk$dmp_id == "P-0000001-T02-WES_2"] == "T02")
check("tracker: a missing file is an empty table",
      nrow(suppressWarnings(load_wes_tracker(file.path(root, "nope.txt")))) == 0 &&
        nrow(load_wes_tracker(NULL)) == 0)

check("wes: a DMP sample id loads the exome of the same sample number",
      identical(rp("P-0000001-T01-IM6", trk), w1) && identical(rp("P-0000001-T02", trk), w1b))
check("wes: a DMP patient id loads every mapped exome tumor",
      identical(rp("P-0000001", trk), sort(c(w1, w1b))))
check("wes: an unmapped DMP id, and a DMP id with no tracker, are errors",
      grepl("No Whole Exome sample is mapped", resolve_wes_sample_paths("P-0000001-T09-IM6", wes_base, trk)$error) &&
        grepl("tracker is not available", resolve_wes_sample_paths("P-0000001-T01-IM6", wes_base, NULL)$error))
check("dispatch: the registry layout picks the resolver",
      identical(resolve_repo_entry_paths("C-000001-P001", treg[treg$key == "wes", ], trk)$paths, w1) &&
        identical(resolve_repo_entry_paths("P-0000002-T01-IH4", treg[treg$key == "impact_heme", ])$paths,
                  file.path(base, "P-00000", "P-0000002-T01-IH4_P-0000002-N01-IH4")))

# The tracker is re-read when the file changes, and only then.
write_tracker(list(c("C-000001-P001-d", "Tumor", "P-0000001-T05-WES")))
Sys.setFileTime(tracker_file, Sys.time() + 60)
check("tracker: a changed file is picked up without a restart",
      identical(load_wes_tracker(tracker_file)$tnum, "T05"))
writeLines("garbage with no header columns", tracker_file)
Sys.setFileTime(tracker_file, Sys.time() + 120)
check("tracker: an unreadable rewrite keeps the last good table",
      identical(suppressWarnings(load_wes_tracker(tracker_file))$tnum, "T05"))

# Associations: IMPACT <-> 2n by identical tumor id, anything <-> WES by patient + number.
im_base <- treg$base[treg$key == "impact"]
im_t1 <- file.path(im_base, "P-00000", "P-0000001-T01-IM6_P-0000001-N01-IM6")
im_t3 <- file.path(im_base, "P-00000", "P-0000001-T03-IM6_P-0000001-N01-IM6")
dir.create(im_t1, recursive = TRUE); dir.create(im_t3, recursive = TRUE)
pair_a <- file.path(treg$base[treg$key == "impact_2n"], "P-00000", "P-0000001-T01-IM6_P-0000001-N01-IM6")
dir.create(file.path(pair_a, "clinical"), recursive = TRUE)
# (the loader fixture above left a P-0000001-T01-IM6 pair under the heme base)
a_im <- find_associated_samples(im_t1, treg, trk)
check("assoc: IMPACT sample -> its exome and the identical tumor id elsewhere",
      setequal(a_im$repo_key, c("wes", "impact_2n", "impact_heme")) &&
        a_im$path[a_im$repo_key == "wes"] == w1 && a_im$sample_id[a_im$repo_key == "wes"] == "C-000001-P001-d" &&
        a_im$path[a_im$repo_key == "impact_2n"] == pair_a)
im_p2 <- file.path(im_base, "P-00000", "P-0000002-T01-IM6_P-0000002-N01-IM6")
dir.create(im_p2, recursive = TRUE)
check("assoc: an IMPACT sample is not linked to a heme sample that only shares its number",
      nrow(find_associated_samples(im_p2, treg, trk)) == 0)
check("assoc: no exome with that sample number -> no Whole Exome row",
      !("wes" %in% find_associated_samples(im_t3, treg, trk)$repo_key))
a_w <- find_associated_samples(w1, treg, trk)
check("assoc: exome -> every IMPACT-family repository holding that patient + number",
      setequal(a_w$repo_key, c("impact", "impact_2n", "impact_heme")) &&
        a_w$path[a_w$repo_key == "impact"] == im_t1 && a_w$path[a_w$repo_key == "impact_2n"] == pair_a &&
        a_w$sample_id[a_w$repo_key == "impact"] == "P-0000001-T01-IM6")
check("assoc: a 2n class dir still finds its copies",
      "wes" %in% find_associated_samples(file.path(pair_a, "clinical"), treg, trk)$repo_key)
check("assoc: an untracked exome, a path outside every repository, and no tracker give nothing",
      nrow(find_associated_samples(w11, treg, trk)) == 0 &&
        nrow(find_associated_samples(s3, treg, trk)) == 0 &&
        !("wes" %in% find_associated_samples(im_t1, treg, NULL)$repo_key))

# Read-only marks where users may not refit / review; it does not stop the
# app's own manifest and qc bookkeeping.
ro <- make_sample(w2, rows = rbind(row_of("reviewed_best_fit", "default", HUMAN, "2025-01-01 10:00:00")))
check("read-only: off unless the option is set", !is_read_only_path(w2))
options(fp.read_only_bases = wes_base)
check("read-only: the base and everything under it, but not its neighbours",
      is_read_only_path(w2) && is_read_only_path(paste0(wes_base, "//")) &&
        !is_read_only_path(paste0(wes_base, "_other/x")) && !is_read_only_path(im_t1) &&
        !is_read_only_path(NA))
check("read-only: load-time bookkeeping can still write the manifest and qc file",
      verify_access_to_write(w2) && nrow(load_reviews(basename(w2), w2)) == 1)
options(fp.read_only_bases = NULL)

cat("\n", n_pass, "passed,", n_fail, "failed\n")
if (n_fail > 0) quit(status = 1)
