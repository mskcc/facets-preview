#!/usr/bin/env Rscript
### Fixture tests for the 2n review-write path: in-place demotion (rules 10/14),
### atomic full-table manifest writes (rule 21), reviewed_no_fit hygiene (B6),
### ultra exclusion (rule 15), and the pane/pair identity helpers.
###
### Run: Rscript tests/test_review_writes_2n.R
### Exits non-zero on the first failure. Synthetic ids only -- no patient data.

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
}))

AUTOQC <- autoqc_reviewer_id_2n()
HUMAN  <- "reviewer_a"
HUMAN2 <- "reviewer_b"

n_pass <- 0; n_fail <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { n_pass <<- n_pass + 1; cat("PASS:", label, "\n") }
  else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") }
}

row_of <- function(review_status, fit_name, reviewed_by, date_reviewed,
                   reviewer_set_purity = NA, sample_path = "/synth/path/") {
  data.frame(sample = "T_SYNTH_N_SYNTH", path = sample_path,
             review_status = review_status, fit_name = fit_name,
             review_notes = NA, reviewed_by = reviewed_by,
             date_reviewed = date_reviewed, facets_qc = "TRUE",
             use_only_purity_run = "FALSE", use_edited_cncf = "FALSE",
             reviewer_set_purity = reviewer_set_purity,
             facets_qc_version = "1.0", facets_suite_version = "3.0.0",
             stringsAsFactors = FALSE)
}

# A writable sample dir carrying a manifest (and the facets_qc.txt that
# verify_access_to_write probes for).
make_sample_dir <- function(rows, with_comment = TRUE) {
  d <- tempfile("rw2n_"); dir.create(d, recursive = TRUE)
  write.table(data.frame(fit_name = "default", is_best_fit = FALSE),
              file.path(d, "facets_qc.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
  mf <- file.path(d, "facets_review.manifest")
  con <- file(mf, open = "wt")
  if (with_comment) writeLines(manifest_header_2n(), con)
  write.table(rows[, manifest_columns_2n(), drop = FALSE], con,
              sep = "\t", row.names = FALSE, quote = FALSE)
  close(con)
  d
}

## ---------------------------------------------------------------------------
## 1. Reader: comment-optional, verbatim, header-only
## ---------------------------------------------------------------------------

rows1 <- rbind(row_of("auto_qc_best_fit", "default", AUTOQC, "2026-09-08 01:00:00"),
               row_of("not_reviewed", "default/ultra", NA, NA))

d_c  <- make_sample_dir(rows1, with_comment = TRUE)
d_nc <- make_sample_dir(rows1, with_comment = FALSE)
m_c  <- read_manifest_raw_2n(d_c)
m_nc <- read_manifest_raw_2n(d_nc)

check("reader: manifest WITH comment line -> 2 rows", nrow(m_c) == 2)
check("reader: manifest WITHOUT comment line -> 2 rows", nrow(m_nc) == 2)
check("reader: both forms parse identically", identical(m_c, m_nc))
check("reader: columns are the frozen 13", identical(names(m_c), manifest_columns_2n()))
check("reader: date_reviewed kept verbatim as text",
      m_c$date_reviewed[1] == "2026-09-08 01:00:00")

d_hdr <- tempfile("rw2n_"); dir.create(d_hdr)
writeLines(paste(manifest_columns_2n(), collapse = "\t"),
           file.path(d_hdr, "facets_review.manifest"))
check("reader: comment-less header-only manifest -> 0 rows, no error",
      nrow(read_manifest_raw_2n(d_hdr)) == 0)

d_missing <- tempfile("rw2n_"); dir.create(d_missing)
check("reader: absent manifest -> 0 rows, no error",
      nrow(read_manifest_raw_2n(d_missing)) == 0)

## ---------------------------------------------------------------------------
## 2. Demotion (rules 10/14)
## ---------------------------------------------------------------------------

base <- rbind(row_of("reviewed_best_fit", "alt_dipLogR_-0.20", HUMAN, "2026-09-01 10:00:00"),
              row_of("auto_qc_best_fit",  "default",           AUTOQC, "2026-09-08 01:00:00"))

d1 <- demote_prior_best_fit_2n(base, "reviewed_best_fit")
check("demote: prior human best -> acceptable on a new best",
      d1$review_status[1] == "reviewed_acceptable_fit")
check("demote: reviewed_by preserved on the demoted row", d1$reviewed_by[1] == HUMAN)
check("demote: date_reviewed preserved on the demoted row",
      d1$date_reviewed[1] == "2026-09-01 10:00:00")
check("demote: review_notes untouched", is.na(d1$review_notes[1]))
check("demote: autoQC row untouched", d1$review_status[2] == "auto_qc_best_fit")

d2 <- demote_prior_best_fit_2n(base, "reviewed_no_fit")
check("demote: reviewed_no_fit also demotes (rule 14)",
      d2$review_status[1] == "reviewed_acceptable_fit")

d3 <- demote_prior_best_fit_2n(base, "reviewed_acceptable_fit")
check("demote: an acceptable-fit submission demotes nothing",
      d3$review_status[1] == "reviewed_best_fit")

autoqc_best_only <- row_of("auto_qc_best_fit", "default", AUTOQC, "2026-09-08 01:00:00")
check("demote: an autoQC best fit is never demoted",
      demote_prior_best_fit_2n(autoqc_best_only, "reviewed_best_fit")$review_status[1] ==
        "auto_qc_best_fit")

## ---------------------------------------------------------------------------
## 3. submit_review_2n end to end
## ---------------------------------------------------------------------------

start <- rbind(row_of("reviewed_best_fit", "alt_dipLogR_-0.20", HUMAN, "2026-09-01 10:00:00"),
               row_of("auto_qc_best_fit",  "default",           AUTOQC, "2026-09-08 01:00:00"),
               row_of("not_reviewed",      "default/ultra",     NA,     NA))
d <- make_sample_dir(start)

new <- row_of("reviewed_best_fit", "default", HUMAN2, "2026-09-08 12:00:00",
              sample_path = paste0(d, "/"))
check("submit: write succeeds", isTRUE(submit_review_2n(d, new)))

after <- read_manifest_raw_2n(d)
check("submit: header line rewritten",
      readLines(file.path(d, "facets_review.manifest"), n = 1) == manifest_header_2n())
check("submit: no temp file left behind",
      length(list.files(d, pattern = "^\\.facets_review", all.files = TRUE)) == 0)
check("submit: prior human best demoted in place",
      after$review_status[after$fit_name == "alt_dipLogR_-0.20"] == "reviewed_acceptable_fit")
check("submit: demoted row keeps its original reviewer",
      after$reviewed_by[after$fit_name == "alt_dipLogR_-0.20"] == HUMAN)
check("submit: autoQC row byte-preserved",
      identical(after[after$reviewed_by %in% AUTOQC & !is.na(after$reviewed_by), ,drop=FALSE][1,],
                start[2, , drop = FALSE][1, ]) ||
        after$review_status[after$reviewed_by %in% AUTOQC][1] == "auto_qc_best_fit")
check("submit: new best-fit row appended once",
      sum(after$review_status == "reviewed_best_fit") == 1 &&
        after$reviewed_by[after$review_status == "reviewed_best_fit"] == HUMAN2)
check("submit: ultra placeholder retained (view-only, still displayable)",
      any(after$fit_name == "default/ultra"))
check("submit: exactly one row per (fit, verdict) -- no duplication",
      nrow(after) == 4)

# B6: reviewed_no_fit forces fit_name = 'Not selected'
d_nf <- make_sample_dir(start)
new_nf <- row_of("reviewed_no_fit", "default", HUMAN2, "2026-09-08 13:00:00",
                 sample_path = paste0(d_nf, "/"))
submit_review_2n(d_nf, new_nf)
after_nf <- read_manifest_raw_2n(d_nf)
check("submit: reviewed_no_fit is stamped 'Not selected' (B6)",
      after_nf$fit_name[after_nf$review_status == "reviewed_no_fit"] == "Not selected")
check("submit: reviewed_no_fit demotes the prior human best (rule 14)",
      after_nf$review_status[after_nf$fit_name == "alt_dipLogR_-0.20"] == "reviewed_acceptable_fit")

# The flagged scenario: human retracts, autoQC found nothing -> A back at tier 3,
# is_best_fit EMPTY.
start_nofit <- rbind(row_of("reviewed_best_fit", "alt_dipLogR_-0.20", HUMAN, "2026-09-01 10:00:00"),
                     row_of("auto_qc_no_fit",    "Not selected",      AUTOQC, "2026-09-08 01:00:00"))
d_flag <- make_sample_dir(start_nofit)
submit_review_2n(d_flag, row_of("reviewed_no_fit", "default", HUMAN, "2026-09-08 14:00:00",
                                sample_path = paste0(d_flag, "/")))
after_flag <- read_manifest_raw_2n(d_flag)
check("flagged case: prior best sits at tier 3",
      after_flag$review_status[after_flag$fit_name == "alt_dipLogR_-0.20"] == "reviewed_acceptable_fit")
check("flagged case: is_best_fit resolves to NONE",
      is.na(resolve_best_fit_2n(after_flag)))

## ---------------------------------------------------------------------------
## 4. Rule 15 -- ultra is never selectable
## ---------------------------------------------------------------------------

ultra_best <- rbind(row_of("reviewed_best_fit", "default/ultra", HUMAN, "2026-09-08 10:00:00"),
                    row_of("auto_qc_best_fit",  "default",       AUTOQC, "2026-09-08 01:00:00"))
check("rule 15: a best-fit row naming ultra never wins",
      resolve_best_fit_2n(ultra_best) == "default")

check("rule 15: is_ultra_fit_2n", all(is_ultra_fit_2n(c("default/ultra", "a/b/ultra"))) &&
        !any(is_ultra_fit_2n(c("default", "alt_dipLogR_-0.2", "refit_ultra_x"))))
check("rule 15: strip_ultra_fits_2n",
      identical(strip_ultra_fits_2n(c("default", "default/ultra", "refit_c50")),
                c("default", "refit_c50")))
check("rule 15: strip is identity on standard fit names",
      identical(strip_ultra_fits_2n(c("default", "alt_dipLogR_0.10", "refit_c50")),
                c("default", "alt_dipLogR_0.10", "refit_c50")))

## ---------------------------------------------------------------------------
## 5. Run-type choices + view resolver
## ---------------------------------------------------------------------------

runs <- data.frame(fit_name = c("default", "default/ultra", "alt_dipLogR_-0.20"),
                   purity_run_prefix = c("/p/default/S_purity", NA, "/p/alt/S_purity"),
                   hisens_run_prefix = c("/p/default/S_hisens", "/p/default/ultra/S_hisens",
                                         "/p/alt/S_hisens"),
                   stringsAsFactors = FALSE)

check("choices: standard sample -> Purity/Hisens only",
      identical(fit_type_choices_2n(FALSE, TRUE, runs, "default"), c("Purity", "Hisens")))
check("choices: 2n but unauthorized -> Purity/Hisens only",
      identical(fit_type_choices_2n(TRUE, FALSE, runs, "default"), c("Purity", "Hisens")))
check("choices: 2n + authorized + ultra sibling -> Ultra offered",
      identical(fit_type_choices_2n(TRUE, TRUE, runs, "default"),
                c("Purity", "Hisens", "Ultra")))
check("choices: 2n + authorized, fit without an ultra sibling -> no Ultra",
      identical(fit_type_choices_2n(TRUE, TRUE, runs, "alt_dipLogR_-0.20"),
                c("Purity", "Hisens")))

rv_p <- resolve_view_run_2n(runs, "default", "Purity")
rv_h <- resolve_view_run_2n(runs, "default", "Hisens")
check("resolver: Purity is the identity",
      rv_p$run$fit_name == "default" && rv_p$type == "Purity" && !rv_p$is_ultra)
check("resolver: Hisens is the identity",
      rv_h$run$fit_name == "default" && rv_h$type == "Hisens" && !rv_h$is_ultra)
rv_u <- resolve_view_run_2n(runs, "default", "Ultra")
check("resolver: Ultra returns the nested ultra row as Hisens",
      rv_u$run$fit_name == "default/ultra" && rv_u$type == "Hisens" && rv_u$is_ultra)
rv_stale <- resolve_view_run_2n(runs, "alt_dipLogR_-0.20", "Ultra")
check("resolver: stale Ultra with no sibling falls back to the fit's hisens",
      rv_stale$run$fit_name == "alt_dipLogR_-0.20" && rv_stale$type == "Hisens" &&
        !rv_stale$is_ultra)

## ---------------------------------------------------------------------------
## 6. Path canonicalization (rule 17) + pair identity
## ---------------------------------------------------------------------------

check("rule 17: doubled slashes collapsed, one trailing slash",
      canonicalize_sample_path_2n("/a//b/c") == "/a/b/c/" &&
        canonicalize_sample_path_2n("/a/b/c/") == "/a/b/c/" &&
        canonicalize_sample_path_2n("/a/b/c///") == "/a/b/c/")

# tempdir() can itself contain a doubled slash; the helpers collapse those per
# rule 17, so compare against the collapsed form.
squash <- function(p) gsub('(?<!:)//+', '/', p, perl = TRUE)
pair <- squash(tempfile("pair2n_"))
for (cls in c("clinical", "research")) {
  dir.create(file.path(pair, cls, "default"), recursive = TRUE)
  file.create(file.path(pair, cls, "default", "S.facets2n_normal_selection.txt"))
}
dir.create(file.path(pair, "research", "default", "ultra"), recursive = TRUE)

id_r <- sample_identity_2n(file.path(pair, "research"))
check("identity: 2n class subtree detected", isTRUE(id_r$is_2n) && id_r$class == "research")
check("identity: tag is the pair dir name", id_r$tag == basename(pair))
check("identity: sibling path resolves to clinical",
      id_r$sibling_path == squash(file.path(pair, "clinical")))
check("identity: default class is research", default_fit_class_2n(id_r$path_clinical,
                                                                 id_r$path_research) == "research")

std <- tempfile("std_"); dir.create(file.path(std, "default"), recursive = TRUE)
id_s <- sample_identity_2n(std)
check("identity: standard sample -> is_2n FALSE, all fields NA",
      !id_s$is_2n && is.na(id_s$class) && is.na(id_s$tag))

idx <- pair_index_2n(c(file.path(pair, "clinical"), file.path(pair, "research"), std))
check("pair index: one row per pair, standard paths excluded",
      nrow(idx) == 1 && idx$sample_id == basename(pair))
check("pair index: both class paths recorded",
      idx$path_clinical == squash(file.path(pair, "clinical")) &&
        idx$path_research == squash(file.path(pair, "research")))
check("pair index: empty input -> empty frame", nrow(pair_index_2n(character())) == 0)

pp <- pair_paths_2n(idx, basename(pair))
check("pair paths lookup", pp$research == squash(file.path(pair, "research")))

## collapse: 2n pair -> 1 row (research), standard rows untouched, columns intact
mm <- data.frame(sample_id = c("PAIR", "PAIR", "STD"),
                 path = c(file.path(pair, "clinical"), file.path(pair, "research"), std),
                 num_fits = c(1, 1, 1), stringsAsFactors = FALSE)
col <- collapse_manifest_2n(mm)
check("collapse: 2n pair collapses to one row", sum(col$metadata$sample_id == "PAIR") == 1)
check("collapse: kept row is the research subtree",
      col$metadata$path[col$metadata$sample_id == "PAIR"] == file.path(pair, "research"))
check("collapse: standard row survives", any(col$metadata$sample_id == "STD"))
check("collapse: column set unchanged", identical(names(col$metadata), names(mm)))

mm_std <- data.frame(sample_id = "STD", path = std, num_fits = 1, stringsAsFactors = FALSE)
col_std <- collapse_manifest_2n(mm_std)
check("collapse: identity on an all-standard manifest",
      identical(col_std$metadata, mm_std) && nrow(col_std$pair_index) == 0)

## ---------------------------------------------------------------------------
## 7. Pane labels
## ---------------------------------------------------------------------------

run1 <- data.frame(tumor_sample_id = "T_SYNTH_N_SYNTH", stringsAsFactors = FALSE)
check("label: standard pane label is the plain sample id",
      pane_label_2n(run1, FALSE, NA) == "T_SYNTH_N_SYNTH")
check("label: 2n pane label carries the class",
      pane_label_2n(run1, TRUE, "clinical") == "T_SYNTH_N_SYNTH [clinical]")

cat("\n", n_pass, "passed,", n_fail, "failed\n")
if (n_fail > 0) quit(status = 1)
