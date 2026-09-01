### 2n (facets2n) variants of shared metadata functions.
###
### These are deliberate FORKS: per the CADENCE 2n autoQC design, no shared
### function used by the standard FACETS tree may change behavior. Each fork
### below names its source function and the exact deltas. If you fix a bug in
### a source function, decide explicitly whether the fix belongs here too.
###
### The 2n autoQC identity: rows written by the 2n autoQC container carry
### reviewed_by == 'auto-qc'. Writer identification everywhere is an exact
### match on reviewed_by -- never on review_status prefixes.

#' The reserved reviewed_by identity of the 2n autoQC writer.
#' @export autoqc_reviewer_id_2n
autoqc_reviewer_id_2n <- function() {
  'auto-qc'
}

#' Detect whether a sample directory is a facets2n (2n) run.
#'
#' A sample (class subtree) is 2n iff a facets2n normal-selection file exists
#' under its default fit: <sample_path>/default/*.facets2n_normal_selection.txt.
#' Standard runs never contain this file, so the probe is a no-op gate for them.
#'
#' @param sample_path facets run directory (a 2n class subtree, e.g. .../<pair>/clinical)
#' @return TRUE if the sample is a 2n run
#' @export is_facets2n_sample
is_facets2n_sample <- function(sample_path) {
  length(Sys.glob(file.path(sample_path, 'default', '*.facets2n_normal_selection.txt'))) > 0
}

# Normalize a facets_qc_version string for comparison. Standard tooling re-reads
# facets_qc.txt without colClasses, degrading "1.0" to "1"; treating those as
# unequal makes metadata_init retain stale-version review rows as duplicates.
# Numeric-like values compare numerically ("1" == "1.0"); everything else
# compares as the literal string (e.g. "unknown").
normalize_qc_version_2n <- function(v) {
  v <- as.character(v)
  n <- suppressWarnings(as.numeric(v))
  ifelse(is.na(n), v, as.character(n))
}

# Rule-13 best-fit resolution ("is_best_fit"): the human's reviewed_best_fit if
# one exists, else the 2n autoQC's auto_qc_best_fit, else NA. Classification is
# by reviewed_by (exact match on the autoQC identity), never by recency.
# Both writers of is_best_fit (this package and the 2n autoQC script) must
# compute this identically.
resolve_best_fit_2n <- function(reviews) {
  reviews <- reviews %>% filter(fit_name != 'Not selected')

  human_best <- reviews %>%
    filter(review_status == 'reviewed_best_fit',
           !is.na(reviewed_by),
           reviewed_by != autoqc_reviewer_id_2n()) %>%
    arrange(desc(date_reviewed))
  if (nrow(human_best) > 0) {
    return(human_best$fit_name[1])
  }

  autoqc_best <- reviews %>%
    filter(review_status == 'auto_qc_best_fit',
           !is.na(reviewed_by),
           reviewed_by == autoqc_reviewer_id_2n())
  if (nrow(autoqc_best) > 0) {
    return(autoqc_best$fit_name[1])
  }

  NA_character_
}

#' 2n variant of update_best_fit_status.
#'
#' Fork of update_best_fit_status (global.R). Deltas:
#'   1. facets_qc.txt is read with colClasses forcing facets_qc_version /
#'      facets_suite_version to character, so "1.0" is not degraded to "1" on
#'      the rewrite (the corruption that makes metadata_init accumulate
#'      duplicate review rows).
#'   2. best_fit follows rule 13 (human reviewed_best_fit, else the 2n autoQC's
#'      auto_qc_best_fit, else none) instead of most-recent-of
#'      {reviewed_best_fit, reviewed_acceptable_fit}.
#'
#' @param sample_id sampleid
#' @param sample_path facets run directory containing 'facets_review.manifest'
#' @export update_best_fit_status_2n
update_best_fit_status_2n <- function(sample_id, sample_path) {

  qc_file <- list.files(sample_path, pattern = "facets_qc\\.txt$", full.names = TRUE)

  if (length(qc_file) == 0) {
    stop(paste0("No facets_qc.txt file found in ", sample_path))
  }

  reviews <-
    get_review_status(sample_id, sample_path) %>%
    filter(!(fit_name == 'Not selected'))

  best_fit <- resolve_best_fit_2n(reviews)

  facets_runs <- fread(qc_file[1],
                       colClasses = list(character = c("facets_qc_version",
                                                       "facets_suite_version")))

  facets_runs$is_best_fit <- FALSE
  if (!is.na(best_fit)) {
    facets_runs$is_best_fit[which(facets_runs$fit_name == best_fit)] <- TRUE
  }

  if (verify_access_to_write(sample_path)) {
    write.table(facets_runs %>% select(-ends_with("_filter_note")),
                file = qc_file[1], quote = FALSE, row.names = FALSE, sep = "\t")
  } else {
    warning(paste0("You do not have write permissions to update ", qc_file[1]))
  }
}

#' 2n variant of metadata_init.
#'
#' Fork of metadata_init (global.R). Deltas:
#'   1. The stale-version review-retention filter compares facets_qc_version
#'      through normalize_qc_version_2n(), so rows whose "1.0" was previously
#'      degraded to "1" are recognized as current-version and are NOT retained
#'      as duplicates alongside the freshly rebuilt rows.
#'   2. is_best_fit follows rule 13 via resolve_best_fit_2n() instead of
#'      most-recent-of {reviewed_best_fit, reviewed_acceptable_fit}.
#'   3. Calls update_best_fit_status_2n() (colClasses-protected, rule-13)
#'      instead of update_best_fit_status().
#' Everything else -- fit-dir enumeration (incl. nested <fit>/ultra), .out
#' parsing, QC invocation, facets_qc.txt rebuild, manifest rebuild -- is
#' identical to metadata_init.
#'
#' @param sample_id sampleid
#' @param sample_path facets run directory
#' @param progress progress bar from shiny
#' @param update_qc_file if TRUE, rewrite facets_qc.txt and the review manifest
#' @return description of the facets run
#' @export metadata_init_2n
metadata_init_2n <- function(sample_id, sample_path, progress = NULL, update_qc_file = TRUE) {
  facets_runs <- get_new_facets_runs_df()
  facets_run_dirs = list.dirs(sample_path, full.names=FALSE, recursive=FALSE)

  ## identify different fits generated for this sample.
  facets_run_dirs <- facets_run_dirs[grep("^facets|^default$|^refit_|^alt_diplogR", facets_run_dirs, ignore.case = T)]

  # 2n view-only ultra fits: surface any <fit>/ultra subdir as a nested fit named
  # "<fit>/ultra" (e.g. "default/ultra"). All downstream path construction is
  # paste0(path, '/', fit_name, '/', sample), so the nested dir resolves naturally.
  ultra_run_dirs <- unlist(lapply(facets_run_dirs, function(d)
    if (dir.exists(file.path(sample_path, d, 'ultra'))) file.path(d, 'ultra') else NULL))
  facets_run_dirs <- c(facets_run_dirs, ultra_run_dirs)

  ### for each run directory, load metadata.
  for(fi in 1:length(facets_run_dirs)) {
    if (!is.null(progress)) {
      progress$inc(1/length(facets_run_dirs), detail = paste(" ", fi, "/", length(facets_run_dirs)))
    }
    fit_name = facets_run_dirs[fi]
    facets_run = paste(sample_path, "/", fit_name, sep="")
    facets_run_files = list.files(facets_run, pattern=".out$")

    if ( length(facets_run_files) == 0 ) { next }

    rm(list = ls()[grep("purity_", ls())]) # remove all previous facets_params
    rm(list = ls()[grep("hisens_", ls())])
    for( fif in 1:length(facets_run_files) ) {

      facets_out_params = readLines(paste0(facets_run, "/", facets_run_files[fif]))
      run_type = "hisens_"
      if (grepl("_purity", facets_out_params[2])) {
        run_type = "purity_"
      }

      run_prefix = ""
      for ( p_idx in 1:length(facets_out_params)) {
        line = gsub(" |#", "", facets_out_params[p_idx])
        sp = unlist(strsplit(line, "="))
        if ( length(sp) == 2) {
          if (sp[1] == "TAG"){
            run_prefix = paste0(facets_run, "/", sp[2])
            assign(paste0(run_type, "prefix"), run_prefix )
          }
          assign(paste0(run_type, sp[1]), sp[2])
        }
      }

      ### for purity runs, run QC. (here we are checking for "not hisens" because some
      ### runs may not have _purity or _hisens suffix)
      rdata_file = paste0(run_prefix, ".Rdata")
      if ( (length(facets_run_files) == 1) || (!grepl('hisens', run_type) & file.exists(rdata_file))) {
        facets_output = facetsSuite::load_facets_output(rdata_file)

        if (!is.null(facets_output$alBalLogR)) {
          assign(paste0(run_type, "alBalLogR"),
                 paste(round(facets_output$alBalLogR[,1],digits = 2),
                       collapse=", "))
        }
        fit_qc = facets_fit_qc(facets_output)
      }
    }

    facets_runs <- rbind(facets_runs,
                         cbind(
                           data.frame(tumor_sample_id = sample_id, path = sample_path, fit_name = fit_name,
                                      purity_run_version = get0("purity_Facetsversion", ifnotfound = NA),
                                      purity_run_prefix = get0("purity_prefix", ifnotfound = NA),
                                      purity_run_Seed = get0("purity_Seed", ifnotfound = NA),
                                      purity_run_cval = get0("hisens_purity_cval", ifnotfound = NA),
                                      purity_run_nhet = get0("purity_min.nhet", ifnotfound = NA),
                                      purity_run_snp_nbhd = get0("purity_snp.nbhd", ifnotfound = NA),
                                      purity_run_ndepth = get0("purity_ndepth", ifnotfound = NA),
                                      purity_run_Purity = round_down(get0("purity_Purity", ifnotfound = NA)),
                                      purity_run_Ploidy = round_down(get0("purity_Ploidy", ifnotfound = NA)),
                                      purity_run_dipLogR = round_down(get0("purity_dipLogR", ifnotfound = NA)),
                                      purity_run_alBalLogR = get0("purity_alBalLogR", ifnotfound = NA),

                                      hisens_run_version = get0("hisens_Facetsversion", ifnotfound = NA),
                                      hisens_run_prefix = get0("hisens_prefix", ifnotfound = NA),
                                      hisens_run_Seed = get0("hisens_Seed", ifnotfound = NA),
                                      hisens_run_cval = get0("hisens_cval", ifnotfound = NA),
                                      hisens_run_nhet = get0("hisens_min.nhet", ifnotfound = NA),
                                      hisens_run_snp_nbhd = get0("hisens_snp.nbhd", ifnotfound = NA),
                                      hisens_run_ndepth = get0("hisens_ndepth", ifnotfound = NA),
                                      hisens_run_hisens = round_down(get0("hisens_hisens", ifnotfound = NA)),
                                      hisens_run_Purity = round_down(get0("hisens_Purity", ifnotfound = NA)),
                                      hisens_run_Ploidy = round_down(get0("hisens_Ploidy", ifnotfound = NA)),
                                      hisens_run_dipLogR = round_down(get0("hisens_dipLogR", ifnotfound = NA)),

                                      manual_note = NA,
                                      is_best_fit = NA,
                                      stringsAsFactors=FALSE),
                           fit_qc))
  }

  if (nrow(facets_runs) == 0) {
    return(NULL)
  }

  # load reviews from the manifest file and annotate each review with the facets QC status.
  existing_reviews <- get_review_status(sample_id, sample_path)

  fit_qc <-
    facets_runs %>%
    mutate(facets_qc_version = as.character(facets_qc_version),
           facets_suite_version = as.character(facets_suite_version)) %>%
    select(sample = tumor_sample_id, fit_name,
           facets_suite_version, facets_qc_version, facets_qc)

  reviews <-
    rbind(fit_qc, fit_qc %>% mutate(fit_name = "Not selected", facets_qc = F) %>% unique) %>%
    left_join(existing_reviews %>%
                filter(!is.na(date_reviewed)) %>%
                select(-facets_qc, -facets_suite_version)) %>%
    mutate(path = sample_path,
           review_status = ifelse(is.na(review_status), 'not_reviewed', review_status),
           ) %>%
    select(sample, path, review_status, fit_name, review_notes, reviewed_by,
           date_reviewed, facets_qc, use_only_purity_run, use_edited_cncf, reviewer_set_purity,
           facets_qc_version, facets_suite_version)

  # Delta 1 (vs metadata_init): normalized version comparison, so "1" == "1.0"
  # and previously-degraded rows are not retained as duplicates.
  reviews <-
    rbind(existing_reviews %>%
            filter(!(fit_name == 'Not selected' |
                       normalize_qc_version_2n(facets_qc_version) ==
                         normalize_qc_version_2n(facets_qc_version()))),
          reviews)

  # Delta 2 (vs metadata_init): rule-13 best fit.
  best_fit = resolve_best_fit_2n(reviews)

  facets_runs$is_best_fit = F
  if (!is.na(best_fit)) {
    facets_runs$is_best_fit[which(facets_runs$fit_name == best_fit)] = T
  }

  if (update_qc_file) {
    # Find any file that ends with "facets_qc.txt"
    qc_file <- list.files(sample_path, pattern = "facets_qc\\.txt$", full.names = TRUE)

    if (length(qc_file) == 0) {
      qc_file <- file.path(sample_path, "facets_qc.txt")
    } else {
      qc_file <- qc_file[1]
    }

    if (verify_access_to_write(sample_path)) {
      write.table(facets_runs %>% select(-ends_with("_filter_note")),
                  file = qc_file, quote = FALSE, row.names = FALSE, sep = "\t")
    } else {
      warning(paste0('You do not have write permissions to update ', qc_file))
    }
  }

  if (update_qc_file) {
    update_review_status_file(sample_path, reviews, T)

    # Delta 3 (vs metadata_init): colClasses-protected, rule-13 variant.
    update_best_fit_status_2n(sample_id, sample_path)
  }
  facets_runs
}
