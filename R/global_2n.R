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
  if (is.null(sample_path) || length(sample_path) != 1 || is.na(sample_path) ||
      !nzchar(sample_path)) {
    return(FALSE)
  }
  length(Sys.glob(file.path(sample_path, 'default', '*.facets2n_normal_selection.txt'))) > 0
}

#' Is a fit name a 2n view-only ultra fit?
#'
#' Ultra fits are surfaced by metadata_init/metadata_init_2n as nested names of
#' the form "<fit>/ultra" (e.g. "default/ultra"). Rule 15: ultra is never
#' selectable by anyone -- not autoQC, not humans -- so it is excluded from
#' every selection path and every best-fit dropdown. Standard datasets contain
#' no ultra dirs, so this predicate is always FALSE for them.
#'
#' @param fit_name character vector of fit names
#' @return logical vector
#' @export is_ultra_fit_2n
is_ultra_fit_2n <- function(fit_name) {
  if (length(fit_name) == 0) return(logical(0))
  grepl('/ultra$', as.character(fit_name))
}

#' Drop ultra fits from a vector of fit names (rule 15).
#'
#' @param fit_names character vector of fit names
#' @return the fit names that are selectable
#' @export strip_ultra_fits_2n
strip_ultra_fits_2n <- function(fit_names) {
  if (length(fit_names) == 0) return(fit_names)
  fit_names[!is_ultra_fit_2n(fit_names)]
}

#' Does a fit have a sibling "<fit>/ultra" run in this sample?
#'
#' @param sample_runs the facets_runs data.frame for a sample (needs fit_name)
#' @param fit_name the fit to test
#' @return TRUE if "<fit_name>/ultra" is present in sample_runs
#' @export has_ultra_sibling_2n
has_ultra_sibling_2n <- function(sample_runs, fit_name) {
  if (is.null(sample_runs) || nrow(sample_runs) == 0) return(FALSE)
  if (is.null(fit_name) || length(fit_name) != 1 || is.na(fit_name) || !nzchar(fit_name)) {
    return(FALSE)
  }
  if (is_ultra_fit_2n(fit_name)) return(FALSE)
  paste0(fit_name, '/ultra') %in% sample_runs$fit_name
}

#' Run-type choices for the Purity/Hisens (/Ultra) selector.
#'
#' "Ultra" is offered only when ALL of: the pane holds a 2n sample, the user is
#' fully authorized, and the selected fit actually has an ultra sibling on disk.
#' In every other case the choices are exactly c('Purity', 'Hisens') -- the
#' literal the UI is built with -- so standard behavior is unchanged.
#'
#' @param is_2n whether the pane's sample is a 2n run
#' @param authorized whether the session has full authorization
#' @param sample_runs the pane's facets_runs data.frame
#' @param fit_name the currently selected fit
#' @return character vector of choices
#' @export fit_type_choices_2n
fit_type_choices_2n <- function(is_2n, authorized, sample_runs, fit_name) {
  base <- c('Purity', 'Hisens')
  if (!isTRUE(is_2n) || !isTRUE(authorized)) return(base)
  if (!has_ultra_sibling_2n(sample_runs, fit_name)) return(base)
  c(base, 'Ultra')
}

#' Resolve which run row and run type a view should render.
#'
#' For run_type 'Ultra' this returns the nested "<fit>/ultra" run row and maps
#' the type to 'Hisens' (ultra fits produce hisens files only). For every other
#' run type it returns the fit's own row and the type unchanged -- i.e. exactly
#' what the caller would have computed itself before this helper existed, which
#' is what keeps standard rendering bit-identical.
#'
#' @param sample_runs the pane's facets_runs data.frame
#' @param fit_name the selected fit
#' @param run_type 'Purity', 'Hisens' or 'Ultra'
#' @return list(run = <one-row df>, type = 'Purity'|'Hisens', is_ultra = logical)
#' @export resolve_view_run_2n
resolve_view_run_2n <- function(sample_runs, fit_name, run_type) {
  own <- sample_runs[which(sample_runs$fit_name == paste0(fit_name)), ]

  if (!identical(as.character(run_type), 'Ultra')) {
    return(list(run = own, type = run_type, is_ultra = FALSE))
  }

  ultra_name <- paste0(fit_name, '/ultra')
  ultra <- sample_runs[which(sample_runs$fit_name == ultra_name), ]

  # Defensive: a stale 'Ultra' selection against a fit with no ultra sibling
  # falls back to the fit's own hisens run rather than erroring on 0 rows.
  if (nrow(ultra) == 0) {
    return(list(run = own, type = 'Hisens', is_ultra = FALSE))
  }

  list(run = ultra, type = 'Hisens', is_ultra = TRUE)
}

#' Canonicalize a facets run path: no doubled slashes, exactly one trailing slash.
#'
#' Rule 17: every writer stamps `path` identically. metadata_init_2n previously
#' stamped `sample_path` verbatim, so manifests ended up with mixed
#' trailing-slash formatting depending on which entry path invoked it (the only
#' manifest diff between otherwise identical pipeline reruns). `path` is never a
#' join key, so this is cosmetic -- but it makes rerun diffs clean and matches
#' the 2n autoQC writer.
#'
#' @param p a path
#' @return the path with collapsed slashes and exactly one trailing slash
#' @export canonicalize_sample_path_2n
canonicalize_sample_path_2n <- function(p) {
  if (is.null(p) || length(p) != 1 || is.na(p) || !nzchar(p)) return(p)
  p <- gsub('(?<!:)//+', '/', p, perl = TRUE)
  paste0(sub('/+$', '', p), '/')
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
  # Rule 15: ultra is never selectable by anyone, so it can never be the
  # resolved best fit -- not even if a legacy manifest row names one.
  reviews <- reviews %>% filter(fit_name != 'Not selected',
                                !is_ultra_fit_2n(fit_name))

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
  # Delta 4 (vs metadata_init): rule-17 path hygiene. The canonical form (one
  # trailing slash) is what gets STAMPED into facets_qc.txt / the manifest, so
  # reruns from different entry paths no longer differ only by a trailing slash.
  # Internal path building keeps the un-slashed form, so the run prefixes this
  # function derives stay free of doubled slashes.
  sample_path       <- sub('/+$', '', gsub('(?<!:)//+', '/', sample_path, perl = TRUE))
  sample_path_stamp <- canonicalize_sample_path_2n(sample_path)

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
                           data.frame(tumor_sample_id = sample_id, path = sample_path_stamp, fit_name = fit_name,
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
    mutate(path = sample_path_stamp,
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

### ---------------------------------------------------------------------------
### 2n sample identity: pair / class subtree resolution.
###
### After SPLIT_FACETS_2N, one 2n pair is TWO standard-shaped sample dirs:
###     all/<patient>/<pair>/clinical
###     all/<patient>/<pair>/research
### The app presents the pair as ONE sample and swaps between the two class
### subtrees. Ultra fits exist only under research/<fit>/ultra.
### ---------------------------------------------------------------------------

#' The two 2n fit classes, in display order.
#' @export fit_classes_2n
fit_classes_2n <- function() c('clinical', 'research')

#' Resolve the pair/class identity of a facets run directory.
#'
#' @param sample_path a facets run directory
#' @return list(is_2n, class, pair_dir, tag, sibling_path, path_clinical, path_research)
#'   For a standard sample: is_2n = FALSE and every 2n field is NA.
#' @export sample_identity_2n
sample_identity_2n <- function(sample_path) {
  none <- list(is_2n = FALSE, class = NA_character_, pair_dir = NA_character_,
               tag = NA_character_, sibling_path = NA_character_,
               path_clinical = NA_character_, path_research = NA_character_)

  if (is.null(sample_path) || length(sample_path) != 1 || is.na(sample_path) ||
      !nzchar(sample_path)) {
    return(none)
  }
  if (!is_facets2n_sample(sample_path)) return(none)

  clean    <- sub('/+$', '', gsub('(?<!:)//+', '/', sample_path, perl = TRUE))
  cls      <- basename(clean)
  pair_dir <- dirname(clean)

  # A 2n run whose dir is not named clinical/research (e.g. a flat pre-split
  # tree, or a hand-copied dir): still 2n, but there is no class to swap.
  if (!(cls %in% fit_classes_2n())) {
    return(list(is_2n = TRUE, class = NA_character_, pair_dir = NA_character_,
                tag = basename(clean), sibling_path = NA_character_,
                path_clinical = NA_character_, path_research = NA_character_))
  }

  path_for <- function(k) {
    p <- file.path(pair_dir, k)
    if (dir.exists(p)) p else NA_character_
  }
  pc <- path_for('clinical')
  pr <- path_for('research')

  list(is_2n         = TRUE,
       class         = cls,
       pair_dir      = pair_dir,
       tag           = basename(pair_dir),
       sibling_path  = if (cls == 'clinical') pr else pc,
       path_clinical = pc,
       path_research = pr)
}

#' Which class should be shown first for a pair.
#'
#' Research is preferred (it carries the ultra fits); clinical is used when
#' research is absent.
#'
#' @param path_clinical,path_research class subtree paths (NA when absent)
#' @return 'research', 'clinical', or NA when neither exists
#' @export default_fit_class_2n
default_fit_class_2n <- function(path_clinical, path_research) {
  if (!is.na(path_research)) return('research')
  if (!is.na(path_clinical)) return('clinical')
  NA_character_
}

#' Build the pair index for a set of facets run paths.
#'
#' One row per 2n pair; standard paths contribute nothing. This is a SIDE TABLE
#' -- it is deliberately not merged into values$manifest_metadata, whose column
#' set is positional in the samples table and in the mapping-file export.
#'
#' @param paths character vector of facets run directories
#' @return data.frame(sample_id, pair_dir, path_clinical, path_research, default_class)
#' @export pair_index_2n
pair_index_2n <- function(paths) {
  empty <- data.frame(sample_id = character(), pair_dir = character(),
                      path_clinical = character(), path_research = character(),
                      default_class = character(), stringsAsFactors = FALSE)
  if (length(paths) == 0) return(empty)

  rows <- lapply(paths, function(p) {
    id <- sample_identity_2n(p)
    if (!isTRUE(id$is_2n) || is.na(id$pair_dir)) return(NULL)
    data.frame(sample_id     = id$tag,
               pair_dir      = id$pair_dir,
               path_clinical = id$path_clinical,
               path_research = id$path_research,
               default_class = default_fit_class_2n(id$path_clinical, id$path_research),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(empty)

  out <- do.call(rbind, rows)
  out[!duplicated(out$sample_id), , drop = FALSE]
}

#' Look up a pair's class subtree paths in the pair index.
#'
#' @param pair_index a pair_index_2n data.frame
#' @param sample_id the pair tag
#' @return named list(clinical=, research=), both possibly NA; NULL if not found
#' @export pair_paths_2n
pair_paths_2n <- function(pair_index, sample_id) {
  if (is.null(pair_index) || nrow(pair_index) == 0) return(NULL)
  if (is.null(sample_id) || length(sample_id) != 1 || is.na(sample_id)) return(NULL)
  hit <- pair_index[pair_index$sample_id == sample_id, , drop = FALSE]
  if (nrow(hit) == 0) return(NULL)
  list(clinical = hit$path_clinical[1], research = hit$path_research[1])
}

#' Collapse a repo-manifest metadata frame so each 2n pair is one row.
#'
#' The 2n cohort manifest carries one row per (pair, class) with an identical
#' `tag`, which would otherwise give two manifest rows sharing a sample_id --
#' and every `path[sample_id %in% input]` lookup in the app would paste the two
#' class paths together. The default-class row is kept and the discarded row's
#' path is preserved in the returned pair index.
#'
#' The returned metadata has EXACTLY the input's columns (the samples table and
#' the mapping-file export both depend on that), and a frame with no 2n paths is
#' returned unchanged.
#'
#' @param metadata output of load_repo_samples() / load_samples()
#' @return list(metadata = <collapsed>, pair_index = <pair_index_2n>)
#' @export collapse_manifest_2n
collapse_manifest_2n <- function(metadata) {
  if (is.null(metadata) || nrow(metadata) == 0 || !('path' %in% names(metadata))) {
    return(list(metadata = metadata, pair_index = pair_index_2n(character())))
  }

  idx <- pair_index_2n(metadata$path)
  if (nrow(idx) == 0) {
    return(list(metadata = metadata, pair_index = idx))
  }

  # Keep the row whose path is the pair's default class; drop the sibling row.
  keep <- vapply(seq_len(nrow(metadata)), function(i) {
    id <- sample_identity_2n(metadata$path[i])
    if (!isTRUE(id$is_2n) || is.na(id$class)) return(TRUE)
    dflt <- default_fit_class_2n(id$path_clinical, id$path_research)
    is.na(dflt) || identical(id$class, dflt)
  }, logical(1))

  list(metadata = metadata[keep, , drop = FALSE], pair_index = idx)
}

#' Display label for a pane: the pair tag plus its class, for 2n samples.
#'
#' Used only where two panes can show the same pair in different classes; for
#' standard samples this returns tumor_sample_id unchanged.
#'
#' @param selected_run one-row run data.frame
#' @param is_2n whether the pane holds a 2n sample
#' @param fit_class the pane's class, or NA
#' @return character label
#' @export pane_label_2n
pane_label_2n <- function(selected_run, is_2n, fit_class) {
  base <- as.character(selected_run$tumor_sample_id[1])
  if (!isTRUE(is_2n) || is.null(fit_class) || length(fit_class) != 1 || is.na(fit_class)) {
    return(base)
  }
  paste0(base, ' [', fit_class, ']')
}

### ---------------------------------------------------------------------------
### 2n gene/arm-level tables.
###
### Forks of get_geneLevel_table / get_armLevel_table (global.R). Deltas:
###   1. The run prefix falls back to the hisens prefix when there is no purity
###      run. Ultra fits are hisens-only, so the originals -- which always read
###      purity_run_prefix -- build "NA.gene_level.txt" and throw.
###   2. A missing file returns an empty table instead of throwing, so the caller
###      can show "not produced for this run" rather than a blank tab.
### ---------------------------------------------------------------------------

# Base name shared by a run's per-fit annotation files, from whichever prefix exists.
run_annotation_prefix_2n <- function(selected_run) {
  p <- selected_run$purity_run_prefix[1]
  if (!is.na(p) && nzchar(p)) return(sub('_purity$', '', p))
  h <- selected_run$hisens_run_prefix[1]
  if (!is.na(h) && nzchar(h)) return(sub('_hisens$', '', h))
  NA_character_
}

#' 2n variant of get_geneLevel_table.
#' @param fit_type retained for signature parity with the original (unused)
#' @param selected_run one-row run data.frame
#' @return gene-level table, or an empty data.table when unavailable
#' @export get_geneLevel_table_2n
get_geneLevel_table_2n <- function(fit_type, selected_run) {
  run_prefix <- run_annotation_prefix_2n(selected_run)
  if (is.na(run_prefix)) return(data.table())

  geneLevel_filename <- paste0(run_prefix, ".gene_level.txt")
  if (!file.exists(geneLevel_filename)) return(data.table())

  geneLevel_data <- data.table::fread(geneLevel_filename)

  if ( !("gene" %in% names(geneLevel_data)) & !("chrom" %in% names(geneLevel_data)) ) {
    return(data.table())
  }

  if (!("cf" %in% names(geneLevel_data))) {
    geneLevel_data[, cf := cf.em]
    geneLevel_data[, tcn := tcn.em]
    geneLevel_data[, lcn := lcn.em]
  }

  geneLevel_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cf = round_down(cf),
                  cf.em = round_down(cf.em)) %>%
    dplyr::select(-median_cnlr_seg, -segclust)
}

#' 2n variant of get_armLevel_table.
#' @param fit_type retained for signature parity with the original (unused)
#' @param selected_run one-row run data.frame
#' @return arm-level table, or an empty data.table when unavailable
#' @export get_armLevel_table_2n
get_armLevel_table_2n <- function(fit_type, selected_run) {
  run_prefix <- run_annotation_prefix_2n(selected_run)
  if (is.na(run_prefix)) return(data.table())

  armLevel_filename <- paste0(run_prefix, ".arm_level.txt")
  if (!file.exists(armLevel_filename)) return(data.table())

  armLevel_data <- data.table::fread(armLevel_filename)

  if ( !("arm" %in% names(armLevel_data)) & !("arm_length" %in% names(armLevel_data)) ) {
    return(data.table())
  }

  armLevel_data %>% dplyr::rowwise()
}

### ---------------------------------------------------------------------------
### 2n manifest writes: atomic, with in-place demotion of a superseded best fit.
### ---------------------------------------------------------------------------

# The frozen 13-column manifest schema, in order. No column may be added or
# reordered -- both writers and every reader depend on this exact shape.
manifest_columns_2n <- function() {
  c('sample', 'path', 'review_status', 'fit_name', 'review_notes', 'reviewed_by',
    'date_reviewed', 'facets_qc', 'use_only_purity_run', 'use_edited_cncf',
    'reviewer_set_purity', 'facets_qc_version', 'facets_suite_version')
}

# The header line every manifest carries.
manifest_header_2n <- function() '# generated by facets-preview app. DO NOT EDIT.'

#' Read a manifest verbatim, every column as character.
#'
#' Unlike get_review_status this applies NO type coercion and NO backwards-
#' compatibility rewriting, so rows written by the 2n autoQC (or by a human)
#' round-trip byte-identically through a rewrite -- in particular date_reviewed
#' is never reformatted via POSIXct. The comment line is skipped only when it is
#' actually present, so a manifest written without one still loads.
#'
#' @param sample_path facets run directory containing 'facets_review.manifest'
#' @return data.frame of character columns (0 rows if the file has no data rows)
#' @export read_manifest_raw_2n
read_manifest_raw_2n <- function(sample_path) {
  empty <- as.data.frame(setNames(replicate(length(manifest_columns_2n()),
                                            character(), simplify = FALSE),
                                  manifest_columns_2n()),
                         stringsAsFactors = FALSE)

  review_file <- file.path(sample_path, 'facets_review.manifest')
  if (!file.exists(review_file) || file.size(review_file) == 0) return(empty)

  first_line <- tryCatch(readLines(review_file, n = 1, warn = FALSE),
                         error = function(e) character(0))
  if (length(first_line) == 0) return(empty)
  skip_n <- if (grepl('^\\s*#', first_line[1])) 1 else 0

  # Header present but no data rows is a valid, empty manifest -- not a missing
  # one. (get_review_status's countLines(.) < 2 test calls the comment-less
  # header-only form "missing"; here it reads as zero reviews.)
  n_lines <- length(readLines(review_file, warn = FALSE))
  if (n_lines <= skip_n + 1) return(empty)

  df <- suppressWarnings(data.table::fread(review_file, skip = skip_n,
                                           colClasses = 'character',
                                           verbose = FALSE, data.table = FALSE))
  if (nrow(df) == 0) return(empty)
  df
}

#' Atomic 2n manifest write (rule 21).
#'
#' Fork of update_review_status_file (global.R). Deltas:
#'   1. ALWAYS a full-table overwrite -- the 2n path never appends, because
#'      in-place demotion means every submission rewrites existing rows.
#'   2. Written to a temp file in the same directory and then renamed, so a
#'      concurrent reader sees either the old or the new manifest, never a
#'      half-written one.
#'   3. The frozen 13 columns are selected and ordered explicitly.
#'
#' @param sample_path facets run directory
#' @param df the complete manifest to write
#' @return TRUE on success, FALSE if not written
#' @export update_review_status_file_2n
update_review_status_file_2n <- function(sample_path, df) {
  if (is.null(df) || nrow(df) == 0) return(FALSE)

  if (!verify_access_to_write(sample_path)) {
    warning('You do not have write permissions to update review status file')
    return(FALSE)
  }

  cols <- manifest_columns_2n()
  missing_cols <- setdiff(cols, names(df))
  for (m in missing_cols) df[[m]] <- NA
  df <- df[, cols, drop = FALSE] %>% unique

  review_file <- file.path(sample_path, 'facets_review.manifest')
  tmp_file    <- file.path(sample_path,
                           paste0('.facets_review.manifest.tmp', Sys.getpid()))

  ok <- tryCatch({
    con <- file(tmp_file, open = 'wt')
    # close() invalidates the connection object, so the cleanup handler must not
    # inspect it -- it only runs on an error path, where con is still open.
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    writeLines(manifest_header_2n(), con)
    suppressWarnings(write.table(df, con, sep = '\t', row.names = FALSE,
                                 quote = FALSE))
    close(con)
    on.exit()
    file.rename(tmp_file, review_file)
  }, error = function(e) {
    warning(paste0('Failed to write ', review_file, ': ', conditionMessage(e)))
    FALSE
  })

  if (!isTRUE(ok) && file.exists(tmp_file)) unlink(tmp_file)
  isTRUE(ok)
}

#' Apply rules 10/14: demote a superseded human best fit, in place.
#'
#' When a new reviewed_best_fit or reviewed_no_fit arrives, any existing
#' HUMAN-authored reviewed_best_fit row is rewritten to reviewed_acceptable_fit.
#' ONLY review_status changes: reviewed_by and date_reviewed stay with the
#' original reviewer even when someone else triggers the demotion, and no note
#' is added. autoQC rows are never touched.
#'
#' @param manifest the current manifest (character columns)
#' @param new_status the review_status being submitted
#' @return the manifest with demotions applied
#' @export demote_prior_best_fit_2n
demote_prior_best_fit_2n <- function(manifest, new_status) {
  if (is.null(manifest) || nrow(manifest) == 0) return(manifest)
  if (!(new_status %in% c('reviewed_best_fit', 'reviewed_no_fit'))) return(manifest)

  is_prior_human_best <-
    !is.na(manifest$review_status) &
    manifest$review_status == 'reviewed_best_fit' &
    (is.na(manifest$reviewed_by) | manifest$reviewed_by != autoqc_reviewer_id_2n())

  manifest$review_status[is_prior_human_best] <- 'reviewed_acceptable_fit'
  manifest
}

#' Submit one human review against a 2n sample (rules 10/14/15/21, B6).
#'
#' Reads the manifest verbatim, demotes a superseded human best fit in place,
#' drops the now-redundant not_reviewed placeholder for the reviewed fit,
#' appends the new row, and writes the whole table atomically.
#'
#' @param sample_path facets run directory
#' @param new_row one-row data.frame carrying the 13 manifest columns
#' @return TRUE on success
#' @export submit_review_2n
submit_review_2n <- function(sample_path, new_row) {
  manifest <- read_manifest_raw_2n(sample_path)

  new_row <- as.data.frame(lapply(new_row, as.character), stringsAsFactors = FALSE)

  # B6: a "no fit available" verdict is about the whole sample, not about
  # whichever fit the (hidden) best-fit selector happened to be left on.
  if (identical(new_row$review_status[1], 'reviewed_no_fit')) {
    new_row$fit_name <- 'Not selected'
  }

  manifest <- demote_prior_best_fit_2n(manifest, new_row$review_status[1])

  # Rule 8: the not_reviewed placeholder for this fit is redundant once a real
  # verdict exists for it.
  if (nrow(manifest) > 0) {
    redundant <- !is.na(manifest$review_status) &
      manifest$review_status == 'not_reviewed' &
      !is.na(manifest$fit_name) &
      manifest$fit_name == new_row$fit_name[1]
    manifest <- manifest[!redundant, , drop = FALSE]
  }

  cols <- manifest_columns_2n()
  for (m in setdiff(cols, names(new_row))) new_row[[m]] <- NA_character_
  combined <- rbind(manifest[, cols, drop = FALSE], new_row[, cols, drop = FALSE])

  update_review_status_file_2n(sample_path, combined)
}

### ---------------------------------------------------------------------------
### 2n sample-list loading.
###
### Forks of load_reviews / metadata_init_quick / load_samples (global.R).
### The self-heal path is the reason these exist: load_reviews() rebuilds a
### missing manifest via metadata_init(), which would (a) use the standard
### ruleset and (b) -- when a 2n class subtree is loaded from a path list --
### stamp sample_id = "clinical"/"research" into the manifest, because
### load_samples() derives the id from basename(path).
### ---------------------------------------------------------------------------

#' 2n variant of load_reviews: self-heals through metadata_init_2n.
#'
#' @param sample_id sample id (the pair tag for 2n)
#' @param sample_path facets run directory
#' @return the reviews data.frame
#' @export load_reviews_2n
load_reviews_2n <- function(sample_id, sample_path) {
  reviews <- get_review_status(sample_id, sample_path)

  if (nrow(reviews) == 0 || !('facets_qc' %in% names(reviews)) ||
      length(which(is.na(reviews$facets_qc))) > 0) {
    metadata_init_2n(sample_id, sample_path)
    return(get_review_status(sample_id, sample_path))
  }
  reviews
}

#' 2n variant of metadata_init_quick.
#'
#' Identical to metadata_init_quick except that the manifest self-heal goes
#' through load_reviews_2n (hence metadata_init_2n).
#'
#' @param sample_id sample id (the pair tag)
#' @param sample_path facets run directory
#' @return minimal description of the facets run
#' @export metadata_init_quick_2n
metadata_init_quick_2n <- function(sample_id, sample_path) {
  run_dir_exists = "No"
  if ( dir.exists(sample_path)) {
    run_dir_exists = "Yes"
  }

  num_fits = ''
  default_fit_name = ''
  default_fit_qc = ''
  review_status = 'Not reviewed'
  reviewed_fit_name = ''
  reviewed_fit_facets_qc = F
  reviewed_fit_use_purity = F
  reviewed_fit_use_edited_cncf = F
  reviewer_set_purity = NA
  reviewed_date = NA
  facets_qc_version = 'unknown'
  facets_suite_version = 'unknown'

  reviews <- load_reviews_2n(sample_id, sample_path)

  if ( nrow(reviews) > 0 ){
    num_fits = nrow(reviews %>% filter(!grepl('Not selected', fit_name)) %>%
                      select(fit_name) %>% unique)

    default_fit_name = 'default'
    if (!(any(default_fit_name %in% reviews$fit_name))) {
      default_fit_name =
        ((reviews %>%
            filter(!grepl('^facets_refit|^refit_|^alt_diplogR|Not sel', fit_name,
                          ignore.case = T)))$fit_name %>%
           unique)[1]

      if (is.na(default_fit_name)) {
        default_fit_name = reviews$fit_name[1]
      }
    }

    if (any(default_fit_name %in% reviews$fit_name)) {
      default_fit_qc = (reviews %>% filter(fit_name == default_fit_name) %>%
                          arrange(desc(date_reviewed)))$facets_qc[1]
    }

    reviews = (reviews %>% filter(review_status != 'not_reviewed') %>%
                 arrange(desc(date_reviewed)))

    if (nrow(reviews) > 0) {
      review_status = reviews$review_status[1]
      reviewed_fit_name = reviews$fit_name[1]
      reviewed_fit_facets_qc = as.logical(reviews$facets_qc[1])
      reviewed_fit_use_purity = as.logical(reviews$use_only_purity[1])
      reviewed_fit_use_edited_cncf = as.logical(reviews$use_edited_cncf[1])
      reviewer_set_purity = reviews$reviewer_set_purity[1]
      reviewed_date = reviews$date_reviewed[1]
      facets_qc_version = reviews$facets_qc_version[1]
      facets_suite_version = reviews$facets_suite_version[1]
    }
  }

  return (list('sample_id' = sample_id,
               'path' = sample_path,
               'num_fits' = num_fits,
               'default_fit_name' = default_fit_name,
               'default_fit_qc' = default_fit_qc,
               'review_status' = review_status,
               'reviewed_fit_name' = reviewed_fit_name,
               'reviewed_fit_facets_qc' = reviewed_fit_facets_qc,
               'reviewed_fit_use_purity' = reviewed_fit_use_purity,
               'reviewed_fit_use_edited_cncf' = reviewed_fit_use_edited_cncf,
               'reviewer_set_purity' = reviewer_set_purity,
               'reviewed_date' = reviewed_date,
               'facets_qc_version' = facets_qc_version,
               'facets_suite_version' = facets_suite_version))
}

#' 2n variant of load_samples: one row per PAIR, keyed by the pair tag.
#'
#' Deltas vs load_samples (global.R):
#'   1. sample_id is the pair tag (basename of the pair dir), not the class
#'      subtree's basename ("clinical"/"research").
#'   2. Both class subtrees of a pair collapse to one row, loaded from the
#'      default class (research when present).
#'   3. Quiet: none of the per-sample print() diagnostics.
#'
#' @param manifest character vector of facets run directories (2n class subtrees)
#' @param progress progress bar from shiny
#' @return metadata data.frame with the same columns as load_samples()
#' @export load_samples_2n
load_samples_2n <- function(manifest, progress = NA) {
  metadata <- data.frame()
  if (length(manifest) == 0) return(metadata)

  idx <- pair_index_2n(manifest)

  # Pairs get one entry (default class); any 2n dir that is not a class subtree
  # is loaded on its own path.
  entries <- list()
  if (nrow(idx) > 0) {
    for (i in seq_len(nrow(idx))) {
      cls  <- idx$default_class[i]
      if (is.na(cls)) next
      path <- if (cls == 'research') idx$path_research[i] else idx$path_clinical[i]
      entries[[length(entries) + 1]] <- list(id = idx$sample_id[i], path = path)
    }
  }
  for (p in manifest) {
    id <- sample_identity_2n(p)
    if (isTRUE(id$is_2n) && is.na(id$class)) {
      entries[[length(entries) + 1]] <- list(id = id$tag, path = p)
    }
  }

  if (length(entries) == 0) return(metadata)

  for (i in seq_along(entries)) {
    metadata <- rbind(metadata,
                      as.data.frame.list(
                        metadata_init_quick_2n(entries[[i]]$id, entries[[i]]$path),
                        stringsAsFactors = FALSE))
    if (!is.null(progress) && !identical(progress, NA)) {
      progress$inc(1/length(entries), detail = paste(" ", i, "/", length(entries)))
    }
  }
  metadata
}
