#' Compile cohort annotations for facets2n (2n) runs, under a review policy
#'
#' Fork of compile_cohort_annotations() for the CADENCE 2n cohort. The original
#' stays untouched for standard runs. Two cohort flavors are compiled side by
#' side by calling this once per policy:
#'
#' \itemize{
#'   \item{\code{policy = 'autoqc'}:} {the fit named by the 2n autoQC's
#'     auto_qc_best_fit row; human reviews ignored entirely. auto_qc_no_fit (or
#'     no autoQC rows at all) falls through to 'default' with ALL review fields
#'     NA -- they are never borrowed from a human row.}
#'   \item{\code{policy = 'ruleset'}:} {the four-tier ladder:
#'     reviewed_best_fit (human) > auto_qc_best_fit > reviewed_acceptable_fit
#'     (most recent date_reviewed) > 'default'. auto_qc_pass is informational,
#'     never a tier.}
#' }
#'
#' The review row that selected the fit supplies review_status, reviewed_by,
#' reviewer_set_purity, use_only_purity_run and use_edited_cncf; blank fields
#' stay blank and are never filled from the other writer's row on the same fit.
#' Fits named '*/ultra' and the 'Not selected' sentinel are never selectable.
#' An explicit per-sample fit_to_use only short-circuits fit selection -- the
#' review-field join still runs (using the per-fit human-else-autoQC row), and
#' every per-sample path returns a uniform one-row data.frame (the original's
#' bare-character return broke the adply rbind and skipped the review join).
#'
#' Writer classification is exact match on reviewed_by == 'auto-qc'
#' (autoqc_reviewer_id_2n()); everything else with a verdict is human.
#'
#' @param samples_to_annotate data.table with columns sample_id, sample_path and
#'   optionally fit_to_use
#' @param output_prefix prefix for .cohort.txt, .gene_level.txt, .arm_level.txt,
#'   .ccf.maf etc.
#' @param ncores parallelization for the adply and the file concatenations
#' @param policy 'autoqc' or 'ruleset' (required)
#'
#' @return samples_annotated table for the selected fits
#' @export compile_cohort_annotations_2n
compile_cohort_annotations_2n <- function(samples_to_annotate, output_prefix, ncores=1, policy) {

  if (missing(policy) || !(policy %in% c('autoqc', 'ruleset'))) {
    stop("compile_cohort_annotations_2n: policy must be 'autoqc' or 'ruleset'")
  }

  parallelize = F
  if (ncores > 1) {
    library(doParallel)
    doParallel::registerDoParallel(cores = ncores)
    parallelize = T
  }

  if (!("fit_to_use" %in% colnames(samples_to_annotate))) {
    samples_to_annotate$fit_to_use = NA
  }

  # the frozen review-field set carried onto every cohort row; all-NA when the
  # policy falls through to 'default' (or to a fit no relevant writer rated).
  empty_review <- function() {
    data.frame(review_status = NA_character_,
               review_notes = NA_character_,
               reviewed_by = NA_character_,
               date_reviewed = NA_character_,
               use_only_purity_run = NA,
               use_edited_cncf = NA,
               reviewer_set_purity = NA_character_,
               stringsAsFactors = FALSE)
  }

  autoqc_id = autoqc_reviewer_id_2n()

  samples_annotated <-
    samples_to_annotate %>%
    select(sample_id, sample_path, fit_to_use) %>%
    left_join(
      adply(samples_to_annotate, 1,
          function(x) {
            sample_id = x$sample_id
            sample_path = x$sample_path
            fit_to_use = x$fit_to_use

            qc_file = paste0(sample_path, '/facets_qc.txt')
            review_file = paste0(sample_path, '/facets_review.manifest')

            if (!file.exists(qc_file) | !file.exists(review_file)) { return() }

            qc_runs = fread(qc_file,
                            colClasses = list(character = c("facets_qc_version",
                                                            "facets_suite_version"))) %>%
              filter(fit_name != 'Not selected')

            if (nrow(qc_runs) == 0) { return() }

            reviews = get_review_status(sample_id, sample_path)

            # verdict rows on selectable fits that actually exist on disk;
            # ultra is never selectable by anyone.
            verdicts = reviews %>%
              filter(fit_name != 'Not selected',
                     !grepl('/ultra$', fit_name),
                     fit_name %in% qc_runs$fit_name)

            human_verdicts = verdicts %>%
              filter(!is.na(reviewed_by), reviewed_by != autoqc_id)
            autoqc_verdicts = verdicts %>%
              filter(!is.na(reviewed_by), reviewed_by == autoqc_id)

            # rows whose fit_name is the 'Not selected' sentinel (no-fit verdicts)
            autoqc_no_fit = reviews %>%
              filter(!is.na(reviewed_by), reviewed_by == autoqc_id,
                     review_status == 'auto_qc_no_fit')

            selected_fit_name = NA_character_
            selected_review = empty_review()

            pick_review_for_fit <- function(fit_name_wanted) {
              # per-fit review row for an explicitly forced fit: human verdict
              # (latest) first, else the autoQC row, else no review at all.
              h = human_verdicts %>%
                filter(fit_name == fit_name_wanted,
                       review_status %in% c('reviewed_best_fit', 'reviewed_acceptable_fit')) %>%
                arrange(desc(date_reviewed))
              if (nrow(h) > 0) { return(h %>% head(n=1)) }
              a = autoqc_verdicts %>% filter(fit_name == fit_name_wanted)
              if (nrow(a) > 0) { return(a %>% head(n=1)) }
              NULL
            }

            if (!is.na(fit_to_use)) {
              # explicit override: short-circuits fit selection only.
              selected_fit_name = fit_to_use
              r = pick_review_for_fit(fit_to_use)
              if (!is.null(r)) { selected_review = r }
            } else if (policy == 'autoqc') {
              best = autoqc_verdicts %>% filter(review_status == 'auto_qc_best_fit')
              if (nrow(best) > 0) {
                selected_fit_name = best$fit_name[1]
                selected_review = best %>% head(n=1)
              } else {
                # auto_qc_no_fit, or autoQC never ran: fall through to default.
                # Review fields stay NA -- never borrowed from a human row.
                selected_fit_name = 'default'
              }
            } else if (policy == 'ruleset') {
              tier1 = human_verdicts %>%
                filter(review_status == 'reviewed_best_fit') %>%
                arrange(desc(date_reviewed))
              tier2 = autoqc_verdicts %>% filter(review_status == 'auto_qc_best_fit')
              tier3 = human_verdicts %>%
                filter(review_status == 'reviewed_acceptable_fit') %>%
                arrange(desc(date_reviewed))

              if (nrow(tier1) > 0) {
                selected_review = tier1 %>% head(n=1)
              } else if (nrow(tier2) > 0) {
                selected_review = tier2 %>% head(n=1)
              } else if (nrow(tier3) > 0) {
                selected_review = tier3 %>% head(n=1)
              }
              if (!is.na(selected_review$review_status[1])) {
                selected_fit_name = selected_review$fit_name[1]
              } else {
                selected_fit_name = 'default'  # tier 4; review fields stay NA
              }
            }

            fit = qc_runs %>% filter(fit_name == selected_fit_name) %>% head(n=1)

            if (nrow(fit) == 0) {
              # rule 20: every complete sample has <class>/default, so the fall-
              # through always resolves; reaching here means the tree and the
              # manifest disagree -- fail loudly rather than compile bad rows.
              stop(paste0("compile_cohort_annotations_2n: selected fit '",
                          selected_fit_name, "' not present in ", qc_file,
                          " for sample ", sample_id))
            }

            selected_review = selected_review %>%
              select(review_status, review_notes, reviewed_by, date_reviewed,
                     use_only_purity_run, use_edited_cncf, reviewer_set_purity) %>%
              mutate(use_only_purity_run = as.logical(use_only_purity_run),
                     use_edited_cncf = as.logical(use_edited_cncf),
                     reviewer_set_purity = as.character(reviewer_set_purity),
                     date_reviewed = as.character(date_reviewed))

            fit <- cbind(fit, selected_review)

            ####
            #### update purity and ploidy based on the selecting review row
            ####
            fit <-
              fit %>%
              rowwise %>%
              mutate(purity = ifelse(!is.na(reviewer_set_purity) & reviewer_set_purity != '',
                                     reviewer_set_purity,
                                     ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                                            purity_run_Purity,
                                            hisens_run_Purity))) %>%
              mutate(ploidy = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                                     purity_run_Ploidy,
                                     hisens_run_Ploidy)) %>%
              ungroup() %>%
              as.data.frame()

            return (fit)
          }, .parallel = parallelize)
    )

  samples_annotated <-
    samples_annotated %>%
    rowwise %>%
    mutate(pfx = paste0(sample_path, '/', fit_name, '/', sample_id)) %>%
    mutate(
      arm_level_file = paste0(pfx, '.arm_level.txt'),
      gene_level_file = paste0(pfx, '.gene_level.txt'),
      ccf_file = paste0(pfx, '.ccf.maf'),
      ccf_nonsignedout_file = paste0(pfx, '.nonsignedout.ccf.maf'),
      cncf_file = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                         paste0(pfx, '_purity.cncf.txt'), paste0(pfx, '_hisens.cncf.txt')),
      seg_file_unadjusted = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                                   paste0(pfx, '_purity_diplogR.unadjusted.seg'), paste0(pfx, '_hisens_diplogR.unadjusted.seg')),
      seg_file = ifelse(!is.na(use_only_purity_run) & use_only_purity_run,
                        paste0(pfx, '_purity_diplogR.adjusted.seg'), paste0(pfx, '_hisens_diplogR.adjusted.seg'))
    ) %>%
    mutate(
      arm_level_file_exists = file.exists(arm_level_file),
      gene_level_file_exists = file.exists(gene_level_file),
      ccf_file_exists = file.exists(ccf_file),
      ccf_nonsignedout_file_exists = file.exists(ccf_nonsignedout_file),
      cncf_file_exists = file.exists(cncf_file),
      seg_file_unadjusted_exists = file.exists(seg_file_unadjusted),
      seg_file_exists = file.exists(seg_file)
    )


  write.table(samples_annotated, file=paste0(output_prefix, '.cohort.txt'), quote=F, row.names=F, sep='\t')
  cl <- makeCluster(ncores)

  ccf_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(ccf_file_exists))$ccf_file,
                                  fread,
                                  simplify = F,
                                  USE.NAMES=F),
                        fill = T)
  write.table(ccf_calls, file=paste0(output_prefix, '.ccf.maf'), quote=F, row.names=F, sep='\t')

  ccf_nonsignedout_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(ccf_nonsignedout_file_exists))$ccf_nonsignedout_file,
                                  fread,
                                  simplify = F,
                                  USE.NAMES=F),
                        fill = T)
  write.table(ccf_nonsignedout_calls, file=paste0(output_prefix, '.nonsignedout.ccf.maf'), quote=F, row.names=F, sep='\t')

  arm_level_calls = rbindlist(parSapply(cl,
                                        (samples_annotated %>% filter(arm_level_file_exists))$arm_level_file,
                                        fread,
                                        simplify = F,
                                        USE.NAMES=F),
                              fill = T)
  write.table(arm_level_calls, file=paste0(output_prefix, '.arm_level.txt'), quote=F, row.names=F, sep='\t')

  gene_level_calls = rbindlist(parSapply(cl,
                                        (samples_annotated %>% filter(gene_level_file_exists))$gene_level_file,
                                        fread,
                                        simplify = F,
                                        USE.NAMES=F),
                              fill = T)
  write.table(gene_level_calls, file=paste0(output_prefix, '.gene_level.txt'), quote=F, row.names=F, sep='\t')

  seg_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(seg_file_exists))$seg_file,
                                  fread,
                                  simplify = F,
                                  USE.NAMES=F),
                        fill = T)
  write.table(seg_calls, file=paste0(output_prefix, '_diplogR.adjusted.seg'), quote=F, row.names=F, sep='\t')

  seg_calls_unadjusted = rbindlist(parSapply(cl,
                                             (samples_annotated %>% filter(seg_file_unadjusted_exists))$seg_file_unadjusted,
                                             fread,
                                             simplify = F,
                                             USE.NAMES=F),
                                   fill = T)
  write.table(seg_calls_unadjusted, file=paste0(output_prefix, '_diplogR.unadjusted.seg'), quote=F, row.names=F, sep='\t')


  cncf_calls = rbindlist(parSapply(cl,
                                  (samples_annotated %>% filter(cncf_file_exists))$cncf_file,
                                  fread,
                                  simplify = F,
                                  USE.NAMES=F),
                        fill = T)
  write.table(cncf_calls, file=paste0(output_prefix, '.cncf.txt'), quote=F, row.names=F, sep='\t')


  return (samples_annotated)
}
