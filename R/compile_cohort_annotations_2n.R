#' Compile cohort annotations for facets2n (2n) runs, under a review policy
#'
#' Fork of compile_cohort_annotations() for the CADENCE 2n cohort. The original
#' stays untouched for standard runs. Several cohort flavors are compiled side
#' by side by calling this once per policy:
#'
#' \itemize{
#'   \item{\code{policy = 'autoqc'}:} {the fit named by the 2n autoQC's
#'     auto_qc_best_fit row; human reviews ignored entirely. auto_qc_no_fit (or
#'     no autoQC rows at all) falls through to 'default' with ALL review fields
#'     NA -- they are never borrowed from a human row.}
#'   \item{\code{policy = 'human'}:} {human reviews only; autoQC rows ignored
#'     entirely. reviewed_best_fit > reviewed_acceptable_fit (each most recent
#'     date_reviewed first) > 'default' with ALL review fields NA.}
#'   \item{\code{policy = 'overall'}:} {the four-tier ladder:
#'     reviewed_best_fit (human) > auto_qc_best_fit > reviewed_acceptable_fit
#'     (most recent date_reviewed) > 'default'. auto_qc_pass is informational,
#'     never a tier. 'ruleset' is accepted as a deprecated alias.}
#' }
#'
#' The review row that selected the fit supplies review_status, reviewed_by,
#' reviewer_set_purity, use_only_purity_run and use_edited_cncf; blank fields
#' stay blank and are never filled from the other writer's row on the same fit.
#' Fits named '*/ultra' and the 'Not selected' sentinel are never selectable.
#' An explicit per-sample fit_to_use only short-circuits fit selection -- the
#' review-field join still runs (using the per-fit row of the writers the policy
#' admits: autoqc = autoQC only, human = human only, overall = human else
#' autoQC), and every per-sample path returns a uniform one-row data.frame (the
#' original's bare-character return broke the adply rbind and skipped the
#' review join).
#'
#' reviewer_set_purity is coerced to numeric (trimws + as.numeric); a value that
#' does not parse is warned about and ignored, so a stray '0.3 ' or '30%' can
#' no longer silently turn the column into text and disable every override.
#'
#' CCF MAFs follow the policy. The per-fit <fit>/<sample>.ccf.maf files are
#' written once by generate_genomic_annotations_2n with whatever review row
#' existed at the time (human else autoQC) -- before the 2n autoQC has run on a
#' new sample, and before any later human review. So before concatenating, each
#' fit's CCF MAF is checked against this policy's selection: if the
#' reviewer_set_purity it was annotated with, or the cncf file it used
#' (purity vs hisens run, edited or not), differs from the selection, the MAF is
#' re-annotated in memory from <sample>.maf with the selected purity and cncf.
#' The per-fit files on disk are never rewritten. The review-stamp columns
#' (reviewer_set_purity, use_only_purity_run, use_edited_cncf) are always set
#' to the selection's values. cohort.txt gains ccf_status /
#' ccf_nonsignedout_status: 'as_annotated', 'reannotated', or
#' 'stale_unrepaired' (re-annotation needed but its inputs were missing).
#'
#' Writer classification is exact match on reviewed_by == 'auto-qc'
#' (autoqc_reviewer_id_2n()); everything else with a verdict is human.
#'
#' @param samples_to_annotate data.table with columns sample_id, sample_path and
#'   optionally fit_to_use
#' @param output_prefix prefix for .cohort.txt, .gene_level.txt, .arm_level.txt,
#'   .ccf.maf etc.
#' @param ncores parallelization for the adply and the file concatenations
#' @param policy 'autoqc', 'human' or 'overall' (required; 'ruleset' is a
#'   deprecated alias for 'overall')
#'
#' @return samples_annotated table for the selected fits
#' @export compile_cohort_annotations_2n
compile_cohort_annotations_2n <- function(samples_to_annotate, output_prefix, ncores=1, policy) {

  if (!missing(policy) && identical(policy, 'ruleset')) { policy = 'overall' }
  if (missing(policy) || !(policy %in% c('autoqc', 'human', 'overall'))) {
    stop("compile_cohort_annotations_2n: policy must be 'autoqc', 'human' or 'overall'")
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
              # per-fit review row for an explicitly forced fit, from the
              # writers this policy admits: human verdict (latest) first, else
              # the autoQC row, else no review at all.
              if (policy != 'autoqc') {
                h = human_verdicts %>%
                  filter(fit_name == fit_name_wanted,
                         review_status %in% c('reviewed_best_fit', 'reviewed_acceptable_fit')) %>%
                  arrange(desc(date_reviewed))
                if (nrow(h) > 0) { return(h %>% head(n=1)) }
              }
              if (policy != 'human') {
                a = autoqc_verdicts %>% filter(fit_name == fit_name_wanted)
                if (nrow(a) > 0) { return(a %>% head(n=1)) }
              }
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
            } else if (policy == 'human') {
              tier1 = human_verdicts %>%
                filter(review_status == 'reviewed_best_fit') %>%
                arrange(desc(date_reviewed))
              tier2 = human_verdicts %>%
                filter(review_status == 'reviewed_acceptable_fit') %>%
                arrange(desc(date_reviewed))

              if (nrow(tier1) > 0) {
                selected_review = tier1 %>% head(n=1)
              } else if (nrow(tier2) > 0) {
                selected_review = tier2 %>% head(n=1)
              }
              if (!is.na(selected_review$review_status[1])) {
                selected_fit_name = selected_review$fit_name[1]
              } else {
                # no human verdict: default, review fields stay NA -- never
                # borrowed from an autoQC row.
                selected_fit_name = 'default'
              }
            } else if (policy == 'overall') {
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

            rsp_raw = trimws(selected_review$reviewer_set_purity[1])
            rsp_num = suppressWarnings(as.numeric(rsp_raw))
            if (is.na(rsp_num) && !is.na(rsp_raw) && !(rsp_raw %in% c('', 'NA'))) {
              warning(paste0("compile_cohort_annotations_2n: ignoring non-numeric reviewer_set_purity '",
                             rsp_raw, "' for sample ", sample_id, " fit ", selected_fit_name))
            }
            selected_review$reviewer_set_purity = rsp_num

            fit <- cbind(fit, selected_review)

            ####
            #### update purity and ploidy based on the selecting review row
            ####
            fit <-
              fit %>%
              rowwise %>%
              mutate(purity = ifelse(!is.na(reviewer_set_purity),
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
    ) %>%
    ungroup()

  ####
  #### CCF MAFs under this policy's selection (see the CCF note in the docs)
  ####
  ccf_rows = seq_len(nrow(samples_annotated))
  ccf_results = parallel::mclapply(ccf_rows, function(i) {
    s = samples_annotated[i, ]
    sel = list(
      reviewer_set_purity = s$reviewer_set_purity,
      use_only_purity_run = isTRUE(s$use_only_purity_run),
      use_edited_cncf = isTRUE(s$use_edited_cncf),
      pfx = s$pfx)
    list(
      signedout = if (s$ccf_file_exists)
        policy_ccf_maf_2n(s$ccf_file, paste0(s$sample_path, '/', s$sample_id, '.maf'), sel),
      nonsignedout = if (s$ccf_nonsignedout_file_exists)
        policy_ccf_maf_2n(s$ccf_nonsignedout_file, paste0(s$sample_path, '/', s$sample_id, '.nonsignedout.maf'), sel))
  }, mc.cores = ncores)

  failed = vapply(ccf_results, function(x) inherits(x, 'try-error'), logical(1))
  if (any(failed)) {
    stop(paste0("compile_cohort_annotations_2n: CCF assembly failed for ",
                paste(samples_annotated$sample_id[failed], collapse = ', '), ": ",
                as.character(ccf_results[[which(failed)[1]]])))
  }

  ccf_status_of = function(x) if (is.null(x)) NA_character_ else x$status
  samples_annotated$ccf_status = vapply(ccf_results, function(x) ccf_status_of(x$signedout), character(1))
  samples_annotated$ccf_nonsignedout_status = vapply(ccf_results, function(x) ccf_status_of(x$nonsignedout), character(1))

  for (col in c('ccf_status', 'ccf_nonsignedout_status')) {
    n = table(samples_annotated[[col]])
    message("compile_cohort_annotations_2n [", policy, "] ", col, ": ",
            paste(names(n), n, sep = '=', collapse = ', '))
  }

  write.table(samples_annotated, file=paste0(output_prefix, '.cohort.txt'), quote=F, row.names=F, sep='\t')

  ccf_calls = rbindlist(lapply(ccf_results, function(x) x$signedout$maf), fill = T)
  write.table(ccf_calls, file=paste0(output_prefix, '.ccf.maf'), quote=F, row.names=F, sep='\t')

  ccf_nonsignedout_calls = rbindlist(lapply(ccf_results, function(x) x$nonsignedout$maf), fill = T)
  write.table(ccf_nonsignedout_calls, file=paste0(output_prefix, '.nonsignedout.ccf.maf'), quote=F, row.names=F, sep='\t')

  cl <- makeCluster(ncores)

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


  stopCluster(cl)
  return (samples_annotated)
}

# ---------- policy-aware CCF MAFs (helpers for compile_cohort_annotations_2n) ----------

# Thin wrapper over the facetsSuite call generate_genomic_annotations_2n makes,
# so the fixture tests can stub it.
ccf_annotate_2n <- function(maf, cncf_txt_file, purity) {
  facetsSuite::ccf_annotate_maf_legacy(maf, cncf_txt_file, purity, algorithm = 'em')
}

# Unrounded FACETS purity for a fit's purity or hisens run, from the same
# Rdata generate_genomic_annotations_2n reads. facets_qc.txt and the .out files
# both round purity to 2 dp, so neither is used for re-annotation.
# Returns NULL when the Rdata is missing; NA purity is a valid value.
facets_run_purity_2n <- function(pfx, use_only_purity_run) {
  rdata_file = paste0(pfx, if (use_only_purity_run) '_purity.Rdata' else '_hisens.Rdata')
  if (!file.exists(rdata_file)) { return(NULL) }
  facetsSuite::load_facets_output(rdata_file)$purity
}

same_num_2n <- function(a, b, tol = 1e-9) {
  a = suppressWarnings(as.numeric(a)); b = suppressWarnings(as.numeric(b))
  (is.na(a) && is.na(b)) || (!is.na(a) && !is.na(b) && abs(a - b) < tol)
}

# One per-fit CCF MAF with the policy's selection applied.
#
# Staleness is judged on the review INPUTS generate_genomic_annotations_2n
# stamped into the MAF -- reviewer_set_purity and the cncf file it used -- not
# on the purity value itself, because the MAF carries the unrounded Rdata
# purity and the cohort row the rounded facets_qc.txt one. The one value check:
# when a reviewer purity is selected, the MAF's purity column must equal it
# (catches annotation runs whose is.numeric() guard skipped a text-typed
# override but still stamped it).
#
# sel: list(reviewer_set_purity (numeric/NA), use_only_purity_run, use_edited_cncf, pfx)
# Returns list(maf, status) with status in as_annotated | reannotated | stale_unrepaired.
policy_ccf_maf_2n <- function(ccf_file, source_maf_file, sel) {
  ccf = fread(ccf_file)

  cncf_file = paste0(sel$pfx, if (sel$use_only_purity_run) '_purity.cncf.txt' else '_hisens.cncf.txt')
  if (sel$use_edited_cncf) { cncf_file = gsub("cncf.txt", "cncf.edited.txt", cncf_file) }

  stamped = function(col) if (col %in% names(ccf) && nrow(ccf) > 0) ccf[[col]][1] else NA
  cncf_used = as.character(stamped('cncf_file_used'))
  stale = nrow(ccf) > 0 && (
    !same_num_2n(stamped('reviewer_set_purity'), sel$reviewer_set_purity) ||
    is.na(cncf_used) || basename(cncf_used) != basename(cncf_file) ||
    (!is.na(sel$reviewer_set_purity) && !same_num_2n(stamped('purity'), sel$reviewer_set_purity)))

  status = 'as_annotated'
  if (stale) {
    purity = sel$reviewer_set_purity
    if (is.na(purity)) { purity = facets_run_purity_2n(sel$pfx, sel$use_only_purity_run) }
    maf = if (file.exists(source_maf_file)) fread(source_maf_file) else NULL

    if (is.null(purity) || is.null(maf) || nrow(maf) == 0 || !file.exists(cncf_file)) {
      warning(paste0("compile_cohort_annotations_2n: ", ccf_file,
                     " does not match the policy selection but cannot be re-annotated",
                     " (missing purity, ", basename(source_maf_file), " or ", basename(cncf_file),
                     "); keeping the per-fit MAF as written"))
      status = 'stale_unrepaired'
    } else {
      fresh = data.table(ccf_annotate_2n(maf, cncf_file, purity))
      # same trailing columns, same order, as generate_genomic_annotations_2n
      fresh[, `:=`(facets_fit = stamped('facets_fit'),
                   facets_suite_qc = stamped('facets_suite_qc'),
                   reviewer_set_purity = NA_real_,
                   use_only_purity_run = NA,
                   use_edited_cncf = NA,
                   cncf_file_used = cncf_file)]
      ccf = fresh
      status = 'reannotated'
    }
  }

  # := without i replaces the whole column, so its type follows the value. A
  # plain ccf$x <- assignment would coerce into fread's type (all-NA columns
  # read as logical turn 0.77 into TRUE).
  if (nrow(ccf) > 0) {
    ccf[, `:=`(reviewer_set_purity = as.numeric(sel$reviewer_set_purity),
               use_only_purity_run = sel$use_only_purity_run,
               use_edited_cncf = sel$use_edited_cncf)]
  }
  list(maf = ccf, status = status)
}
