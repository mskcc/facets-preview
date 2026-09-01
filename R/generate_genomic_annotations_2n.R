#' generate genomic annotations (2n variant)
#'
#' Fork of generate_genomic_annotations() for facets2n (2n) samples, per the
#' CADENCE 2n autoQC design. The original stays untouched for standard runs.
#' Deltas vs the original:
#'   1. regenerate_qc = TRUE calls metadata_init_2n() (colClasses-protected,
#'      rule-13 is_best_fit) instead of metadata_init(); the regenerate_qc =
#'      FALSE read of facets_qc.txt forces the version columns to character.
#'   2. The review dedup actually selects ONE review row per fit: the human
#'      verdict if one exists (latest date_reviewed among humans), else the 2n
#'      autoQC's row (reviewed_by == 'auto-qc'), else no review (NA fields).
#'      The original's dedup joined its marker on fit_name alone, so it removed
#'      nothing and duplicate rows multiplied through the join below.
#'   3. The review join key is fit_name only. 'path' is never a join key (2n
#'      ruleset rule 17): facets_qc.txt and the manifest disagree on trailing
#'      slashes, which silently NA'd every review column -- so
#'      reviewer_set_purity / use_only_purity_run / use_edited_cncf were never
#'      applied. The dedup in (2) is what makes the fit_name-only join safe;
#'      these two fixes are one change.
#' The per-fit annotation body (QC, gene/arm level, ccf) is identical to the
#' original.
#'
#' @param sample_id - eg: <tumor>_<normal> pair id
#' @param sample_path - the 2n class subtree (e.g. .../<pair>/clinical)
#' @param config_file - facets_preview json config
#' @param regenerate_qc - rebuild facets_qc.txt + manifest via metadata_init_2n
#'
#' @export generate_genomic_annotations_2n
generate_genomic_annotations_2n = function(sample_id, sample_path, config_file, regenerate_qc = T) {

  if (!file.exists(config_file)) {
    stop("invalid config file provided to generate_genomic_annotations_2n")
  }

  config = configr::read.config(config_file)
  source(config$facets_qc_script)
  library(facetsSuite, lib.loc = config$facets_suite_lib)

  if (regenerate_qc) {
    facets_runs_qc <- metadata_init_2n(sample_id, sample_path) %>% data.table
  } else {
    facets_runs_qc <- fread(paste0(sample_path, '/facets_qc.txt'),
                            colClasses = list(character = c("facets_qc_version",
                                                            "facets_suite_version")))
  }

  reviews <-
    get_review_status(sample_id, sample_path) %>%
    select(sample_id = sample, review_status, fit_name, reviewed_by,
           date_reviewed, use_only_purity_run, use_edited_cncf, reviewer_set_purity) %>%
    filter(fit_name != 'Not selected')

  # One review row per fit: human verdict > autoQC verdict > anything else
  # (not_reviewed placeholders carry NA review fields anyway). Within a class,
  # latest date_reviewed wins. Classification is by reviewed_by, exact match on
  # the 2n autoQC identity.
  reviews <-
    reviews %>%
    mutate(writer_rank = case_when(
      review_status %in% c('reviewed_best_fit', 'reviewed_acceptable_fit') &
        !is.na(reviewed_by) & reviewed_by != autoqc_reviewer_id_2n() ~ 2,
      !is.na(reviewed_by) & reviewed_by == autoqc_reviewer_id_2n() ~ 1,
      TRUE ~ 0
    )) %>%
    group_by(fit_name) %>%
    arrange(desc(writer_rank), desc(date_reviewed)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-writer_rank, -reviewed_by) %>%
    replace_na(list(use_only_purity_run = F, use_edited_cncf = F))

  facets_runs_qc <-
    facets_runs_qc %>%
    left_join(reviews, by=c('fit_name'))

  plyr::adply(facets_runs_qc, 1, function(r) {
    print(paste0('processing ', r$tumor_sample_id, ' fit:', r$fit_name))
    prefix = paste0(r$path, '/', r$fit_name, '/', r$tumor_sample_id)
    purity_rdata_file = paste0(prefix, '_purity.Rdata')
    hisens_rdata_file = paste0(prefix, '_hisens.Rdata')

    if (!file.exists(purity_rdata_file) || !file.exists(hisens_rdata_file)) {
      print('_purity or _hisens runs not found. Skipping....')
      return()
    }

    purity_output = facetsSuite::load_facets_output(purity_rdata_file)
    hisens_output = facetsSuite::load_facets_output(hisens_rdata_file)

    metadata = c(
      map_dfr(list(purity_output, hisens_output),
              function(x) { facetsSuite::arm_level_changes(x$segs, x$ploidy, 'hg19')[-5] }),
      map_dfr(list(purity_output, hisens_output),
              function(x) facetsSuite::calculate_lst(x$segs, x$ploidy, 'hg19')),
      map_dfr(list(purity_output, hisens_output),
              function(x) facetsSuite::calculate_ntai(x$segs, x$ploidy, 'hg19')),
      map_dfr(list(purity_output, hisens_output),
              function(x) facetsSuite::calculate_hrdloh(x$segs, x$ploidy)),
      map_dfr(list(purity_output, hisens_output),
              function(x) facetsSuite::calculate_loh(x$segs, x$snps, 'hg19'))
    )

    out_file = paste0(prefix, '_hisens.out')
    out_params = readLines(out_file)

    for ( p_idx in 1:length(out_params)) {
      line = gsub(" |#", "", out_params[p_idx])
      sp = unlist(strsplit(line, "="))
      if (length(sp) == 2) {
        if (sp[1] == "purity_cval"){ out_purity_cval = sp[2] }
        if (sp[1] == "cval"){ out_cval = sp[2] }
      }
    }

    qc = map_dfr(list(purity_output, hisens_output), function(x) check_fit(x, genome = 'hg19')) %>%
      add_column(sample = sample_id,
                 cval = c(out_purity_cval, out_cval), .before = 1)

    # Write QC
    write.table(qc, paste0(prefix, '.qc.txt'), quote=F, row.names=F, sep='\t')

    # Write gene level // use hisensitivity run

    # NOTE: pass `algorithm` by name. Standard facetsSuite is
    # gene_level_changes(facets_output, genome, algorithm), but facets-suite-2n
    # inserts targetFile as the 3rd positional arg
    # (facets_output, genome, targetFile, algorithm). A positional 'em' would be
    # read as targetFile there and crash; naming it works for both APIs, and 2n's
    # targetFile then defaults to NULL -> the genome-wide gene list, matching standard.
    if (r$use_only_purity_run) {
      gene_level = facetsSuite::gene_level_changes(purity_output, 'hg19', algorithm = 'em')
    } else {
      gene_level = facetsSuite::gene_level_changes(hisens_output, 'hg19', algorithm = 'em')
    }
    gene_level = gene_level %>% add_column(sample = sample_id, .before = 1)

    write.table(gene_level, paste0(prefix, '.gene_level.txt'), quote=F, row.names=F, sep='\t')

    # Write arm level // use purity run
    arm_level = facetsSuite::arm_level_changes(purity_output$segs, purity_output$ploidy, 'hg19') %>%
      pluck('full_output') %>%
      add_column(sample = sample_id, .before = 1)
    write.table(arm_level, paste0(prefix, '.arm_level.txt'), quote=F, row.names=F, sep='\t')

    # ccf annotation
    cncf_txt_file = paste0(prefix, '_hisens.cncf.txt')
    purity = hisens_output$purity
    if (r$use_only_purity_run) {
      cncf_txt_file = paste0(prefix, '_purity.cncf.txt')
      purity = purity_output$purity
    }

    if (r$use_edited_cncf) {
      cncf_txt_file = gsub("cncf.txt", "cncf.edited.txt", cncf_txt_file)
    }

    if (!is.na(r$reviewer_set_purity) & is.numeric(r$reviewer_set_purity)) {
      purity = r$reviewer_set_purity
    }

    sample_maf_file = paste0(sample_path, '/', sample_id, '.maf')
    sample_maf_nonsignedout_file = paste0(sample_path, '/', sample_id, '.nonsignedout.maf')

    if (!file.exists(cncf_txt_file)) {
      warning(paste0('cncf file does not exist: ', cncf_txt_file, '! Skipping ccf-annotation for: ', r$sample_id))
      return()
    }

    if (file.exists(sample_maf_file)) {
      maf = fread(sample_maf_file) %>% data.table

      if(nrow(maf) == 0) {
        warning(paste0('No mutations for sample: ', sample_id))
      } else {
        ccf_maf = facetsSuite::ccf_annotate_maf_legacy(maf,
                                                       cncf_txt_file,
                                                       purity,
                                                       algorithm='em')
        ccf_maf$facets_fit = paste0(r$path, '/', r$fit_name)
        ccf_maf$facets_suite_qc = r$facets_suite_qc
        ccf_maf$reviewer_set_purity = r$reviewer_set_purity
        ccf_maf$use_only_purity_run = r$use_only_purity_run
        ccf_maf$use_edited_cncf = r$use_edited_cncf
        ccf_maf$cncf_file_used = cncf_txt_file

        write.table(ccf_maf, file=paste0(sample_path, '/', r$fit_name, '/', sample_id, '.ccf.maf'), quote=F, row.names=F, sep='\t')
      }
    }

    if (file.exists(sample_maf_nonsignedout_file)) {
      maf_nonsignedout = fread(sample_maf_nonsignedout_file) %>% data.table

      if(nrow(maf_nonsignedout) == 0) {
        warning(paste0('No mutations for sample: ', sample_id))
      } else {
        ccf_maf_nonsignedout = facetsSuite::ccf_annotate_maf_legacy(maf_nonsignedout,
                                                       cncf_txt_file,
                                                       purity,
                                                       algorithm='em')
        ccf_maf_nonsignedout$facets_fit = paste0(r$path, '/', r$fit_name)
        ccf_maf_nonsignedout$facets_suite_qc = r$facets_suite_qc
        ccf_maf_nonsignedout$reviewer_set_purity = r$reviewer_set_purity
        ccf_maf_nonsignedout$use_only_purity_run = r$use_only_purity_run
        ccf_maf_nonsignedout$use_edited_cncf = r$use_edited_cncf
        ccf_maf_nonsignedout$cncf_file_used = cncf_txt_file

        write.table(ccf_maf_nonsignedout, file=paste0(sample_path, '/', r$fit_name, '/', sample_id, '.nonsignedout.ccf.maf'), quote=F, row.names=F, sep='\t')
      }
    }


  }, .parallel = F)
}
