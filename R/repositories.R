### ---------------------------------------------------------------------------
### VM-mode sample repositories.
###
### On the facetflow VM the FACETS trees live under one root, one tree per
### assay x pipeline (standard / 2n). The "Add samples by DMP-ID" panel picks
### one of these and expands short DMP ids against it. Desktop mode keeps its
### own per-session repository paths and does not use this table.
### ---------------------------------------------------------------------------

#' The VM repository table.
#'
#' Bases already include the `all/` bucket root. Each may be overridden from
#' `global.config` with `repo_base_<key> = /path` (passed in as `overrides`).
#'
#' @param overrides a named list (the parsed global.config), may be empty
#' @return data.frame(key, label, base)
#' @export vm_repository_registry
vm_repository_registry <- function(overrides = list()) {
  root <- "/data1/core006/ccs/shared/resources"
  reg <- data.frame(
    key   = c("impact", "impact_2n", "impact_heme", "impact_heme_2n"),
    label = c("IMPACT Standard", "IMPACT 2n", "IMPACT-Heme Standard", "IMPACT-Heme 2n"),
    base  = file.path(root, c("impact/facets/all", "impact_2n/facets/all",
                              "impact_heme/facets/all", "impact_heme_2n/facets/all")),
    stringsAsFactors = FALSE)
  if (is.list(overrides)) {
    for (k in reg$key) {
      ov <- overrides[[paste0("repo_base_", k)]]
      if (!is.null(ov) && length(ov) == 1 && !is.na(ov) && nzchar(ov)) {
        reg$base[reg$key == k] <- sub('/+$', '', ov)
      }
    }
  }
  reg
}

#' DMP sample-id shapes accepted by the loader.
#'
#' `I[MH]` covers both IMPACT (-IM6) and IMPACT-Heme (-IH4) panels.
#'
#' @return list(short=, full=) regexes
#' @export dmp_id_patterns
dmp_id_patterns <- function() {
  list(short = "^[A-Z]-\\d{7}-T\\d{2}-I[MH]\\d$",
       full  = "^[A-Z]-\\d{7}-T\\d{2}-I[MH]\\d_[A-Z]-\\d{7}-N\\d{2}-I[MH]\\d$")
}

#' Expand a DMP id into a sample (pair) directory under a repository base.
#'
#' Layout: `<base>/<first 7 chars of the id>/<TUMOR>_<NORMAL>/`. A full-form id
#' names the pair dir directly; a short-form id is prefix-matched against the
#' bucket's dirs (first match wins, as the desktop loader does). For a 2n
#' repository this is the PAIR dir; resolve_2n_pair_dir() picks the class.
#'
#' @param sample_id a short or full DMP id
#' @param base a repository base (already ending in the bucket root, e.g. .../all)
#' @return list(path=<dir>|NULL, error=<message>|NULL)
#' @export resolve_repo_sample_path
resolve_repo_sample_path <- function(sample_id, base) {
  pat <- dmp_id_patterns()
  sample_id <- trimws(sample_id)
  if (!grepl(pat$full, sample_id) && !grepl(pat$short, sample_id)) {
    return(list(path = NULL, error = paste("Invalid format for sample ID:", sample_id)))
  }
  bucket <- file.path(sub('/+$', '', base), substr(sample_id, 1, 7))

  if (grepl(pat$full, sample_id)) {
    p <- file.path(bucket, sample_id)
    if (!dir.exists(p)) {
      return(list(path = NULL, error = paste("Folder does not exist for:", sample_id)))
    }
    return(list(path = p, error = NULL))
  }

  if (!dir.exists(bucket)) {
    return(list(path = NULL, error = paste0("No matching folder found for: ", sample_id,
                                            " (searched ", bucket, ")")))
  }
  dirs  <- list.dirs(bucket, full.names = FALSE, recursive = FALSE)
  found <- dirs[startsWith(dirs, sample_id)]
  if (length(found) == 0) {
    return(list(path = NULL, error = paste0("No matching folder found for: ", sample_id,
                                            " (searched ", bucket, ")")))
  }
  list(path = file.path(bucket, found[1]), error = NULL)
}

#' Which repository a sample path belongs to.
#'
#' Longest base that is a path-prefix of `path` wins; "Other" when none is.
#'
#' @param path a sample directory
#' @param registry a vm_repository_registry() data.frame
#' @return the repository label, or "Other"
#' @export repository_label_for_path
repository_label_for_path <- function(path, registry) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return("Other")
  cp <- sub('/+$', '', gsub('/+', '/', path))
  best <- NA_character_; best_len <- -1L
  for (i in seq_len(nrow(registry))) {
    b <- sub('/+$', '', gsub('/+', '/', registry$base[i]))
    if (startsWith(cp, paste0(b, '/')) && nchar(b) > best_len) {
      best <- registry$label[i]; best_len <- nchar(b)
    }
  }
  if (is.na(best)) "Other" else best
}
