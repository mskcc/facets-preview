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
#' @return data.frame(key, label, base, layout, read_only)
#' @export vm_repository_registry
vm_repository_registry <- function(overrides = list()) {
  root <- "/data1/core006/ccs/shared/resources"
  wes  <- "/data1/core006/ccs_pipelines/tempo/wes_repo/Results/v2.1.x/cohort_level/MSKWESRP/somatic"
  reg <- data.frame(
    key   = c("impact", "impact_2n", "impact_heme", "impact_heme_2n", "wes"),
    label = c("IMPACT Standard", "IMPACT 2n", "IMPACT-Heme Standard", "IMPACT-Heme 2n",
              "Whole Exome"),
    base  = c(file.path(root, c("impact/facets/all", "impact_2n/facets/all",
                                "impact_heme/facets/all", "impact_heme_2n/facets/all")),
              wes),
    # dmp_bucket: <base>/<first 7 chars>/<TUMOR>_<NORMAL>/
    # tempo:      <base>/<TUMOR>__<NORMAL>/facets/<TUMOR>__<NORMAL>/
    layout    = c(rep("dmp_bucket", 4), "tempo"),
    # A read-only repository takes no user changes: no refits, no reviews, no
    # segment edits. Load-time manifest / qc generation is unaffected.
    # See is_read_only_path().
    read_only = c(rep(FALSE, 4), TRUE),
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

#' The registry row a sample path belongs to.
#'
#' Longest base that is a path-prefix of `path` wins.
#'
#' @param path a sample directory
#' @param registry a vm_repository_registry() data.frame
#' @return a one-row data.frame, or NULL when no base holds the path
#' @export repository_entry_for_path
repository_entry_for_path <- function(path, registry) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return(NULL)
  if (is.null(registry) || nrow(registry) == 0) return(NULL)
  cp <- sub('/+$', '', gsub('/+', '/', path))
  best <- NA_integer_; best_len <- -1L
  for (i in seq_len(nrow(registry))) {
    b <- sub('/+$', '', gsub('/+', '/', registry$base[i]))
    if (startsWith(cp, paste0(b, '/')) && nchar(b) > best_len) {
      best <- i; best_len <- nchar(b)
    }
  }
  if (is.na(best)) NULL else registry[best, , drop = FALSE]
}

#' Which repository a sample path belongs to.
#'
#' @param path a sample directory
#' @param registry a vm_repository_registry() data.frame
#' @return the repository label, or "Other"
#' @export repository_label_for_path
repository_label_for_path <- function(path, registry) {
  e <- repository_entry_for_path(path, registry)
  if (is.null(e)) "Other" else e$label
}

#' Is this path inside a read-only repository?
#'
#' "Read-only" means users may not change the sample: no refits, no reviews, no
#' segment edits. The app's own bookkeeping on load (creating
#' facets_review.manifest, refreshing facets_qc.txt) still happens, exactly as
#' in any other repository.
#'
#' The bases are set once per process from the registry
#' (`options(fp.read_only_bases=)`); when unset (desktop mode) nothing is
#' read-only.
#'
#' @param path a sample directory, or anything beneath one
#' @return TRUE/FALSE
#' @export is_read_only_path
is_read_only_path <- function(path) {
  bases <- getOption("fp.read_only_bases")
  if (is.null(bases) || length(bases) == 0) return(FALSE)
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) return(FALSE)
  cp <- sub('/+$', '', gsub('/+', '/', path))
  for (b in bases) {
    b <- sub('/+$', '', gsub('/+', '/', b))
    if (nzchar(b) && (cp == b || startsWith(cp, paste0(b, '/')))) return(TRUE)
  }
  FALSE
}

#' Split a DMP id into patient and sample number.
#'
#' Accepts a patient id, or anything that starts with <patient>-<T|N><nn>.
#'
#' @param id e.g. P-0000000, P-0000000-T01, P-0000000-T01-IM6, P-0000000-T01-WES
#' @return list(patient=, tnum=) with tnum NA for a bare patient id; NULL if not a DMP id
#' @export parse_dmp_id
parse_dmp_id <- function(id) {
  id <- trimws(sub("_.*$", "", id))
  m <- regmatches(id, regexec("^([A-Z]-\\d{7,})(?:-([TN]\\d+)(?:-.*)?)?$", id, perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  list(patient = m[2], tnum = if (nzchar(m[3])) m[3] else NA_character_)
}

#' Whole Exome (tempo layout) pair dirs whose tumor id matches `id`.
#'
#' `id` matches a tumor id when it is that id, a leading run of its whole
#' '-'-separated tokens (C-XXXXXX, C-XXXXXX-P001), or, once it reaches the fourth
#' token, a plain prefix (C-XXXXXX-P001-d matches ...-d and ...-d01). So
#' C-XXXXXX-P001 never picks up C-XXXXXX-P0011.
#'
#' @param id a CMO id or id prefix, or a full <TUMOR>__<NORMAL> pair
#' @param base the repository base
#' @param exact require the tumor id to equal `id`
#' @return character vector of facets dirs (<base>/<pair>/facets/<pair>) that exist
#' @export wes_facets_dirs_for_id
wes_facets_dirs_for_id <- function(id, base, exact = FALSE) {
  base <- sub('/+$', '', base)
  if (grepl("__", id, fixed = TRUE)) {
    pairs <- id
  } else {
    pairs <- basename(Sys.glob(file.path(base, paste0(id, "*"))))
    tumor <- sub("__.*$", "", pairs)
    keep  <- tumor == id
    if (!exact) {
      keep <- keep | startsWith(tumor, paste0(id, "-")) |
        (lengths(strsplit(id, "-", fixed = TRUE)) >= 4 & startsWith(tumor, id))
    }
    pairs <- pairs[keep & grepl("__", pairs, fixed = TRUE)]
  }
  dirs <- file.path(base, pairs, "facets", pairs)
  dirs[dir.exists(dirs)]
}

#' Expand a CMO or DMP id into Whole Exome facets directories.
#'
#' @param sample_id a CMO id in any short form, a full pair, or a DMP patient/sample id
#' @param base the Whole Exome repository base
#' @param tracker a load_wes_tracker() data.frame (needed for DMP ids only)
#' @return list(paths=<dirs>, error=<message>|NULL)
#' @export resolve_wes_sample_paths
resolve_wes_sample_paths <- function(sample_id, base, tracker = NULL) {
  sample_id <- trimws(sample_id)
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", sample_id)) {
    return(list(paths = character(), error = paste("Invalid format for sample ID:", sample_id)))
  }
  dmp <- if (grepl("^P-\\d{7}", sample_id)) parse_dmp_id(sample_id) else NULL
  if (!is.null(dmp)) {
    if (is.null(tracker) || nrow(tracker) == 0) {
      return(list(paths = character(),
                  error = paste0("Cannot look up ", sample_id,
                                 ": the Whole Exome sample tracker is not available.")))
    }
    hit <- tracker[tracker$class == "tumor" & tracker$patient == dmp$patient, , drop = FALSE]
    if (!is.na(dmp$tnum)) hit <- hit[hit$tnum == dmp$tnum, , drop = FALSE]
    if (nrow(hit) == 0) {
      return(list(paths = character(),
                  error = paste("No Whole Exome sample is mapped to", sample_id)))
    }
    paths <- unique(unlist(lapply(unique(hit$cmo_id), wes_facets_dirs_for_id,
                                  base = base, exact = TRUE)))
    if (length(paths) == 0) {
      return(list(paths = character(),
                  error = paste0("No Whole Exome facets folder found for: ", sample_id,
                                 " (", paste(unique(hit$cmo_id), collapse = ", "), ")")))
    }
    return(list(paths = paths, error = NULL))
  }
  paths <- wes_facets_dirs_for_id(sample_id, base)
  if (length(paths) == 0) {
    return(list(paths = character(),
                error = paste0("No matching folder found for: ", sample_id,
                               " (searched ", base, ")")))
  }
  list(paths = paths, error = NULL)
}

#' Expand an id against one repository, whatever its layout.
#'
#' @param sample_id the id as typed
#' @param entry a one-row vm_repository_registry() data.frame
#' @param tracker a load_wes_tracker() data.frame (tempo layout, DMP ids)
#' @return list(paths=<dirs>, error=<message>|NULL)
#' @export resolve_repo_entry_paths
resolve_repo_entry_paths <- function(sample_id, entry, tracker = NULL) {
  if (identical(entry$layout, "tempo")) {
    return(resolve_wes_sample_paths(sample_id, entry$base, tracker))
  }
  r <- resolve_repo_sample_path(sample_id, entry$base)
  list(paths = if (is.null(r$path)) character() else r$path, error = r$error)
}
