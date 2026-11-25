### ---------------------------------------------------------------------------
### Whole Exome <-> DMP id mapping, and cross-repository sample links.
###
### Tempo WES samples are named by CMO id. The sample tracker that tempo2cbio
### uses carries, per CMO sample, the DMP id of the same specimen
### (<patient>-<T|N><nn>-WES...). Patient + sample number is what ties a WES
### sample to its IMPACT counterpart (<patient>-<Tnn>-IM6 etc).
###
### The tracker is rewritten nightly outside this app, so it is read from a
### configured path and re-read whenever its mtime changes -- nothing about its
### contents is baked into the package or the image.
### ---------------------------------------------------------------------------

.wes_tracker_cache <- new.env(parent = emptyenv())

.empty_wes_tracker <- function() {
  data.frame(cmo_id = character(), dmp_id = character(), patient = character(),
             tnum = character(), class = character(), stringsAsFactors = FALSE)
}

#' Read the WES sample tracker as a CMO id <-> DMP id table.
#'
#' Columns are found case-insensitively (`CMO_Sample_ID`, `investigatorSampleId`,
#' optional `tumorOrNormal`). A cell holding several comma-joined DMP ids yields
#' one row per id. Rows without a DMP-shaped id are dropped.
#'
#' The result is cached per path and reused until the file's mtime changes. A
#' file that cannot be read (missing, or caught mid-rewrite) returns the last
#' good table for that path, or an empty one.
#'
#' @param path the tracker TSV; defaults to `getOption("fp.wes_sample_tracker")`
#' @return data.frame(cmo_id, dmp_id, patient, tnum, class) with class tumor/normal
#' @export load_wes_tracker
load_wes_tracker <- function(path = getOption("fp.wes_sample_tracker")) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(.empty_wes_tracker())
  }
  cached <- .wes_tracker_cache[[path]]
  last_good <- if (is.null(cached)) .empty_wes_tracker() else cached$table
  if (!file.exists(path)) {
    if (is.null(cached)) warning(paste0("WES sample tracker not found: ", path))
    return(last_good)
  }
  mtime <- file.mtime(path)
  if (!is.null(cached) && identical(cached$mtime, mtime)) return(cached$table)

  raw <- tryCatch(
    data.table::fread(path, sep = "\t", header = TRUE, colClasses = "character",
                      quote = "", fill = TRUE, data.table = FALSE, showProgress = FALSE),
    error = function(e) NULL)
  cols <- if (is.null(raw)) character() else tolower(names(raw))
  i_cmo <- match("cmo_sample_id", cols)
  i_dmp <- match("investigatorsampleid", cols)
  if (is.null(raw) || is.na(i_cmo) || is.na(i_dmp)) {
    warning(paste0("WES sample tracker unreadable or missing CMO_Sample_ID / ",
                   "investigatorSampleId: ", path))
    return(last_good)
  }
  i_cls <- match("tumorornormal", cols)

  cmo <- trimws(raw[[i_cmo]])
  ids <- strsplit(ifelse(is.na(raw[[i_dmp]]), "", raw[[i_dmp]]), ",", fixed = TRUE)
  n   <- lengths(ids)
  tab <- data.frame(cmo_id = rep(cmo, n), dmp_id = trimws(unlist(ids)),
                    stringsAsFactors = FALSE)
  cls <- if (is.na(i_cls)) rep(NA_character_, sum(n)) else rep(tolower(trimws(raw[[i_cls]])), n)

  m  <- regmatches(tab$dmp_id, regexec("^([A-Z]-\\d{7,})-([TN]\\d+)(?:-.*)?$", tab$dmp_id, perl = TRUE))
  ok <- lengths(m) > 0 & nzchar(tab$cmo_id)
  tab <- tab[ok, , drop = FALSE]
  tab$patient <- vapply(m[ok], function(x) x[2], character(1))
  tab$tnum    <- vapply(m[ok], function(x) x[3], character(1))
  cls <- cls[ok]
  # Trust the tracker's tumorOrNormal; fall back to the T/N in the DMP id.
  tab$class <- ifelse(!is.na(cls) & cls %in% c("tumor", "normal"), cls,
                      ifelse(startsWith(tab$tnum, "T"), "tumor", "normal"))
  tab <- unique(tab)
  rownames(tab) <- NULL

  .wes_tracker_cache[[path]] <- list(mtime = mtime, table = tab)
  tab
}

#' The tumor id of a sample directory, given the repository it lives in.
#'
#' @param path a sample directory (for 2n, the pair dir or its class subtree)
#' @param entry the one-row registry entry holding `path`
#' @return the tumor id (DMP id for dmp_bucket, CMO id for tempo), or NA
#' @export tumor_id_for_repo_path
tumor_id_for_repo_path <- function(path, entry) {
  base <- sub('/+$', '', gsub('/+', '/', entry$base))
  rel  <- substring(sub('/+$', '', gsub('/+', '/', path)), nchar(base) + 2)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
  if (identical(entry$layout, "tempo")) {
    if (length(parts) < 1) return(NA_character_)
    return(sub("__.*$", "", parts[1]))
  }
  if (length(parts) < 2) return(NA_character_)
  sub("_.*$", "", parts[2])
}

#' Other repositories' copies of the sample at `path`.
#'
#' A Whole Exome sample is tied to an IMPACT-family sample by patient + sample
#' number, through the tracker. Between two IMPACT-family repositories (e.g.
#' IMPACT Standard and IMPACT 2n) only the identical tumor id counts, so an
#' IMPACT sample is never linked to a Heme sample that merely shares a number.
#'
#' @param path the loaded sample directory
#' @param registry a vm_repository_registry() data.frame
#' @param tracker a load_wes_tracker() data.frame
#' @return data.frame(repo_key, label, sample_id, path); zero rows when none
#' @export find_associated_samples
find_associated_samples <- function(path, registry, tracker = NULL) {
  out <- data.frame(repo_key = character(), label = character(),
                    sample_id = character(), path = character(), stringsAsFactors = FALSE)
  entry <- repository_entry_for_path(path, registry)
  if (is.null(entry)) return(out)
  tumor <- tumor_id_for_repo_path(path, entry)
  if (is.na(tumor) || !nzchar(tumor)) return(out)
  if (is.null(tracker)) tracker <- .empty_wes_tracker()
  tracker <- tracker[tracker$class == "tumor", , drop = FALSE]
  from_wes <- identical(entry$layout, "tempo")

  # (patient, tnum) keys of this specimen.
  if (from_wes) {
    keys <- unique(tracker[tracker$cmo_id == tumor, c("patient", "tnum"), drop = FALSE])
  } else {
    d <- parse_dmp_id(tumor)
    if (is.null(d) || is.na(d$tnum)) return(out)
    keys <- data.frame(patient = d$patient, tnum = d$tnum, stringsAsFactors = FALSE)
  }

  add <- function(e, ids, paths) {
    if (length(paths) == 0) return(invisible())
    out <<- rbind(out, data.frame(repo_key = e$key, label = e$label, sample_id = ids,
                                  path = paths, stringsAsFactors = FALSE))
  }

  for (i in seq_len(nrow(registry))) {
    e <- registry[i, , drop = FALSE]
    if (e$key == entry$key || !dir.exists(e$base)) next
    if (identical(e$layout, "tempo")) {
      if (nrow(keys) == 0) next
      cmo <- unique(tracker$cmo_id[paste(tracker$patient, tracker$tnum) %in%
                                     paste(keys$patient, keys$tnum)])
      for (id in cmo) {
        p <- wes_facets_dirs_for_id(id, e$base, exact = TRUE)
        add(e, rep(id, length(p)), p)
      }
    } else {
      # From WES any panel with the same patient + number; otherwise the same tumor id.
      prefixes <- if (from_wes) paste0(keys$patient, "-", keys$tnum, "-") else paste0(tumor, "_")
      for (pre in unique(prefixes)) {
        bucket <- file.path(sub('/+$', '', e$base), substr(pre, 1, 7))
        if (!dir.exists(bucket)) next
        dirs <- list.dirs(bucket, full.names = FALSE, recursive = FALSE)
        dirs <- dirs[startsWith(dirs, pre)]
        add(e, sub("_.*$", "", dirs), file.path(bucket, dirs))
      }
    }
  }
  out <- out[!duplicated(out$path), , drop = FALSE]
  rownames(out) <- NULL
  out
}
