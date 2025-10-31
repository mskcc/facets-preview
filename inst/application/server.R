#' helper function for app
#'
#' @param input list of facets run directories
#' @param output progress bar from shiny
#' @return runs serverend
#' @export server
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT datatable
#' @import dplyr
#' @import stringr
#' @import shinyjs
#' @import rhandsontable
#' @import gridExtra
#' @import digest
#' @import httr
#' @import jsonlite

options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)


read_session_data <- function(file_path) {
  if (file.exists(file_path)) {
    return(read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
  } else {
    return(NULL)
  }
}

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (is.atomic(a) && length(a) == 1 && is.na(a)) return(b)
    if (is.character(a) && length(a) == 1 && !nzchar(a)) return(b)
    a
  }
}


# ---- Simple key=value config reader for VM global.config ----
.read_kv_config <- function(cfg_path) {
  if (!file.exists(cfg_path)) return(list())
  lines <- readLines(cfg_path, warn = FALSE)
  # strip comments and whitespace
  lines <- gsub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  out <- list()
  for (ln in lines) {
    m <- regexec("^([A-Za-z0-9_]+)\\s*=\\s*(.*)$", ln)
    mm <- regmatches(ln, m)[[1]]
    if (length(mm) == 3) {
      key <- mm[2]
      val <- mm[3]
      # strip surrounding quotes if present
      val <- trimws(val)
      if (startsWith(val, "\"") && endsWith(val, "\"")) {
        val <- substring(val, 2, nchar(val) - 1)
      }
      out[[key]] <- val
    }
  }
  out
}

# Return restricted paths respecting VM config, else default
get_restricted_paths <- function() {
  if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
    rp <- getOption("fp.restricted_paths", NULL)
    if (is.null(rp)) character(0) else rp
  } else {
    c("/juno/work/ccs/shared/resources/", "/data1/core006/ccs/shared/resources/")
  }
}
# -------------------------------------------------------------

init_fp_paths <- function() {
  mode <- Sys.getenv("FP_MODE", "")
  if (identical(mode, "vm")) {
    base <- Sys.getenv("FP_USER_BASE_WORKDIR", "")
    user <- Sys.getenv("FP_USER_ID", "")
    store_dir <- normalizePath(file.path(base, user), mustWork = FALSE)

    if (!nzchar(base) || !nzchar(user)) {
      warning("[FP] VM mode enabled but FP_USER_BASE_WORKDIR or FP_USER_ID is empty.")
    }

    dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)

    options(
      "fp.store_dir"     = store_dir,
      "fp.session_file"  = file.path(store_dir, ".fp_session.dat"),
      "fp.personal_file" = file.path(store_dir, ".fp_personal.dat")
    )

    # [VM] Load global.config from the *base* workdir (shared across users)
    # File example:
    #   restricted_paths = "/data1/core006/ccs/shared/resources/"
    cfg_path <- file.path(base, "global.config")
    cfg <- .read_kv_config(cfg_path)
    if (!is.null(cfg$restricted_paths)) {
      # allow comma-separated values too
      paths <- unlist(strsplit(cfg$restricted_paths, "\\s*,\\s*"))
      options("fp.restricted_paths" = paths)
    }

    opts_repo <- list(
      impact = cfg$impact_repo_path %||% "",
      tcga   = cfg$tcga_repo_path   %||% "",
      tempo  = cfg$tempo_repo_path  %||% ""
    )
    options("fp.default_repo_paths" = opts_repo)

    message("[FP] VM init: repo defaults = ", paste(
      sprintf("impact=%s, tcga=%s, tempo=%s", opts_repo$impact, opts_repo$tcga, opts_repo$tempo),
      collapse=""
    ))

    # legacy symlinks
    try({
      home_session  <- path.expand("~/.fp_session.dat")
      home_personal <- path.expand("~/.fp_personal.dat")
      if (!file.exists(home_session)  && !file.exists(home_session,  follow.links = TRUE)) {
        file.symlink(getOption("fp.session_file"),  home_session)
      }
      if (!file.exists(home_personal) && !file.exists(home_personal, follow.links = TRUE)) {
        file.symlink(getOption("fp.personal_file"), home_personal)
      }
    }, silent = TRUE)

  } else {
    options(
      "fp.store_dir"     = path.expand("~"),
      "fp.session_file"  = path.expand("~/.fp_session.dat"),
      "fp.personal_file" = path.expand("~/.fp_personal.dat")
    )
  }
}

# Call once at startup
init_fp_paths()

fp_store_dir     <- function() getOption("fp.store_dir")
fp_session_path  <- function() getOption("fp.session_file")
fp_personal_path <- function() getOption("fp.personal_file")

is_vm_mode <- function() identical(Sys.getenv("FP_MODE", ""), "vm")

get_vm_repo_defaults <- function() {
  d <- getOption("fp.default_repo_paths", default = NULL)
  if (is.null(d)) list(impact="", tcga="", tempo="") else d
}

# Identity translator for VM mode; passthrough for now
vm_identity_path <- function(p) {
  if (is_vm_mode()) return(p %||% "")
  p %||% ""
}

fp_personal_dir <- function() {
  # Prefer the UI (user might have overridden), then session_data, then VM default
  p <- input$personal_storage_path %||% session_data$personal_storage_path %||% file.path(fp_store_dir(), "personal")
  if (!nzchar(p)) return("")
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  if (!grepl("/$", p)) p <- paste0(p, "/")
  p
}

.log_df <- function(label, df, max_rows = 5, max_cols = 20) {
  try({
    if (is.null(df)) {
      message("[DEBUG] ", label, ": <NULL>")
      return(invisible())
    }
    message("[DEBUG] ", label, ": class=", paste(class(df), collapse = "/"),
            " nrow=", ifelse(is.data.frame(df), nrow(df), NA),
            " ncol=", ifelse(is.data.frame(df), ncol(df), NA))
    if (is.data.frame(df)) {
      # names
      message("[DEBUG] ", label, " names: ", paste(utils::head(names(df), max_cols), collapse = ", "),
              if (ncol(df) > max_cols) " …" else "")
      # glimpse types
      tps <- vapply(df, function(x) paste(class(x), collapse = "/"), character(1))
      message("[DEBUG] ", label, " types: ", paste(utils::head(paste0(names(df), ":", tps), max_cols), collapse = ", "),
              if (length(tps) > max_cols) " …" else "")
      # top rows (non-breaking)
      message("[DEBUG] ", label, " head:")
      utils::capture.output(utils::head(df, max_rows)) |> paste(collapse = "\n") |> message()
    }
  }, silent = TRUE)
  invisible(NULL)
}

.dt_error_callback <- DT::JS(
  "table.on('error.dt', function(e, settings, techNote, message){",
  "  Shiny.setInputValue('datatable_samples_error', message, {priority: 'event'});",
  "});"
)

server <-
function(input, output, session) {
  values <- reactiveValues(manifest_metadata = NULL, samples_ready = FALSE, samples_loading = FALSE, dt_sel = NULL, config_file = ifelse( exists("facets_preview_config_file"), facets_preview_config_file, "<not set>"))
  output$verbatimTextOutput_sessionInfo <- renderPrint({print(sessionInfo())})
  output$verbatimTextOutput_signAs <- renderText({paste0(system('whoami', intern = T))})

  ignore_storage_change <- reactiveVal(FALSE)
  ignore_storage_change_compare <- reactiveVal(FALSE)
  selected_counts_file <- reactiveVal(NULL)
  skipSampleChange <- reactiveVal(FALSE)

  vm_prefilled <- reactiveVal(FALSE)

  restricted_paths <- get_restricted_paths()

  session_data <- reactiveValues(
    personal_storage_path = NULL,
    mount_refit_path = NULL,
    session_remote_refit = NULL,
    remote_refit_path = NULL,
    repository_path_impact = NULL,
    remote_path_impact = NULL,
    session_switch_impact = NULL,
    repository_path_tempo = NULL,
    remote_path_tempo = NULL,
    session_switch_tempo = NULL,
    repository_path_tcga = NULL,
    remote_path_tcga = NULL,
    session_switch_tcga = NULL,
    auth_password = NULL,
    password_valid = NULL,
    password_personal = NULL
  )

  shinyjs::hide("div_imageOutput_pngImage2")

  default_geneLevel_columns <- c("sample", "gene", "chrom", "cf.em", "tcn.em", "lcn.em", "cn_state", "filter")
  geneLevel_columns <- reactiveVal(NULL)
  geneLevel_columns_compare <- reactiveVal(NULL)
  shinyjs::hide("displayModeSwitchDiv_geneLevel")


  default_armLevel_columns <- c("sample", "arm", "tcn", "lcn", "cn_length", "arm_length", "frac_of_arm", "cn_state")
  armLevel_columns <- reactiveVal(NULL)
  armLevel_columns_compare <- reactiveVal(NULL)
  shinyjs::hide("displayModeSwitchDiv_armLevel")
  shinyjs::hide("selectColumnsDiv_armLevel")


  valid_hashed_password <- "0c75707e31ad67243d510045c5545401339db24104a5d9947ef57eca0e300307"
  valid_personal_password <- "e2b03f0d6b892667621b9f8cfa54353db7d760f72b8c26f4e9577800f8bc4505"

  session_data_file <- fp_session_path()
  personal_repo_meta_file <- fp_personal_path()
  initial_session_data <- read_session_data(session_data_file)










  if (!is.null(initial_session_data)) {

    updateTextInput(session, "personal_storage_path", value = initial_session_data$personal_storage_path)

    updateTextInput(session, "mount_refit_path", value = initial_session_data$mount_refit_path)
    updateTextInput(session, "remote_refit_path", value = initial_session_data$remote_refit_path)
    shinyWidgets::updateSwitchInput(session, "session_remote_refit", value = initial_session_data$session_remote_refit)

    updateTextInput(session, "repository_path_impact", value = initial_session_data$repository_path_impact)
    updateTextInput(session, "remote_path_impact", value = initial_session_data$remote_path_impact)
    shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = initial_session_data$session_switch_impact)

    updateTextInput(session, "repository_path_tempo", value = initial_session_data$repository_path_tempo)
    updateTextInput(session, "remote_path_tempo", value = initial_session_data$remote_path_tempo)
    shinyWidgets::updateSwitchInput(session, "session_switch_tempo", value = initial_session_data$session_switch_tempo)

    updateTextInput(session, "repository_path_tcga", value = initial_session_data$repository_path_tcga)
    updateTextInput(session, "remote_path_tcga", value = initial_session_data$remote_path_tcga)
    shinyWidgets::updateSwitchInput(session, "session_switch_tcga", value = initial_session_data$session_switch_tcga)

    updateTextInput(session, "auth_password", value = initial_session_data$auth_password)

    # Update the reactiveValues object
    session_data$personal_storage_path <- initial_session_data$personal_storage_path

    session_data$mount_refit_path <- initial_session_data$mount_refit_path
    session_data$remote_refit_path <- initial_session_data$remote_refit_path
    session_data$session_remote_refit <- initial_session_data$session_remote_refit

    session_data$repository_path_impact <- initial_session_data$repository_path_impact
    session_data$remote_path_impact <- initial_session_data$remote_path_impact
    session_data$session_switch_impact <- initial_session_data$session_switch_impact

    session_data$repository_path_tempo <- initial_session_data$repository_path_tempo
    session_data$remote_path_tempo <- initial_session_data$remote_path_tempo
    session_data$session_switch_tempo <- initial_session_data$session_switch_tempo

    session_data$repository_path_tcga <- initial_session_data$repository_path_tcga
    session_data$remote_path_tcga <- initial_session_data$remote_path_tcga
    session_data$session_switch_tcga <- initial_session_data$session_switch_tcga

    session_data$auth_password <- initial_session_data$auth_password
    session_data$password_valid <- 0 #Always assume the password is invalid when reading the file.
    session_data$password_personal <- 0 #Always assume the password is invalid when reading the file.

  }

  # ---- VM: apply defaults if inputs still blank (runs once, no onFlushed/later) ----
  local({
    if (!is_vm_mode()) return()

    defs <- get_vm_repo_defaults()

    set_if_blank <- function(id, value) {
      val <- isolate(input[[id]])
      if (!nzchar(val) && nzchar(value)) {
        updateTextInput(session, id, value = value)
        TRUE
      } else {
        FALSE
      }
    }

    # Repos from global.config (only if still blank after session restore)
    imp_set  <- set_if_blank("repository_path_impact", defs$impact %||% "")
    if (imp_set) updateTextInput(session, "remote_path_impact", value = defs$impact %||% "")
    shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)
    session_data$session_switch_impact <- TRUE

    tcga_set <- set_if_blank("repository_path_tcga",   defs$tcga   %||% "")
    if (tcga_set) updateTextInput(session, "remote_path_tcga",   value = defs$tcga   %||% "")

    tempo_set <- set_if_blank("repository_path_tempo",  defs$tempo  %||% "")
    if (tempo_set) updateTextInput(session, "remote_path_tempo",  value = defs$tempo  %||% "")

    # Personal storage defaults to <store_dir>/personal/ if still blank after session restore
    if (!nzchar(isolate(input$personal_storage_path))) {
      base <- fp_store_dir() %||% ""
      if (nzchar(base)) {
        personal_dir <- file.path(base, "personal")
        dir.create(personal_dir, recursive = TRUE, showWarnings = FALSE)
        if (!grepl("/$", personal_dir)) personal_dir <- paste0(personal_dir, "/")
        updateTextInput(session, "personal_storage_path", value = personal_dir)
        session_data$personal_storage_path <- personal_dir
      }
    }

    if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
      ps <- file.path(fp_store_dir(), "personal")
      dir.create(ps, recursive = TRUE, showWarnings = FALSE)
      if (!grepl("/$", ps)) ps <- paste0(ps, "/")

      # Only prefill if the session file didn't already set it (initial_session_data)
      if (is.null(initial_session_data) || !nzchar(initial_session_data$personal_storage_path)) {
        updateTextInput(session, "personal_storage_path", value = ps)
        session_data$personal_storage_path <- ps
      }
    }


    # Refit remote mirrors local (VM shows as hidden/disabled anyway)
    updateTextInput(session, "remote_refit_path", value = (isolate(input$mount_refit_path) %||% ""))

    # Force all mount switches OFF visually in VM
    shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)
    shinyWidgets::updateSwitchInput(session, "session_switch_tempo",  value = FALSE)
    shinyWidgets::updateSwitchInput(session, "session_switch_tcga",   value = FALSE)
    shinyWidgets::updateSwitchInput(session, "session_remote_refit",  value = FALSE)
  })


  observe({
    values$config_file = ifelse( exists("facets_preview_config_file"), facets_preview_config_file, "<not set>")
    if (!suppressWarnings(file.exists(values$config_file))) {
      showModal(modalDialog( title = "config file not found",
                             'config file not found. Expects \"facets_preview_config_file\" variable in .Rprofile',
                             easyClose = TRUE))
      return(NULL)
    }

    values$config = configr::read.config(values$config_file)

    updateSelectInput(session, "selectInput_repo",
                      choices = as.list(c("none", values$config$repo$name)),
                      selected = "none")

    source(values$config$facets_qc_script)

    library(facetsSuite, lib.loc = values$config$facets_suite_lib)

    shinyjs::html("element_facets_qc_version1", paste0('facets qc version: ', facets_qc_version()))
    shinyjs::html("element_facets_qc_version2", paste0('facets qc version: ', facets_qc_version()))

  })

  observeEvent(input$link_choose_repo, {
    # Change the following line for more examples
    showModal(
      modalDialog(
        selectInput("selectInput_repo", "choose respository:", values$config$repo$name),
        footer = tagList(
          actionButton("actionButton_selectRepo", "Submit"),
          modalButton('Dismiss'))
      )
    )
  })

  observeEvent(input$datatable_samples_error, {
    msg <- input$datatable_samples_error
    message('[DT error hook] ', msg)
    showNotification(paste('DT error:', msg), type = 'error', duration = 6)
  })

  observeEvent(input$actionButton_selectRepo, {
    values$selected_repo = as.list(values$config$repo %>% filter(name == input$selectInput_repo) %>% head(n=1))
    shinyjs::html("element_repo_name", paste0('Selected repository: ', values$selected_repo$name))
    shinyjs::html("element_repo_manifest", paste0('manifest file: ', values$selected_repo$manifest_file))
    removeModal()
  })

  #' helper function for app
  #'
  #' @return checks for mount
  #' @export verify_sshfs_mount
  verify_sshfs_mount <- function(watcher_dir) {
    if (values$config$verify_sshfs_mount == "") {
      return(TRUE)
    }
    fs = paste0("/", values$config$verify_sshfs_mount)

    if (!grepl(paste0(":", fs, " "),
               paste(system("mount 2>&1", intern=TRUE), collapse=" ")) |
        grepl("No such file",
              paste(system(paste0("ls ", watcher_dir, " 2>&1"), intern=TRUE), collapse=" "))) {
      shinyjs::showElement(id= "wellPanel_mountFail")
      showModal(modalDialog( title = paste0(fs, " mount not detected"), "Re-mount and try again" ))
      stopApp(1)
      return (FALSE)
    }

    shinyjs::hideElement(id= "wellPanel_mountFail")
    return(TRUE)
  }

  #' helper function for app
  #'
  #' @param selected_sample sampleid
  #' @param selected_sample_path facets run directory containing 'facets_review.manifest'
  #' @return nothing
  #' @export refresh_review_status
  refresh_review_status <- function(selected_sample, selected_sample_path, facets_runs) {
    review_df <- get_review_status(selected_sample, selected_sample_path)
    if ( dim(review_df)[1] > 0) {
      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

      output$datatable_fitReviews <- DT::renderDataTable({
        DT::datatable(facets_runs %>%
                        mutate(facets_qc = ifelse(facets_qc, gicon('ok'), gicon('remove'))) %>%
                        mutate(is_best_fit = ifelse(is_best_fit, gicon('thumbs-up'), '')) %>%
                        select(fit_name, facets_qc, facets_qc_version, manual_review_best_fit = is_best_fit) %>%
                        unique,
                      selection=list(mode='single'),
                      colnames = c('Fit', 'facets QC', 'facets QC ver.','Reviewed as best fit?'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                     pageLength = 100, dom='t'),
                      rownames=FALSE, escape = F)
      })

      output$datatable_reviewHistory <- DT::renderDataTable({
        DT::datatable(review_df %>%
                        filter(review_status != 'not_reviewed') %>%
                        mutate(use_only_purity_run = ifelse(use_only_purity_run, gicon('ok-sign'), '')) %>%
                        mutate(use_edited_cncf = ifelse(use_edited_cncf, gicon('ok-sign'), '')) %>%
                        mutate(facets_qc = ifelse(facets_qc, gicon('ok'), gicon('remove'))) %>%
                        dplyr::select(-sample, -path, -facets_suite_version) %>%
                        dplyr::arrange(desc(date_reviewed)) %>%
                        select(fit_name, review_status, facets_qc, facets_qc_version, review_notes,
                               reviewed_by, date_reviewed, use_only_purity_run, use_edited_cncf,
                               reviewer_set_purity),
                      selection=list(mode='single'),
                      colnames = c('Fit', 'Review Status', 'facets QC', 'facets QC ver.', 'Notes',
                                   'Reviewer', 'Date Reviewed', 'Use purity run only?',
                                   'Use edited.cncf.txt?', 'Reviewer set purity:'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets = 0:6)),
                                     pageLength = 100, dom = 't'),
                      rownames=FALSE, escape = F)
      })
    }
  }

  shinyjs::hideElement("button_saveChanges")



  observeEvent(input$reviewTabsetPanel, {
    if (input$reviewTabsetPanel == "cBioPortal") {

      #selected_sample <- paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected, 1]), collapse = "")
      selected_sample = paste(unlist(values$manifest_metadata$sample_id[values$manifest_metadata$sample_id %in% input$selectInput_selectSample]), collapse="")

      dmp_id <- (values$manifest_metadata %>% filter(sample_id == selected_sample))$dmp_id[1]

      url <- NULL
      if (!is.null(dmp_id) && !is.na(dmp_id)) {
        url <- paste0('https://cbioportal.mskcc.org/patient?studyId=mskimpact&caseId=', dmp_id)
      } else if (grepl('P\\-\\d{7}.*', selected_sample)) {
        patient_id <- gsub("\\-T.*", "", selected_sample)
        url <- paste0('https://cbioportal.mskcc.org/patient?studyId=mskimpact&caseId=', patient_id)
      } else {
        showModal(modalDialog(title = "Not a valid DMP ID", "Cannot open this sample in cBioPortal"))
        return()
      }

      # Send the URL to the client-side to open in the user's browser
      session$sendCustomMessage(type = 'openURL', message = url)
      updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")
    }
  })

  # --- Session env banner renderer ---
  observe({
    # no-op; keeps a spot if you later want reactivity
  })

  # Hide repo/edit sections in VM; keep visible in local
  observeEvent(TRUE, {
    if (!is_vm_mode()) return()

    # Hide the whole UI blocks (make sure you added these ids in ui.R)
    for (sec in c("section_tempo", "section_tcga", "section_refit")) {
      shinyjs::hide(sec)
    }

    # Disable all inputs inside those sections (belt & suspenders)
    to_disable <- c(
      # IMPACT
      "repository_path_impact", "remote_path_impact", "session_switch_impact",
      # TEMPO
      "repository_path_tempo",  "remote_path_tempo",  "session_switch_tempo",
      # TCGA
      "repository_path_tcga",   "remote_path_tcga",   "session_switch_tcga",
      # Refit
      "mount_refit_path", "remote_refit_path", "session_remote_refit"
    )
    lapply(to_disable, function(id) shinyjs::disable(id))

    # Ensure all mount switches are OFF visually too
    shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = FALSE)
    shinyWidgets::updateSwitchInput(session, "session_switch_tempo",  value = FALSE)
    shinyWidgets::updateSwitchInput(session, "session_switch_tcga",   value = FALSE)
    shinyWidgets::updateSwitchInput(session, "session_remote_refit",  value = FALSE)
  }, once = TRUE, ignoreInit = FALSE)


  output$fp_session_banner <- renderUI({
    null_if_empty <- function(x) if (nzchar(x)) x else "null"
    show_or_unset <- function(x) if (nzchar(x)) x else "(unset)"

    # env vars
    fp_mode              <- Sys.getenv("FP_MODE", "")
    fp_user_id           <- Sys.getenv("FP_USER_ID", "")
    fp_user_base_workdir <- Sys.getenv("FP_USER_BASE_WORKDIR", "")
    fp_user_workdir      <- Sys.getenv("FP_USER_WORKDIR", "")

    base_rows <- list(
      list(label = "FP_MODE",              value = null_if_empty(fp_mode)),
      list(label = "FP_USER_ID",           value = null_if_empty(fp_user_id)),
      list(label = "FP_USER_BASE_WORKDIR", value = null_if_empty(fp_user_base_workdir)),
      list(label = "FP_USER_WORKDIR",      value = null_if_empty(fp_user_workdir)),
      list(label = "fp_session_path()",    value = tryCatch(fp_session_path(),  error = function(e) "null")),
      list(label = "fp_personal_path()",   value = tryCatch(fp_personal_path(), error = function(e) "null"))
    )

    vm_rows <- list()
    if (is_vm_mode()) {
      defs <- get_vm_repo_defaults()
      # fall back to VM defaults if inputs are still blank at first render
      imp <- isolate(input$repository_path_impact); if (!nzchar(imp)) imp <- defs$impact %||% ""
      tcg <- isolate(input$repository_path_tcga);   if (!nzchar(tcg)) tcg <- defs$tcga   %||% ""
      tmp <- isolate(input$repository_path_tempo);  if (!nzchar(tmp)) tmp <- defs$tempo  %||% ""
      rft <- isolate(input$mount_refit_path) %||% ""

      vm_rows <- list(
        list(label = "IMPACT Path", value = show_or_unset(imp)),
        list(label = "TCGA Path",   value = show_or_unset(tcg)),
        list(label = "TEMPO Path",  value = show_or_unset(tmp)),
        list(label = "Refit Path",  value = show_or_unset(rft))
      )
    }

    rows <- c(base_rows, vm_rows)

    div(
      style = "margin-bottom:12px;padding:12px;border:1px solid #ddd;border-radius:8px;background:#f8f9fa;",
      tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:6px;",
               tags$strong(style="font-size:16px;", "Session")),
      tags$table(
        style="width:100%;border-collapse:collapse;font-family:monospace;font-size:12px;",
        tags$tbody(
          lapply(rows, function(r) {
            tags$tr(
              tags$td(style="width:220px;padding:2px 6px;color:#333;font-weight:600;", r$label),
              tags$td(style="padding:2px 6px;word-break:break-all;", r$value)
            )
          })
        )
      )
    )
  })





  observeEvent(input$button_repoSamplesInput, {

    if (!is_vm_mode() && is.null(values$selected_repo))  {
      showModal(modalDialog(title = "Failed",
                            paste0("No facets repository selected. Please choose one.")
      ))
      return(NULL)
    }

    # make sure the sample input string is the right format
    tumor_ids <- gsub(' |\\s|\\t', '', input$textAreaInput_repoSamplesInput)

    if (!grepl(values$selected_repo$tumor_id_format, tumor_ids)) {
      showModal(modalDialog(title = "Incorrect format!",
                            paste0("Tumor Sample IDs are in incorrect format. ",
                                   "Expecting one or more (comma-separated) IDs")
                            ))
      return(NULL)
    }
    values$loaded_time = Sys.time()

    tumor_ids <- unlist(strsplit(tumor_ids, ","))

    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")

    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)

    # local
    manifest_file <- values$selected_repo$manifest_file

    # VM override
    if (is_vm_mode()) {
      # Pull the root from the inputs/session_data/defaults, in that order
      impact_root <- input$remote_path_impact %||% session_data$remote_path_impact %||% get_vm_repo_defaults()$impact

      # Keep the same manifest file name the config uses, just relocate it under the VM root.
      mf <- basename(manifest_file %||% "facets_repo_manifest.txt")
      manifest_file <- file.path(impact_root, mf)
    }

    values$manifest_metadata <- load_repo_samples(tumor_ids, manifest_file, progress)

    num_samples_queried = length(tumor_ids)
    num_samples_found = nrow(values$manifest_metadata)
    if (num_samples_queried != num_samples_found) {
      showModal(modalDialog(title = "Warning!",
                            paste0("Note: Only ", num_samples_found, " of the ", num_samples_queried,
                                   " Tumor IDs queried are found in the respository.")
      ))
      if (num_samples_found == 0) {
        return(NULL)
      }
    }
    values$submitted_refits <- c()
  })

  observeEvent(input$button_samplesInput, {

    values$samples_ready <- FALSE
    values$samples_loading <- TRUE



    #print("button_samplesInput-1")

    # Get the input from textAreaInput_samplesInput and clean it up
    input_text <- input$textAreaInput_samplesInput

    #print(input_text)

    # If the input is empty, return early and do nothing
    if (is.null(input_text) || nzchar(trimws(input_text)) == FALSE) {
      values$samples_loading <- FALSE
      values$samples_ready <- FALSE
      return()  # Exit the function without doing anything
    }

    #print(lines)
    # Split the input by newline, space, tab, or comma
    lines <- unlist(strsplit(input_text, "[,\t \n]+"))

    # Remove any empty strings from the list of lines
    lines <- lines[nzchar(lines)]

    # If no valid lines remain after cleaning, return early
    if (length(lines) == 0) {
      values$samples_loading <- FALSE
      values$samples_ready <- FALSE
      return()
    }

    #print("button_samplesInput-2")


    # Initialize a list to store non-existing paths
    non_existing_paths <- list()

    # Check if each path exists, keep only the ones that exist
    existing_lines <- lines[sapply(lines, function(path) {
      if (dir.exists(path)) {
        return(TRUE)  # Keep the path if the folder exists
      } else {
        non_existing_paths <<- append(non_existing_paths, path)  # Add non-existing path to list
        return(FALSE)  # Exclude the path
      }
    })]

    #print("button_samplesInput-3")


    # If any non-existing paths were found, display a warning notification
    if (length(non_existing_paths) > 0) {
      showNotification(paste("The following paths do not exist and were not loaded:",
                             paste(non_existing_paths, collapse = ", ")),
                       type = "warning", duration = 8)
    }

    # If no valid paths remain after cleaning, return early
    if (length(existing_lines) == 0) {
      values$samples_loading <- FALSE
      values$samples_ready <- FALSE
      return()
    }

    #print("button_samplesInput-4")


    # Join cleaned lines with newline as separator
    cleaned_text <- paste(existing_lines, collapse = "\n")

    #print(list(list(className = 'dt-center', targets = 0:9)))

    # Reset selected repo and loaded time
    values$selected_repo <- NULL
    values$loaded_time <- Sys.time()

    #print("button_samplesInput-5")


    # Update the navbar to the "tabPanel_samplesManifest" tab
    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_samplesManifest")

    # Create and manage the progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Reading Samples:", value = 0)

    # Process the cleaned manifest (split again by newline just to ensure consistency)
    manifest <- unlist(stringr::str_split(cleaned_text, "\n"))

    #print("button_samplesInput-6")


    # Call the function to load the samples
    manifest_metadata <- load_samples(manifest, progress)
    #print("button_samplesInput-6.5")
    values$manifest_metadata <- manifest_metadata

    .log_df("values$manifest_metadata (assigned)", values$manifest_metadata)


    #print("button_samplesInput-7")


    # Reset submitted refits
    values$submitted_refits <- c()

    values$samples_loading <- FALSE
    values$samples_ready <- is.data.frame(values$manifest_metadata) && nrow(values$manifest_metadata) > 0
  })




  output$datatable_samples <- DT::renderDataTable({
    # While loading, show a static table (prevents Ajax error)
    if (isTRUE(values$samples_loading)) {
      return(
        DT::datatable(
          data.frame(Status = "Loading samples manifest…"),
          options = list(dom = 't', paging = FALSE),
          rownames = FALSE
        )
      )
    }

    # Wait until loader marked data as ready
    req(isTRUE(values$samples_ready))

    tryCatch({
      mm <- values$manifest_metadata
      .log_df("manifest_metadata (raw)", mm)

      # Guard: if empty, show guidance
      if (is.null(mm) || !is.data.frame(mm) || nrow(mm) == 0) {
        message("[DEBUG] manifest_metadata is empty/invalid; showing guidance table.")
        return(
          DT::datatable(
            data.frame(Info = "No samples to display. Add samples and click ‘Load Samples’."),
            options = list(dom = 't', paging = FALSE),
            rownames = FALSE
          )
        )
      }

      # Normalize column presence (use reviewed_date, not reviewed_fit_date)
      ensure_cols <- function(df, cols, default = NA) {
        for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- default
        df
      }
      if ("reviewed_fit_date" %in% names(mm) && !"reviewed_date" %in% names(mm)) {
        mm$reviewed_date <- mm$reviewed_fit_date
      }
      if ("reviewed_fit_date" %in% names(mm)) mm$reviewed_fit_date <- NULL

      mm <- ensure_cols(
        mm,
        c(
          "sample_id", "num_fits", "default_fit_name", "default_fit_qc",
          "review_status", "reviewed_fit_name", "reviewed_fit_facets_qc",
          "reviewed_fit_use_purity", "reviewed_fit_use_edited_cncf",
          "reviewer_set_purity", "reviewed_date",
          # droppable extras that sometimes appear:
          "path", "facets_suite_version", "facets_qc_version"
        )
      )

      safe_bool <- function(x) {
        y <- suppressWarnings(as.logical(x))
        ifelse(is.na(y), FALSE, y)
      }
      mm$default_fit_qc               <- safe_bool(mm$default_fit_qc)
      mm$reviewed_fit_facets_qc       <- safe_bool(mm$reviewed_fit_facets_qc)
      mm$reviewed_fit_use_purity      <- safe_bool(mm$reviewed_fit_use_purity)
      mm$reviewed_fit_use_edited_cncf <- safe_bool(mm$reviewed_fit_use_edited_cncf)

      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))

      # Drop known non-display columns if present
      drop_cols <- intersect(c("path", "facets_suite_version", "facets_qc_version"), names(mm))
      out <- mm[, setdiff(names(mm), drop_cols), drop = FALSE]

      # Format glyph columns
      out$default_fit_qc <- ifelse(out$default_fit_qc, gicon("ok"), gicon("remove"))
      out$reviewed_fit_facets_qc <- dplyr::case_when(
        is.na(out$review_status) | out$review_status == "Not reviewed" ~ "",
        TRUE ~ ifelse(out$reviewed_fit_facets_qc, gicon("ok"), gicon("remove"))
      )
      out$reviewed_fit_use_purity      <- ifelse(out$reviewed_fit_use_purity, gicon("ok-sign"), "")
      out$reviewed_fit_use_edited_cncf <- ifelse(out$reviewed_fit_use_edited_cncf, gicon("ok-sign"), "")

      # Enforce exact column order to match headers (prevents count mismatch)
      desired_cols <- c(
        "sample_id", "num_fits", "default_fit_name", "default_fit_qc",
        "review_status", "reviewed_fit_name", "reviewed_fit_facets_qc",
        "reviewed_fit_use_purity", "reviewed_fit_use_edited_cncf",
        "reviewer_set_purity", "reviewed_date"
      )
      out <- out[, desired_cols, drop = FALSE]
      .log_df("datatable_samples (final out)", out)

      DT::datatable(
        out,
        selection = list(
          mode = "single",
          selected = if (is.null(values$dt_sel)) NULL else values$dt_sel
        ),
        colnames = c(
          "Sample ID (tag)", "# fits", "Default Fit", "Default Fit QC",
          "Review Status", "Reviewed Fit", "Reviewed Fit QC", "purity run only?",
          "edited.cncf.txt?", "Reviewer purity", "Date Reviewed"
        ),
        options = list(
          pageLength = 20,
          columnDefs = list(list(className = "dt-center", targets = 0:9))
        ),
        rownames = FALSE,
        escape = FALSE,
        callback = .dt_error_callback   # <-- JS hook to report any DT error text
      )
    }, error = function(e) {
      message("[datatable_samples] ERROR (R): ", conditionMessage(e))
      DT::datatable(
        data.frame(Error = "Failed to render Samples Manifest. See server logs for details."),
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE
      )
    })
  })





  # Downloadable csv of selected dataset ----
  output$download_mapping_file <- downloadHandler(
    filename = function() {
      paste0('facets_mapping_file_', gsub(' |-|:', '_', Sys.time()), '.txt')
    },
    content = function(file) {

      elapsed_time = as.integer(difftime(Sys.time(), values$loaded_time, units = 'secs'))
      showModal(modalDialog( title = "Warning!",
                             paste0(elapsed_time,
                             " seconds have elapsed since reviews were loaded. To ensure capturing most recent reviews, ",
                             " load samples again from 'Load Samples' page")))

      write.table(values$manifest_metadata %>%
                  rowwise %>%
                  dplyr::mutate(has_reviewed_fit =
                           ifelse(review_status %in% c('reviewed_acceptable_fit','reviewed_best_fit'),
                                  T, F)) %>%
                  dplyr::mutate(run_type = ifelse(has_reviewed_fit & as.logical(reviewed_fit_use_purity),
                                                  'purity', 'hisens')) %>%
                  dplyr::mutate(fit_to_use = ifelse(has_reviewed_fit,
                                                    reviewed_fit_name, default_fit_name)) %>%
                  dplyr::mutate(cncf_file = paste0(path, '/', fit_to_use, '/', sample_id, '_', run_type, '.cncf',
                                                   ifelse(reviewed_fit_use_edited_cncf,
                                                          '.edited.txt', '.txt'))) %>%
                  select(-fit_to_use, -run_type, -has_reviewed_fit),
                file, row.names = F, quote=F, sep='\t')
    }
  )



  # Function to check if the given path is restricted
  is_restricted_path <- function(check_path) {
    # Check if the path contains any of the restricted paths
    any(sapply(restricted_paths, function(restricted) grepl(restricted, check_path)))
  }

  # Function to check if a given local path represents a remote file
  is_remote_file <- function(local_path) {
    if (is_vm_mode()) return(FALSE)  # no sshfs in VM

    # Get the mount information
    mount_df <- get_mount_info()

    # Check if the local path matches any of the local paths in the mount_df
    matched_row <- mount_df[sapply(mount_df$local_path, function(mount_local_path) {
      grepl(mount_local_path, local_path)
    }), ]

    # If matched_row is not empty, then the file represents a remote location
    return(nrow(matched_row) > 0)
  }

  # Function to get the remote path corresponding to a given local path
  get_remote_path <- function(local_path) {
    if (identical(Sys.getenv("FP_MODE", ""), "vm")) return(NULL)
    mount_df <- get_mount_info()
    matched_row <- mount_df[sapply(mount_df$local_path, function(mount_local_path) {
      grepl(mount_local_path, local_path)
    }), ]
    if (nrow(matched_row) > 0) {
      matched_local_path  <- matched_row$local_path[1]
      matched_remote_path <- matched_row$remote_path[1]
      return(gsub(matched_local_path, matched_remote_path, local_path))
    }
    NULL
  }

  get_local_path <- function(remote_path) {
    if (identical(Sys.getenv("FP_MODE", ""), "vm")) return(NULL)
    mount_df <- get_mount_info()
    matched_row <- mount_df[sapply(mount_df$remote_path, function(mount_remote_path) {
      grepl(mount_remote_path, remote_path)
    }), ]
    if (nrow(matched_row) > 0) {
      matched_remote_path <- matched_row$remote_path[1]
      matched_local_path  <- matched_row$local_path[1]
      return(gsub(matched_remote_path, matched_local_path, remote_path))
    }
    NULL
  }



  get_personal_path <- function(local_path) {
    # Get the personal storage base path from session_data
    personal_storage_path <- session_data$personal_storage_path

    # Ensure the personal storage path ends with a single trailing slash
    personal_storage_path <- sub("/+$", "", personal_storage_path)  # Remove any trailing slashes first

    # Extract the final directory name from the local_path
    final_directory_name <- basename(local_path)

    # Construct the full personal path
    personal_path <- file.path(personal_storage_path, final_directory_name)

    # Ensure the final path ends with a trailing slash
    personal_path <- paste0(personal_path, "/")

    return(personal_path)
  }

  get_remote_path_from_personal <- function(path_or_personal) {
    # Ensure the mapping file exists
    personal_repo_meta_file <- fp_personal_path()
    if (!file.exists(personal_repo_meta_file)) {
      create_personal_storage_file()
    }

    # Read mapping
    df_personal <- read.table(
      personal_repo_meta_file,
      sep = "\t", header = TRUE, stringsAsFactors = FALSE,
      na.strings = "", fill = TRUE
    )

    # Determine the personal path:
    # - If the arg is already under the personal root, use it directly
    # - Otherwise, derive the personal path from the provided sample path
    personal_root <- session_data$personal_storage_path %||% ""
    if (nzchar(personal_root) && startsWith(path_or_personal, personal_root)) {
      personal_path <- path_or_personal
    } else {
      personal_path <- get_personal_path(path_or_personal)
    }

    # Normalize trailing slash for comparison
    norm <- function(x) sub("/+$", "/", x)
    personal_path <- norm(personal_path)

    if (nrow(df_personal) == 0 || !"Personal" %in% names(df_personal)) return(NULL)

    rows <- which(norm(df_personal$Personal) == personal_path)
    if (length(rows) == 0) {
      # Silent miss; just return NULL (no noisy prints)
      return(NULL)
    }

    # Return the Local (i.e., repository/local) path that maps to this personal path
    df_personal$Local[rows[1]]
  }





  library(shiny)
  library(shinyjs)

  create_personal_storage_file <- function()
  {
    # Define the path to the personal repo meta file
    personal_repo_meta_file <- path.expand(fp_personal_path())

    # Check if the file exists; if not, create it
    if (!file.exists(personal_repo_meta_file)) {
      # Initialize an empty data frame with just the headers
      df <- data.frame(
        Remote = character(),
        Local = character(),
        Personal = character(),
        stringsAsFactors = FALSE
      )

      # Write the data frame with only headers to the file in tab-delimited format
      write.table(df, file = personal_repo_meta_file, sep = "\t", row.names = FALSE, col.names = TRUE)
    }
  }


  observeEvent(input$storageType, {

    if (ignore_storage_change()) {
      # Reset the flag and exit the observer without running handleSampleChange
      ignore_storage_change(FALSE)
      return()
    }
    ignore_storage_change(TRUE)

    # Get the selected sample path from values$manifest_metadata
    selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% input$selectInput_selectSample]), collapse = "")

    # Check if selected_sample_path is empty or NULL, and return if it is
    if (is.null(selected_sample_path) || selected_sample_path == "") {
      #print("Selected sample path is empty or NULL. Exiting function.")
      return()
    }

    # Check if storageType is Personal (FALSE) and if personal_storage_path is empty
    if (!input$storageType && session_data$personal_storage_path == "") {
      showNotification("Invalid personal repository configuration.", type = "error")

      # Set the value of storageType back to TRUE (Remote)
      updateSwitchInput(session, "storageType", value = TRUE)

      return()  # Exit function after resetting the storageType
    }

    #session_data$personal_storage_path

    if (input$storageType) {
      #print("Storage Type is set to Remote")

      # Get the personal path using the selected sample path
      personal_path <- get_personal_path(selected_sample_path)

      # Define the path to the personal repo meta file
      personal_repo_meta_file <- file.path(fp_personal_path())

      # Check if the .fp_personal.dat file exists
      if (file.exists(personal_repo_meta_file)) {
        # Read the .fp_personal.dat file
        df <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

        # Check if the personal path exists in the data frame
        existing_row <- which(df$Personal == personal_path)

        if (length(existing_row) > 0) {
          # Update the manifest metadata to local paths
          update_manifest_metadata_to_local(personal_path)

          # Update the text area and trigger the input message
          paths_list <- values$manifest_metadata$path
          updateTextAreaInput(session, "textAreaInput_samplesInput", value = paste(paths_list, collapse = "\n"))
          session$sendInputMessage("button_samplesInput", list(value = "trigger"))

          # Get the selected sample again after updating the manifest metadata
          selected_sample <- paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected, 1]), collapse = "")
          selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% selected_sample]), collapse = "")

          if (!skipSampleChange()) {
            handleSampleChange()
          }
        } else {
          print("No matching personal path found in .fp_personal.dat for switching to Remote")
        }
      } else {
        print(".fp_personal.dat file does not exist. Cannot switch to Remote.")
      }

    } else {
      #print("Storage Type is set to Personal")

      # Get the personal path using the selected sample path
      personal_path <- get_personal_path(selected_sample_path)

      # Get the remote path using the selected sample path
      remote_path <- get_remote_path(selected_sample_path)

      # Define the path to the personal repo meta file
      personal_repo_meta_file <- file.path(fp_personal_path())

      # Check if the file exists; if not, create it
      if (!file.exists(personal_repo_meta_file)) {

        # Initialize the data frame with the first entry
        df <- data.frame(
          Remote = remote_path,
          Local = selected_sample_path,
          Personal = personal_path,
          stringsAsFactors = FALSE
        )

        # Write the data frame to the file in tab-delimited format
        write.table(df, file = personal_repo_meta_file, sep = "\t", row.names = FALSE, col.names = TRUE)
      }

      # Read the .fp_personal.dat file (whether it was just created or already existed)
      df <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

      # Check if the personal path already exists in the data frame
      existing_row <- which(df$Personal == personal_path)

      if (length(existing_row) > 0) {
        # If the personal path exists, update the Remote and Local paths if they have changed
        #print("Personal path exists, updating Remote and Local paths if needed")

        # Check if remote_path and selected_sample_path are valid
        if (!is.null(remote_path) && nzchar(remote_path) &&
            !is.null(selected_sample_path) && nzchar(selected_sample_path)) {

          if (df$Remote[existing_row] != remote_path || df$Local[existing_row] != selected_sample_path) {
            df$Remote[existing_row] <- remote_path
            df$Local[existing_row] <- selected_sample_path
            #print("Updated existing entry in .fp_personal.dat")
          }
        } else {
          print("Error: remote_path or selected_sample_path is invalid.")
        }
      } else {
        # If the personal path does not exist, add a new entry

        new_entry <- data.frame(
          Remote = remote_path,
          Local = selected_sample_path,
          Personal = personal_path,
          stringsAsFactors = FALSE
        )

        # Add the new entry to the data frame
        df <- rbind(df, new_entry)
      }

      # Remove any rows with NA in all columns (just in case)
      df <- df[complete.cases(df), ]

      # Write the updated data frame back to the file
      write.table(df, file = personal_repo_meta_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

      # Ensure the personal directory exists
      if (!dir.exists(personal_path)) {
        dir.create(personal_path, recursive = TRUE)
        #print(paste("Created directory:", personal_path))  # Debugging message
      }

      # Get the list of directories in the personal storage path
      directories <- list.dirs(session_data$personal_storage_path, full.names = TRUE, recursive = FALSE)

      # Check if there are 50 or more directories
      if (length(directories) >= 50) {
        # Display a modal error message
        showModal(modalDialog(
          title = "Error",
          "Your personal repository is at maximum capacity. ",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        shinyWidgets::updateSwitchInput(session, "storageType", value = TRUE)
        return()  # Exit the observer since no further action should be taken
      }

      # Get the list of files in the selected_sample_path directory, ignoring .dat.gz files
      files_to_copy <- list.files(selected_sample_path, full.names = TRUE, recursive = TRUE)
      files_to_copy <- files_to_copy[!grepl("\\.dat\\.gz$", files_to_copy)]

      # Use a progress bar to show the copying process
      shiny::withProgress(message = 'Copying new files...', value = 0, {
        total_files <- length(files_to_copy)
        files_copied <- 0
        if (total_files > 0) {
          for (i in seq_along(files_to_copy)) {
            # Construct the destination path for the file
            destination_file <- file.path(personal_path, sub(selected_sample_path, "", files_to_copy[i]))

            # Only copy the file if it doesn't already exist at the destination
            if (!file.exists(destination_file)) {
              dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)
              file.copy(files_to_copy[i], destination_file)
              files_copied <- files_copied + 1
            }
            incProgress(1 / total_files)
          }
          #print(paste("Copied", files_copied, "new files to:", personal_path))
        } else {
          print("No files to copy.")
        }
      })

      #print(paste("Personal Path:", personal_path))

      # Update manifest metadata using the new personal path
      update_manifest_metadata(personal_path)

      paths_list <- values$manifest_metadata$path
      updateTextAreaInput(session, "textAreaInput_samplesInput", value = paste(paths_list, collapse = "\n"))
      session$sendInputMessage("button_samplesInput", list(value = "trigger"))

      # Get the selected sample again after updating the manifest metadata
      selected_sample <- paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected, 1]), collapse = "")
      selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% selected_sample]), collapse = "")

      if (!skipSampleChange()) {
        handleSampleChange()
      }

    }
    ignore_storage_change(FALSE)
  })

  observeEvent(input$storageType_compare, {
    if (ignore_storage_change_compare()) {
      # Reset the flag and exit the observer without running handleSampleChange
      ignore_storage_change_compare(FALSE)
      return()
    }
    ignore_storage_change_compare(TRUE)
    #print("ToggleStorage2")

    # Get the selected sample path from values$manifest_metadata
    selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% input$selectInput_selectSample_compare]), collapse = "")

    # Check if selected_sample_path is empty or NULL, and return if it is
    if (is.null(selected_sample_path) || selected_sample_path == "") {
      #print("Selected sample path is empty or NULL. Exiting function.")
      return()
    }

    # Check if storageType is Personal (FALSE) and if personal_storage_path is empty
    if (!input$storageType_compare && session_data$personal_storage_path == "") {
      showNotification("Invalid personal repository configuration.", type = "error")

      # Set the value of storageType back to TRUE (Remote)
      updateSwitchInput(session, "storageType_compare", value = TRUE)

      return()
    }


    if (input$storageType_compare) {
      #print("Storage Type is set to Remote")

      # Get the personal path using the selected sample path
      personal_path <- get_personal_path(selected_sample_path)

      # Define the path to the personal repo meta file
      personal_repo_meta_file <- file.path(fp_personal_path())

      # Check if the .fp_personal.dat file exists
      if (file.exists(personal_repo_meta_file)) {
        # Read the .fp_personal.dat file
        df <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

        # Check if the personal path exists in the data frame
        existing_row <- which(df$Personal == personal_path)

        if (length(existing_row) > 0) {
          # Update the manifest metadata to local paths
          update_manifest_metadata_to_local(personal_path)

          # Update the text area and trigger the input message
          paths_list <- values$manifest_metadata$path
          updateTextAreaInput(session, "textAreaInput_samplesInput", value = paste(paths_list, collapse = "\n"))
          session$sendInputMessage("button_samplesInput", list(value = "trigger"))

          # Get the selected sample again after updating the manifest metadata
          selected_sample <- paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected, 1]), collapse = "")
          selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% selected_sample]), collapse = "")

          if (!skipSampleChange()) {
            handleSampleChange_compare()
          }
        } else {
          print("No matching personal path found in .fp_personal.dat for switching to Remote")
        }
      } else {
        print(".fp_personal.dat file does not exist. Cannot switch to Remote.")
      }

    } else {
      #print("Storage Type is set to Personal")

      # Get the personal path using the selected sample path
      personal_path <- get_personal_path(selected_sample_path)

      # Get the remote path using the selected sample path
      remote_path <- get_remote_path(selected_sample_path)

      # Define the path to the personal repo meta file
      personal_repo_meta_file <- file.path(fp_personal_path())

      # Check if the file exists; if not, create it
      if (!file.exists(personal_repo_meta_file)) {

        # Initialize the data frame with the first entry
        df <- data.frame(
          Remote = remote_path,
          Local = selected_sample_path,
          Personal = personal_path,
          stringsAsFactors = FALSE
        )

        # Write the data frame to the file in tab-delimited format
        write.table(df, file = personal_repo_meta_file, sep = "\t", row.names = FALSE, col.names = TRUE)
      }

      # Read the .fp_personal.dat file (whether it was just created or already existed)
      df <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

      # Check if the personal path already exists in the data frame
      existing_row <- which(df$Personal == personal_path)

      if (length(existing_row) > 0) {
        # If the personal path exists, update the Remote and Local paths if they have changed
        #print("Personal path exists, updating Remote and Local paths if needed")

        # Check if remote_path and selected_sample_path are valid
        if (!is.null(remote_path) && nzchar(remote_path) &&
            !is.null(selected_sample_path) && nzchar(selected_sample_path)) {

          if (df$Remote[existing_row] != remote_path || df$Local[existing_row] != selected_sample_path) {
            df$Remote[existing_row] <- remote_path
            df$Local[existing_row] <- selected_sample_path
            #print("Updated existing entry in .fp_personal.dat")
          }
        } else {
          print("Error: remote_path or selected_sample_path is invalid.")
        }
      } else {
        # If the personal path does not exist, add a new entry
        #print("Adding new entry to .fp_personal.dat")

        new_entry <- data.frame(
          Remote = remote_path,
          Local = selected_sample_path,
          Personal = personal_path,
          stringsAsFactors = FALSE
        )

        # Add the new entry to the data frame
        df <- rbind(df, new_entry)
      }

      # Remove any rows with NA in all columns (just in case)
      df <- df[complete.cases(df), ]

      # Write the updated data frame back to the file
      write.table(df, file = personal_repo_meta_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

      # Ensure the personal directory exists
      if (!dir.exists(personal_path)) {
        dir.create(personal_path, recursive = TRUE)
        #print(paste("Created directory:", personal_path))
      }

      # Get the list of directories in the personal storage path
      directories <- list.dirs(session_data$personal_storage_path, full.names = TRUE, recursive = FALSE)

      # Check if there are 50 or more directories
      if (length(directories) >= 50) {
        # Display a modal error message
        showModal(modalDialog(
          title = "Error",
          "Your personal repository is at maximum capacity. ",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        shinyWidgets::updateSwitchInput(session, "storageType_compare", value = TRUE)
        return()  # Exit the observer since no further action should be taken
      }

      # Get the list of files in the selected_sample_path directory, ignoring .dat.gz files
      files_to_copy <- list.files(selected_sample_path, full.names = TRUE, recursive = TRUE)
      files_to_copy <- files_to_copy[!grepl("\\.dat\\.gz$", files_to_copy)]

      # Use a progress bar to show the copying process
      shiny::withProgress(message = 'Copying new files...', value = 0, {
        total_files <- length(files_to_copy)
        files_copied <- 0
        if (total_files > 0) {
          for (i in seq_along(files_to_copy)) {
            # Construct the destination path for the file
            destination_file <- file.path(personal_path, sub(selected_sample_path, "", files_to_copy[i]))

            # Only copy the file if it doesn't already exist at the destination
            if (!file.exists(destination_file)) {
              dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)
              file.copy(files_to_copy[i], destination_file)
              files_copied <- files_copied + 1
            }
            incProgress(1 / total_files)
          }
          #print(paste("Copied", files_copied, "new files to:", personal_path))
        } else {
          print("No files to copy.")
        }
      })

      #print(paste("Personal Path:", personal_path))

      # Update manifest metadata using the new personal path
      update_manifest_metadata(personal_path)

      paths_list <- values$manifest_metadata$path
      updateTextAreaInput(session, "textAreaInput_samplesInput_compare", value = paste(paths_list, collapse = "\n"))
      session$sendInputMessage("button_samplesInput", list(value = "trigger"))

      # Get the selected sample again after updating the manifest metadata
      selected_sample <- paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected, 1]), collapse = "")
      selected_sample_path <- paste(unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% selected_sample]), collapse = "")

      if (!skipSampleChange()) {
        handleSampleChange_compare()
      }
    }
    ignore_storage_change_compare(FALSE)
  })


  update_manifest_metadata <- function(personal_path) {
    #print("Updating manifest metadata with personal paths")

    # Ensure the .fp_personal.dat file exists
    personal_repo_meta_file <- file.path(fp_personal_path())
    if (!file.exists(personal_repo_meta_file)) {
      create_personal_storage_file()
      #stop(".fp_personal.dat file does not exist. Cannot update manifest metadata.")
    }

    # Read the .fp_personal.dat file into a data frame
    df_personal <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

    # Find the row in .fp_personal.dat where Personal matches the provided personal_path
    matching_row <- df_personal[df_personal$Personal == personal_path, ]

    if (nrow(matching_row) == 0) {
      print(paste("No matching personal path found in .fp_personal.dat for", personal_path))
      return()
    }

    # Get the corresponding Local path
    local_path <- matching_row$Local

    # Get the current manifest metadata
    manifest_metadata <- values$manifest_metadata

    # Find and update the manifest path where it matches the Local path
    for (i in seq_len(nrow(manifest_metadata))) {
      if (manifest_metadata$path[i] == local_path) {
        # Replace the local path with the personal path
        manifest_metadata$path[i] <- personal_path
        #print(paste("Replaced", local_path, "with", personal_path))
      }
    }

    # Update the values$manifest_metadata with the modified data
    values$manifest_metadata <- manifest_metadata

    #print("Updated manifest metadata:")
    #print(values$manifest_metadata)
  }


  update_manifest_metadata_to_local <- function(personal_path) {
    #print("Updating manifest metadata with local paths")

    # Ensure the .fp_personal.dat file exists
    personal_repo_meta_file <- file.path(fp_personal_path())
    if (!file.exists(personal_repo_meta_file)) {
      create_personal_storage_file()
      #stop(".fp_personal.dat file does not exist. Cannot update manifest metadata.")
    }

    # Read the .fp_personal.dat file into a data frame
    df_personal <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, na.strings = "", fill = TRUE)

    # Find the row in .fp_personal.dat where Personal matches the provided personal_path
    matching_row <- df_personal[df_personal$Personal == personal_path, ]

    if (nrow(matching_row) == 0) {
      print(paste("No matching personal path found in .fp_personal.dat for", personal_path))
      return()
    }

    # Get the corresponding Local path
    local_path <- matching_row$Local

    # Get the current manifest metadata
    manifest_metadata <- values$manifest_metadata

    # Find and update the manifest path where it matches the Personal path
    for (i in seq_len(nrow(manifest_metadata))) {
      if (manifest_metadata$path[i] == personal_path) {
        # Replace the personal path with the local path
        manifest_metadata$path[i] <- local_path
        #print(paste("Replaced", personal_path, "with", local_path))
      }
    }

    # Update the values$manifest_metadata with the modified data
    values$manifest_metadata <- manifest_metadata

    #print("Updated manifest metadata:")
    #print(values$manifest_metadata)
  }








  observeEvent(input$datatable_samples_rows_selected, {

    skipSampleChange(TRUE)

    selected_sample = paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected,1]), collapse="")
    selected_sample_path = paste(unlist(values$manifest_metadata[input$datatable_samples_rows_selected,2]), collapse="")
    selected_sample_num_fits = values$manifest_metadata[input$datatable_samples_rows_selected,4]

    # VM: do not do any mount/mapping logic — just use the path as-is
    if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
      # Navigate to Review Fits (keep this tab id consistent across app)
      updateNavbarPage(session, "navbarPage1", selected = "tabPanel_reviewFits")
      updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")

      # Load runs directly from the provided path (no mapping in VM)
      progress <- shiny::Progress$new(min = 1, max = 4)
      on.exit(progress$close(), add = TRUE)
      progress$set(message = "Loading data", value = 1)

      values$sample_runs         <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
      values$sample_runs_compare <- values$sample_runs

      if (is.null(values$sample_runs) || nrow(values$sample_runs) == 0) {
        showModal(modalDialog(
          title = "No runs detected",
          paste0(
            "Could not find any runs under:\n\n",
            selected_sample_path,
            "\n\nCheck that the path contains FACETS outputs and a facets_review.manifest."
          ),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # Update review status table (args: sample_id, sample_path, runs_df)
      refresh_review_status(selected_sample, selected_sample_path, values$sample_runs)

      # ----------- Populate the dropdowns (this fixes the empty selects) -----------
      # All sample ids from the current manifest table
      mm <- values$manifest_metadata
      filtered_sample_id <- mm$sample_id[!is.na(mm$sample_id) & nzchar(mm$sample_id)]

      updateSelectInput(
        session, "selectInput_selectSample",
        choices  = as.list(filtered_sample_id),
        selected = selected_sample
      )
      updateSelectInput(
        session, "selectInput_selectSample_compare",
        choices  = as.list(filtered_sample_id),
        selected = selected_sample
      )

      # Fit choices from the loaded runs
      fit_names   <- unique(values$sample_runs$fit_name)
      fit_choices <- as.list(c("Not selected", fit_names))
      default_sel <- if ("default" %in% fit_names) "default" else "Not selected"

      updateSelectInput(
        session, "selectInput_selectFit",
        choices  = fit_choices,
        selected = default_sel
      )
      updateSelectInput(
        session, "selectInput_selectFit_compare",
        choices  = fit_choices,
        selected = default_sel
      )

      # Keep internal “current selection” mirrors in sync (if you use them later)
      values$show_fit         <- default_sel
      values$show_fit_compare <- default_sel

      # Prevent any legacy mount logic from executing below in this observer
      matched_row <- data.frame()

      # Default to REMOTE in VM
      shinyWidgets::updateSwitchInput(session, "storageType", value = TRUE)
      shinyWidgets::updateSwitchInput(session, "storageType_compare", value = TRUE)

      skipSampleChange(FALSE)

      return(NULL)
    }




    if (selected_sample_num_fits == 0) {
      showModal(modalDialog( title = "No fits found for this sample",
                             "Path to this sample may be incorrect. " ))
      return(NULL)  # print some kind of error and exit;
    }

    updateNavbarPage(session, "navbarPage1", selected = "tabPanel_reviewFits")
    updateTabsetPanel(session, "reviewTabsetPanel", selected = "png_image_tabset")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)

    #print("LoadingSample")

    if (!identical(Sys.getenv("FP_MODE", ""), "vm") &&
        is.null(attr(values$sample_runs, "vm_loaded"))) {

      # (keep your existing mount logic as-is)
      mount_df <- get_mount_info()
      matched_row <- mount_df[sapply(mount_df$local_path, function(local_path) {
        grepl(local_path, selected_sample_path)
      }), ]

      if (nrow(matched_row) > 0) {
        if (is_restricted_path(matched_row$remote_path)) {
          if (session_data$password_valid == 1 || !is_remote_file(selected_sample_path)) {
            showNotification("You are authorized to make changes to this sample.", type = "message")
          } else {
            showNotification("You are not authorized to perform refits or reviews for this sample. Authenticate on the session tab to unlock.", type = "error")
          }
        }
        values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
        values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
      } else {
        showNotification("You are authorized to make changes to this sample.", type = "message")
        values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress)
        values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress)
      }
    }


    #print("matchedRows")

    #Hide/show refit box when necessary.
    observe({
      if ((session_data$password_personal == 1 && !input$storageType) ||
          session_data$password_valid == 1 ||
          (is.null(get_remote_path(selected_sample_path)) ||
           is_remote_file(selected_sample_path) &&
           !is_restricted_path(get_remote_path(selected_sample_path)))) {
        shinyjs::show("fitPanel")
        if(input$session_remote_refit)
        {
          shinyjs::show("use_remote_refit_switch")
          shinyjs::show("remote_refit_options")
          shinyWidgets::updateSwitchInput(session, "use_remote_refit_switch", value = TRUE)
        }
        else
        {
          shinyjs::hide("use_remote_refit_switch")
          shinyjs::hide("remote_refit_options")
          shinyWidgets::updateSwitchInput(session, "use_remote_refit_switch", value = FALSE)
        }
      } else {
        shinyjs::hide("fitPanel")
      }
    })

    #Hide/show the remote/local storage box when necessary.
    observe({
      # Check if the selected_sample_path is valid and if it's a remote file
      if (!is.null(selected_sample_path)) {
        # Compute the corresponding personal path for this sample
        personal_path_for_selected <- get_personal_path(selected_sample_path)

        if ((is_remote_file(selected_sample_path) ||
             !is.null(get_remote_path_from_personal(personal_path_for_selected))) &&
            session_data$password_personal == 1) {
          shinyjs::show("storageTypeDiv")
          shinyjs::show("storageTypeDiv_compare")
          if (is_remote_file(selected_sample_path)) {
            shinyWidgets::updateSwitchInput(session, "storageType", value = TRUE)
            shinyWidgets::updateSwitchInput(session, "storageType_compare", value = TRUE)
          } else {
            shinyWidgets::updateSwitchInput(session, "storageType", value = FALSE)
            shinyWidgets::updateSwitchInput(session, "storageType_compare", value = FALSE)
          }
        } else {
          shinyjs::hide("storageTypeDiv")
          shinyjs::hide("storageTypeDiv_compare")
        }
      }

    })

    #print("matchRow2")

    if (nrow(matched_row) > 0) {
      # Check if the remote_path contains "/juno/work/"
      if (is_restricted_path(matched_row$remote_path)) {
        if (session_data$password_valid == 1 || !is_remote_file(selected_sample_path)) {
          showNotification("You are authorized to make changes to this sample.", type = "message")
        } else {
          showNotification("You are not authorized to perform refits or reviews for this sample. Authenticate on the session tab to unlock.", type = "error")
        }
      }
      values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
      values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
    } else {
      showNotification("You are authorized to make changes to this sample.", type = "message")
      values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress)
      values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress)
    }

    #print("matchRow3")

    output$verbatimTextOutput_runParams <- renderText({})
    output$verbatimTextOutput_runParams_compare <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)

    # update with review status
    refresh_review_status(selected_sample, selected_sample_path, values$sample_runs)

    ## get best fit if exists; other-wise default
    selected_run = values$sample_runs %>% filter(fit_name=='default') %>% head(n=1)

    if (nrow(values$sample_runs %>% filter(is_best_fit)) == 1) {
      selected_run = values$sample_runs %>% filter(is_best_fit) %>% head(n=1)
    } else {
      default_fit = (values$manifest_metadata %>% filter(sample_id == selected_sample))$default_fit_name
      selected_run = values$sample_runs %>% filter(fit_name==default_fit) %>% head(n=1)
    }

    ## hack around reactive to toggle to selected_run$fit_name
    values$show_fit = ifelse(nrow(selected_run) == 0, 'Not selected', selected_run$fit_name)
    values$show_fit_compare = ifelse(nrow(selected_run) == 0, 'Not selected', selected_run$fit_name)

    ## bind to drop-down
    updateSelectInput(session, "selectInput_selectFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = ifelse (input$selectInput_selectFit == 'Not selected' & values$show_fit != 'Not selected',
                                         values$show_fit, 'Not selected')
    )

    updateSelectInput(session, "selectInput_selectFit_compare",
                      choices = as.list(c("Not selected", unlist(values$sample_runs_compare$fit_name))),
                      selected = ifelse (input$selectInput_selectFit_compare == 'Not selected' & values$show_fit_compare != 'Not selected',
                                         values$show_fit_compare, 'Not selected')
    )


    # Filter out any NA or empty strings from the list of sample IDs
    filtered_sample_id <- values$manifest_metadata$sample_id[!is.na(values$manifest_metadata$sample_id) & values$manifest_metadata$sample_id != ""]

    # Update the selectInput with the filtered list of choices
    updateSelectInput(
      session,
      "selectInput_selectSample",
      choices = as.list(unlist(filtered_sample_id)),
      selected = selected_sample
    )

    updateSelectInput(
      session,
      "selectInput_selectSample_compare",
      choices = as.list(unlist(filtered_sample_id)),
      selected = selected_sample
    )

    updateSelectInput(session, "selectInput_selectBestFit",
                      choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                      selected = "Not selected"
    )

    set_default_countFile()


    if (nrow(selected_run) > 0) {
      updateTextInput(session, "textInput_newDipLogR", label = NULL, value = "")
      updateTextInput(session, "textInput_newPurityCval", label = NULL, value = selected_run$purity_run_cval)
      updateTextInput(session, "textInput_newHisensCval", label = NULL, value = selected_run$hisens_run_cval)
      updateTextInput(session, "textInput_newPurityMinNHet", label = NULL, value = selected_run$purity_run_nhet)
      updateTextInput(session, "textInput_newHisensMinNHet", label = NULL, value = selected_run$hisens_run_nhet)
      updateTextInput(session, "textInput_newSnpWindowSize", label = NULL, value = selected_run$purity_run_snp_nbhd)
      updateTextInput(session, "textInput_newNormalDepth", label = NULL, value = selected_run$purity_run_ndepth)
      updateSelectInput(session, "selectInput_newFacetsLib",
                        choices = as.list(values$config$facets_lib$version),
                        selected = selected_run$purity_run_version)
    }

    shinyjs::delay(2500, {  # Adding some delay here because if we turn this off too soon the sample input field is still updating and it will double-load data.
      skipSampleChange(FALSE)
    })
  })


  observeEvent(input$button_copyClipPath, {
    if (input$selectInput_selectFit == "Not selected"){
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    clip <- pipe("pbcopy", "w")
    write.table(paste0(selected_run$path[1], "/", selected_run$fit_name[1], "/"),
                file=clip,
                quote=F,
                col.names=F,
                row.names=F,
                eol = '')
    close(clip)
  })

  observeEvent(input$link_advancedOptions, {
    shinyjs::toggleElement(id='wellPanel_advancedOptions')
  })


  observeEvent(input$selectInput_selectSample, {
    if (identical(Sys.getenv("FP_MODE", ""), "vm") || !skipSampleChange()) {
      handleSampleChange()
    }
  })



  handleSampleChange <- function() {
    # Figure out the selected sample + path from the manifest table
    selected_sample <- paste(
      unlist(values$manifest_metadata$sample_id[values$manifest_metadata$sample_id %in% input$selectInput_selectSample]),
      collapse = ""
    )
    selected_sample_path <- paste(
      unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% input$selectInput_selectSample]),
      collapse = ""
    )

    # Always (re)load runs for the newly selected sample
    progress <- shiny::Progress$new(); on.exit(progress$close(), add = TRUE)
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)

    if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
      # VM: load directly, no mount/mapping
      values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
    } else {
      # Local: preserve mount/mapping behavior
      mount_df <- get_mount_info()
      matched_row <- mount_df[sapply(mount_df$local_path, function(local_path) {
        grepl(local_path, selected_sample_path)
      }), ]
      if (nrow(matched_row) > 0) {
        values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
      } else {
        values$sample_runs <- metadata_init(selected_sample, selected_sample_path, progress)
      }
    }

    # If no runs, stop gracefully
    if (is.null(values$sample_runs) || nrow(values$sample_runs) == 0) return(NULL)

    # Keep storage type switch in a sane state (personal mapping file may be absent)
    personal_repo_meta_file <- file.path(fp_personal_path())
    if (!file.exists(personal_repo_meta_file)) {
      create_personal_storage_file()
      shinyWidgets::updateSwitchInput(session, "storageType", value = TRUE)
    } else {
      df_personal <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE,
                                stringsAsFactors = FALSE, na.strings = "", fill = TRUE)
      in_personal <- selected_sample_path %in% df_personal$Personal
      shinyWidgets::updateSwitchInput(session, "storageType", value = !isTRUE(in_personal == TRUE))
    }

    # Update review status & UI
    refresh_review_status(selected_sample, selected_sample_path, values$sample_runs)

    # Choose a starting fit (best > default > Not selected)
    selected_run <- values$sample_runs %>% dplyr::filter(fit_name == "default") %>% head(n = 1)
    if (nrow(values$sample_runs %>% dplyr::filter(is_best_fit)) == 1) {
      selected_run <- values$sample_runs %>% dplyr::filter(is_best_fit) %>% head(n = 1)
    } else {
      default_fit <- (values$manifest_metadata %>% dplyr::filter(sample_id == selected_sample))$default_fit_name
      selected_run <- values$sample_runs %>% dplyr::filter(fit_name == default_fit) %>% head(n = 1)
    }

    values$show_fit <- ifelse(nrow(selected_run) == 0, "Not selected", selected_run$fit_name)

    updateSelectInput(
      session, "selectInput_selectFit",
      choices  = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
      selected = ifelse(input$selectInput_selectFit == "Not selected" && values$show_fit != "Not selected",
                        values$show_fit, "Not selected")
    )

    updateSelectInput(
      session, "selectInput_selectBestFit",
      choices  = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
      selected = "Not selected"
    )

    # Keep the sample dropdowns in sync (choices already set elsewhere)
    # Re-apply parameter fields if we have a selected run
    if (nrow(selected_run) > 0) {
      updateTextInput(session, "textInput_newDipLogR", label = NULL, value = "")
      updateTextInput(session, "textInput_newPurityCval",  label = NULL, value = selected_run$purity_run_cval)
      updateTextInput(session, "textInput_newHisensCval",  label = NULL, value = selected_run$hisens_run_cval)
      updateTextInput(session, "textInput_newPurityMinNHet", label = NULL, value = selected_run$purity_run_nhet)
      updateTextInput(session, "textInput_newHisensMinNHet", label = NULL, value = selected_run$hisens_run_nhet)
      updateTextInput(session, "textInput_newSnpWindowSize", label = NULL, value = selected_run$purity_run_snp_nbhd)
      updateTextInput(session, "textInput_newNormalDepth",   label = NULL, value = selected_run$purity_run_ndepth)
      updateSelectInput(session, "selectInput_newFacetsLib",
                        choices  = as.list(values$config$facets_lib$version),
                        selected = selected_run$purity_run_version)
    }

    # Make sure the fit panel visibility reflects perms/mode
    output$verbatimTextOutput_runParams <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src = "", width = 0, height = 0) }, deleteFile = FALSE)

    # Minimal show/hide logic untouched (same as your current observer’s intent)
    # – we’re leaving your existing show/hide observers as-is

    set_default_countFile()
  }




  observeEvent(input$selectInput_selectSample_compare, {
    if (identical(Sys.getenv("FP_MODE", ""), "vm") || !skipSampleChange()) {
      handleSampleChange_compare()
    }
  })



  handleSampleChange_compare <- function() {
    selected_sample <- paste(
      unlist(values$manifest_metadata$sample_id[values$manifest_metadata$sample_id %in% input$selectInput_selectSample_compare]),
      collapse = ""
    )
    selected_sample_path <- paste(
      unlist(values$manifest_metadata$path[values$manifest_metadata$sample_id %in% input$selectInput_selectSample_compare]),
      collapse = ""
    )

    # Always (re)load runs for the compare sample
    progress <- shiny::Progress$new(); on.exit(progress$close(), add = TRUE)
    progress$set(message = "Loading FACETS runs for the selected sample:", value = 0)

    if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
      values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
    } else {
      mount_df <- get_mount_info()
      matched_row <- mount_df[sapply(mount_df$local_path, function(local_path) {
        grepl(local_path, selected_sample_path)
      }), ]
      if (nrow(matched_row) > 0) {
        values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress, FALSE)
      } else {
        values$sample_runs_compare <- metadata_init(selected_sample, selected_sample_path, progress)
      }
    }

    if (is.null(values$sample_runs_compare) || nrow(values$sample_runs_compare) == 0) return(NULL)

    # Storage switch handling (mirrors base sample logic, but non-blocking)
    personal_repo_meta_file <- file.path(fp_personal_path())
    if (!file.exists(personal_repo_meta_file)) {
      create_personal_storage_file()
      shinyWidgets::updateSwitchInput(session, "storageType_compare", value = TRUE)
    } else {
      df_personal <- read.table(personal_repo_meta_file, sep = "\t", header = TRUE,
                                stringsAsFactors = FALSE, na.strings = "", fill = TRUE)
      in_personal <- selected_sample_path %in% df_personal$Personal
      shinyWidgets::updateSwitchInput(session, "storageType_compare", value = !isTRUE(in_personal == TRUE))
    }

    # Update review status table for compare set
    refresh_review_status(selected_sample, selected_sample_path, values$sample_runs_compare)

    # Choose a starting fit (best > default > Not selected)
    selected_run <- values$sample_runs_compare %>% dplyr::filter(fit_name == "default") %>% head(n = 1)
    if (nrow(values$sample_runs_compare %>% dplyr::filter(is_best_fit)) == 1) {
      selected_run <- values$sample_runs_compare %>% dplyr::filter(is_best_fit) %>% head(n = 1)
    } else {
      default_fit <- (values$manifest_metadata %>% dplyr::filter(sample_id == selected_sample))$default_fit_name
      selected_run <- values$sample_runs_compare %>% dplyr::filter(fit_name == default_fit) %>% head(n = 1)
    }

    values$show_fit_compare <- ifelse(nrow(selected_run) == 0, "Not selected", selected_run$fit_name)

    updateSelectInput(
      session, "selectInput_selectFit_compare",
      choices  = as.list(c("Not selected", unlist(values$sample_runs_compare$fit_name))),
      selected = ifelse(input$selectInput_selectFit_compare == "Not selected" && values$show_fit_compare != "Not selected",
                        values$show_fit_compare, "Not selected")
    )

    updateSelectInput(
      session, "selectInput_selectBestFit_compare",
      choices  = as.list(c("Not selected", unlist(values$sample_runs_compare$fit_name))),
      selected = "Not selected"
    )

    output$verbatimTextOutput_runParams_compare <- renderText({})
    output$imageOutput_pngImage2 <- renderImage({ list(src = "", width = 0, height = 0) }, deleteFile = FALSE)

    # leave the rest of your UI/render logic as-is
  }


  observeEvent(input$selectInput_selectFit, {

    output$verbatimTextOutput_runParams <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)

    if ( input$selectInput_selectFit == 'Not selected') {
      if (!is.null(values$show_fit) && values$show_fit != '') {
        updateSelectInput(session, "selectInput_selectFit",
                          choices = as.list(c("Not selected", unlist(values$sample_runs$fit_name))),
                          selected = values$show_fit
        )
        values$show_fit = ''
      }
      return (NULL)
    }

    # update other text options
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    if ( selected_run$is_best_fit[1]) {
      shinyjs::showElement(id="div_bestFitTrophy", anim = TRUE, animType = "fade", time = 0.5)
    } else {
      shinyjs::hideElement(id="div_bestFitTrophy", anim = TRUE, animType = "fade", time = 0.5)
    }

    output$verbatimTextOutput_name_of_qc_fit <- renderText({
      paste0(selected_run$fit_name)
    })


    if(input$compareFitsCheck)
    {
      selected_run_compare <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]

      output$datatable_QC_flags <- DT::renderDataTable({
        filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter",
                           "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter",
                           "em_cncf_icn_discord_filter", "dipLogR_too_low_filter",
                           "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
        filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern",
                         "No hyper segmentation", "Not high ploidy", "Has valid purity",
                         "em vs. cncf TCN/LCN discordance", "dipLogR not too low",
                         "ICN is discordant with allelic state ", "High % subclonal","contamination check")

        df <- data.frame(filter_name = filter_names,
                         passed = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                         passed_compare = unlist(selected_run_compare[, paste0(filter_columns, '_pass')], use.names = F),
                         note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
        gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
        DT::datatable(df %>%
                        mutate(passed = ifelse(passed, gicon('ok'), gicon('remove')))
                         %>% mutate(passed_compare = ifelse(passed_compare, gicon('ok'), gicon('remove'))),
                      selection=list(mode='single'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                     pageLength = 50, dom = 't', rownames= FALSE),
                      colnames = c("Filter" , selected_run$tumor_sample_id, selected_run_compare$tumor_sample_id, "Flag Description"),
                      escape=F)
      })

      combined_run <- rbind(selected_run, selected_run_compare)
      output$datatable_QC_metrics <- DT::renderDataTable({
        DT::datatable(combined_run %>%
                        select(-ends_with("note")) %>%
                        select(-ends_with("pass")) %>%
                        t,
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 200,
                        dom = 't',
                        rownames = FALSE
                      ),
                      colnames = c(""))
      })

    }
    else
    {
      output$datatable_QC_flags <- DT::renderDataTable({
        filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter",
                           "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter",
                           "em_cncf_icn_discord_filter", "dipLogR_too_low_filter",
                           "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
        filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern",
                         "No hyper segmentation", "Not high ploidy", "Has valid purity",
                         "em vs. cncf TCN/LCN discordance", "dipLogR not too low",
                         "ICN is discordant with allelic state ", "High % subclonal","contamination check")

        df <- data.frame(filter_name = filter_names,
                         passed = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                         note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
        gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
        DT::datatable(df %>%
                        mutate(passed = ifelse(passed, gicon('ok'), gicon('remove'))),
                      selection=list(mode='single'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                     pageLength = 50, dom = 't', rownames= FALSE),
                      colnames = c("Filter" , selected_run$tumor_sample_id, "Note"),
                      escape=F)
      })

      output$datatable_QC_metrics <- DT::renderDataTable({
        DT::datatable(selected_run %>%
                        select(-ends_with("note")) %>%
                        select(-ends_with("pass")) %>%
                        t,
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 200,
                        dom = 't',
                        rownames = FALSE
                      ),
                      colnames = c(""))
      })
    }

    ## if 'purity' run exists, show it by default; otherwise show the hisens run.
    ## The following piece of code is just a hack to fool the reactive environment to trigger showing
    ## selected run on the first selection
    values$show_fit_type = ifelse(!is.na(selected_run$purity_run_version[1]), 'Purity', 'Hisens')

    if (is.null(input$radioGroupButton_fitType) || input$radioGroupButton_fitType == 'Hisens') {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Purity")
    } else {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected="Hisens")
    }
  })


  observeEvent(input$selectInput_selectFit_compare, {

    output$verbatimTextOutput_runParams_compare <- renderText({})
    output$imageOutput_pngImage2 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)

    if ( input$selectInput_selectFit_compare == 'Not selected') {
      if (!is.null(values$show_fit_compare) && values$show_fit_compare != '') {
        updateSelectInput(session, "selectInput_selectFit_compare",
                          choices = as.list(c("Not selected", unlist(values$sample_runs_compare$fit_name))),
                          selected = values$show_fit_compare
        )
        values$show_fit_compare = ''
      }
      return (NULL)
    }

    # update other text options
    selected_run <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]
    if ( selected_run$is_best_fit[1]) {
      shinyjs::showElement(id="div_bestFitTrophy_compare", anim = TRUE, animType = "fade", time = 0.5)
    } else {
      shinyjs::hideElement(id="div_bestFitTrophy_compare", anim = TRUE, animType = "fade", time = 0.5)
    }


    if(input$compareFitsCheck)
    {
      selected_run_base <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

      output$datatable_QC_flags <- DT::renderDataTable({
        filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter",
                           "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter",
                           "em_cncf_icn_discord_filter", "dipLogR_too_low_filter",
                           "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
        filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern",
                         "No hyper segmentation", "Not high ploidy", "Has valid purity",
                         "em vs. cncf TCN/LCN discordance", "dipLogR not too low",
                         "ICN is discordant with allelic state ", "High % subclonal","contamination check")

        df <- data.frame(filter_name = filter_names,
                         passed = unlist(selected_run_base[, paste0(filter_columns, '_pass')], use.names = F),
                         passed_compare = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                         note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
        gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
        DT::datatable(df %>%
                        mutate(passed = ifelse(passed, gicon('ok'), gicon('remove')))
                      %>% mutate(passed_compare = ifelse(passed_compare, gicon('ok'), gicon('remove'))),
                      selection=list(mode='single'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                     pageLength = 50, dom = 't', rownames= FALSE),
                      colnames = c("Filter" , selected_run_base$tumor_sample_id, selected_run$tumor_sample_id, "Flag Description"),
                      escape=F)
      })

      combined_run <- rbind(selected_run_base, selected_run)
      output$datatable_QC_metrics <- DT::renderDataTable({
        DT::datatable(combined_run %>%
                        select(-ends_with("note")) %>%
                        select(-ends_with("pass")) %>%
                        t,
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 200,
                        dom = 't',
                        rownames = FALSE
                      ),
                      colnames = c(""))
      })
    }


    ## if 'purity' run exists, show it by default; otherwise show the hisens run.
    ## The following piece of code is just a hack to fool the reactive environment to trigger showing
    ## selected run on the first selection
    values$show_fit_type_compare = ifelse(!is.na(selected_run$purity_run_version[1]), 'Purity', 'Hisens')

    if (is.null(input$radioGroupButton_fitType_compare) || input$radioGroupButton_fitType_compare == 'Hisens') {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType_compare", selected="Purity")
    } else {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType_compare", selected="Hisens")
    }
  })


  observeEvent(input$dynamic_dipLogR, {
    if (input$dynamic_dipLogR) {
      session$sendCustomMessage("toggleOverlay", TRUE)
    } else {
      session$sendCustomMessage("toggleOverlay", FALSE)
    }
  })


  observeEvent(input$textInput_newDipLogR, {
    shinyWidgets::updateMaterialSwitch(session, "dynamic_dipLogR", value = FALSE)
  })


  # Initialize the default columns for Arm-Level
  observeEvent(processed_armLevel_data(), {
    data <- processed_armLevel_data()

    if (is.null(data)) return()

    # Set the armLevel_columns if not already set
    if (is.null(armLevel_columns())) {
      armLevel_columns(names(data))

      # Create armLevel_columns_compare with prefixed names
      compare_columns <- c(
        paste0("sample1_", names(data)),
        paste0("sample2_", names(data))
      )
      armLevel_columns_compare(compare_columns)

      # Set initial selected columns based on defaults
      initial_selected <- intersect(default_armLevel_columns, names(data))
      updateCheckboxGroupInput(session, "selectedColumns_sample1_armLevel",
                               choices = names(data),
                               selected = initial_selected)
    }
  })

  observeEvent(input$displayMode_armLevel, {
    current_columns <- c(input$selectedColumns_sample1_armLevel, input$selectedColumns_sample2_armLevel)
    available_columns <- armLevel_columns()
    available_columns_compare <- armLevel_columns_compare()

    if (input$displayMode_armLevel) {
      # Switching to long form (displayMode_armLevel = TRUE)

      # Convert selected short form columns back to long form
      selected_long <- unique(gsub("^sample1_|^sample2_", "", current_columns))

      updateCheckboxGroupInput(session, "selectedColumns_sample1_armLevel",
                               choices = available_columns,
                               selected = intersect(selected_long, available_columns))

      # Clear sample2 columns
      updateCheckboxGroupInput(session, "selectedColumns_sample2_armLevel",
                               choices = character(0),
                               selected = character(0))

    } else {
      # Switching to short form (displayMode_armLevel = FALSE)

      # Convert selected long form columns to short form
      selected_short_sample1 <- paste0("sample1_", intersect(current_columns, available_columns))
      selected_short_sample2 <- paste0("sample2_", intersect(current_columns, available_columns))

      sample1_choices <- intersect(available_columns_compare, paste0("sample1_", available_columns))
      sample2_choices <- intersect(available_columns_compare, paste0("sample2_", available_columns))

      updateCheckboxGroupInput(session, "selectedColumns_sample1_armLevel",
                               choices = sample1_choices,
                               selected = intersect(c(selected_short_sample1, input$selectedColumns_sample1_armLevel), sample1_choices))

      updateCheckboxGroupInput(session, "selectedColumns_sample2_armLevel",
                               choices = sample2_choices,
                               selected = intersect(c(selected_short_sample2, input$selectedColumns_sample2_armLevel), sample2_choices))
    }
  })

  processed_armLevel_data <- reactive({
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    selected_run_compare <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]

    req(input$radioGroupButton_fitType, selected_run)

    armLevel_data1 <- tryCatch({
      get_armLevel_table(input$radioGroupButton_fitType, selected_run)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(armLevel_data1) || nrow(armLevel_data1) == 0) {
      return(NULL)
    }

    if (!input$compareFitsCheck) {
      return(armLevel_data1)
    }

    armLevel_data2 <- if (!is.null(input$radioGroupButton_fitType_compare) && !is.null(selected_run_compare)) {
      tryCatch({
        get_armLevel_table(input$radioGroupButton_fitType_compare, selected_run_compare)
      }, error = function(e) {
        NULL
      })
    } else {
      NULL
    }

    combined_armLevel_data <- if (is.null(armLevel_data2) || nrow(armLevel_data2) == 0) {
      armLevel_data1
    } else {
      if (input$displayMode_armLevel) {
        rbind(armLevel_data1, armLevel_data2)
      } else {
        common_columns <- intersect(names(armLevel_data1), names(armLevel_data2))
        for (col in common_columns) {
          armLevel_data1[[col]] <- as.character(armLevel_data1[[col]])
          armLevel_data2[[col]] <- as.character(armLevel_data2[[col]])
        }
        armLevel_data1 <- armLevel_data1 %>% dplyr::rename_with(~ paste0("sample1_", .))
        armLevel_data2 <- armLevel_data2 %>% dplyr::rename_with(~ paste0("sample2_", .))
        combined <- dplyr::full_join(armLevel_data1, armLevel_data2, by = c("sample1_arm" = "sample2_arm"))
        combined[is.na(combined)] <- "-"
        combined
      }
    }

    return(combined_armLevel_data)
  })


  observeEvent(processed_geneLevel_data(), {
    data <- processed_geneLevel_data()

    # Set the geneLevel_columns if not already set
    if (is.null(geneLevel_columns())) {
      geneLevel_columns(names(data))

      # Create geneLevel_columns_compare with prefixed names
      compare_columns <- c(
        paste0("sample1_", names(data)),
        paste0("sample2_", names(data))
      )
      geneLevel_columns_compare(compare_columns)

      # Set initial selected columns based on defaults
      initial_selected <- intersect(default_geneLevel_columns, names(data))
      updateCheckboxGroupInput(session, "selectedColumns_sample1",
                               choices = names(data),
                               selected = initial_selected)
    }
  })

  observeEvent(input$displayMode_geneLevel, {
    current_columns <- c(input$selectedColumns_sample1, input$selectedColumns_sample2)
    available_columns <- geneLevel_columns()
    available_columns_compare <- geneLevel_columns_compare()

    if (input$displayMode_geneLevel) {
      # Switching to long form (displayMode_geneLevel = TRUE)
      selected_long <- unique(gsub("^sample1_|^sample2_", "", current_columns))

      updateCheckboxGroupInput(session, "selectedColumns_sample1",
                               choices = available_columns,
                               selected = intersect(selected_long, available_columns))

      # Clear sample2 columns
      updateCheckboxGroupInput(session, "selectedColumns_sample2",
                               choices = character(0),
                               selected = character(0))

    } else {
      # Switching to short form (displayMode_geneLevel = FALSE)
      selected_short_sample1 <- paste0("sample1_", intersect(current_columns, available_columns))
      selected_short_sample2 <- paste0("sample2_", intersect(current_columns, available_columns))

      sample1_choices <- intersect(available_columns_compare, paste0("sample1_", available_columns))
      sample2_choices <- intersect(available_columns_compare, paste0("sample2_", available_columns))

      updateCheckboxGroupInput(session, "selectedColumns_sample1",
                               choices = sample1_choices,
                               selected = intersect(c(selected_short_sample1, input$selectedColumns_sample1), sample1_choices))

      updateCheckboxGroupInput(session, "selectedColumns_sample2",
                               choices = sample2_choices,
                               selected = intersect(c(selected_short_sample2, input$selectedColumns_sample2), sample2_choices))
    }
  })

  processed_geneLevel_data <- reactive({
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    selected_run_compare <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]

    req(input$radioGroupButton_fitType, selected_run)

    geneLevel_data1 <- tryCatch({
      get_geneLevel_table(input$radioGroupButton_fitType, selected_run)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(geneLevel_data1) || nrow(geneLevel_data1) == 0) {
      return(NULL)
    }

    if (!input$compareFitsCheck) {
      return(geneLevel_data1)
    }

    geneLevel_data2 <- if (!is.null(input$radioGroupButton_fitType_compare) && !is.null(selected_run_compare)) {
      tryCatch({
        get_geneLevel_table(input$radioGroupButton_fitType_compare, selected_run_compare)
      }, error = function(e) {
        NULL
      })
    } else {
      NULL
    }

    combined_geneLevel_data <- if (is.null(geneLevel_data2) || nrow(geneLevel_data2) == 0) {
      geneLevel_data1
    } else {
      if (input$displayMode_geneLevel) {
        rbind(geneLevel_data1, geneLevel_data2)
      } else {
        common_columns <- intersect(names(geneLevel_data1), names(geneLevel_data2))
        for (col in common_columns) {
          geneLevel_data1[[col]] <- as.character(geneLevel_data1[[col]])
          geneLevel_data2[[col]] <- as.character(geneLevel_data2[[col]])
        }
        geneLevel_data1 <- geneLevel_data1 %>% dplyr::rename_with(~ paste0("sample1_", .))
        geneLevel_data2 <- geneLevel_data2 %>% dplyr::rename_with(~ paste0("sample2_", .))
        combined <- dplyr::full_join(geneLevel_data1, geneLevel_data2, by = c("sample1_gene" = "sample2_gene"))
        combined[is.na(combined)] <- "-"
        combined
      }
    }

    return(combined_geneLevel_data)
  })


  observeEvent(input$displayOptionsSwitch_geneLevel, {
    if (input$displayOptionsSwitch_geneLevel) {
      # When display options are turned on, show the selected columns div
      shinyjs::show("selectColumnsDiv_geneLevel", anim = TRUE, animType = "slide")

      # Only show displayModeSwitchDiv_geneLevel if compareFitsSwitch is on
      if (input$compareFitsCheck) {
        shinyjs::show("displayModeSwitchDiv_geneLevel", anim = TRUE, animType = "slide")
      }
    } else {
      # When display options are turned off, hide both divs
      shinyjs::hide("selectColumnsDiv_geneLevel", anim = TRUE, animType = "slide")
      shinyjs::hide("displayModeSwitchDiv_geneLevel", anim = TRUE, animType = "slide")
    }
  })

  observeEvent(input$displayOptionsSwitch_armLevel, {
    if (input$displayOptionsSwitch_armLevel) {
      # When display options are turned on, show the selected columns div
      shinyjs::show("selectColumnsDiv_armLevel", anim = TRUE, animType = "slide")

      # Only show displayModeSwitchDiv_armLevel if compareFitsSwitch is on
      if (input$compareFitsCheck) {
        shinyjs::show("displayModeSwitchDiv_armLevel", anim = TRUE, animType = "slide")
      }
    } else {
      # When display options are turned off, hide both divs
      shinyjs::hide("selectColumnsDiv_armLevel", anim = TRUE, animType = "slide")
      shinyjs::hide("displayModeSwitchDiv_armLevel", anim = TRUE, animType = "slide")
    }
  })


  observeEvent(input$compareFitsCheck, {
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    selected_run_compare <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]


    if (input$compareFitsCheck) {
      output$datatable_QC_flags <- DT::renderDataTable({
        filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter",
                           "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter",
                           "em_cncf_icn_discord_filter", "dipLogR_too_low_filter",
                           "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
        filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern",
                         "No hyper segmentation", "Not high ploidy", "Has valid purity",
                         "em vs. cncf TCN/LCN discordance", "dipLogR not too low",
                         "ICN is discordant with allelic state ", "High % subclonal","contamination check")

        df <- data.frame(filter_name = filter_names,
                         passed = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                         passed_compare = unlist(selected_run_compare[, paste0(filter_columns, '_pass')], use.names = F),
                         note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
        gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
        DT::datatable(df %>%
                        mutate(passed = ifelse(passed, gicon('ok'), gicon('remove')))
                      %>% mutate(passed_compare = ifelse(passed_compare, gicon('ok'), gicon('remove'))),
                      selection=list(mode='single'),
                      options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                     pageLength = 50, dom = 't', rownames= FALSE),
                      colnames = c("Filter" , selected_run$tumor_sample_id, selected_run_compare$tumor_sample_id, "Flag Description"),
                      escape=F)
      })

      output$datatable_cncf <- DT::renderDataTable({
        cncf_data1 <- get_cncf_table(input$radioGroupButton_fitType, selected_run)
        cncf_data2 <- get_cncf_table(input$radioGroupButton_fitType_compare, selected_run_compare)

        if (dim(cncf_data1)[1] == 0 || dim(cncf_data2)[1] == 0) {
          showModal(modalDialog(title = "CNCF file missing", "Invalid CNCF file"))
          return()
        }

        combined_data <- rbind(cncf_data1, cncf_data2)

        DT::datatable(combined_data,
                      selection = list(mode = 'single'),
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 50,
                        rownames = FALSE
                      ))
      })



      output$datatable_geneLevel <- DT::renderDataTable({
        data <- processed_geneLevel_data()

        if (is.null(data)) return(NULL)

        # Subset the data based on selected columns, but only select columns that exist in the data
        selected_cols <- intersect(c(input$selectedColumns_sample1, input$selectedColumns_sample2), names(data))
        if (!is.null(selected_cols) && length(selected_cols) > 0) {
          data <- data[, selected_cols, drop = FALSE]
        }

        DT::datatable(data,
                      selection = list(mode = 'single'),
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 50,
                        rownames = FALSE
                      ))
      })


      output$datatable_armLevel <- DT::renderDataTable({
        data <- processed_armLevel_data()

        if (is.null(data)) return(NULL)

        # Subset the data based on selected columns
        selected_cols <- intersect(c(input$selectedColumns_sample1_armLevel, input$selectedColumns_sample2_armLevel), names(data))
        if (!is.null(selected_cols) && length(selected_cols) > 0) {
          data <- data[, selected_cols, drop = FALSE]
        }

        DT::datatable(data,
                      selection = list(mode = 'single'),
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 50,
                        rownames = FALSE
                      ))
      })

      combined_run <- rbind(selected_run, selected_run_compare)
      output$datatable_QC_metrics <- DT::renderDataTable({
        DT::datatable(combined_run %>%
                        select(-ends_with("note")) %>%
                        select(-ends_with("pass")) %>%
                        t,
                      options = list(
                        columnDefs = list(
                          list(targets = "_all", className = 'dt-center')
                        ),
                        pageLength = 200,
                        dom = 't',
                        rownames = FALSE
                      ),
                      colnames = c(""))
      })

      output$editableSegmentsTable <- rhandsontable::renderRHandsontable({
        cncf_data1 <- get_cncf_table(input$radioGroupButton_fitType_compare, selected_run_compare) %>%
          data.frame()
        cncf_data2 <- get_cncf_table(input$radioGroupButton_fitType, selected_run) %>%
          data.frame()

        if (dim(cncf_data1)[1] == 0 || dim(cncf_data2)[1] == 0) {
          showModal(modalDialog(title = "CNCF file missing", "Invalid CNCF file"))
          return()
        }

        combined_cncf_data <- rbind(cncf_data1, cncf_data2)

        if (!is.null(combined_cncf_data)) {
          rhandsontable::rhandsontable(combined_cncf_data, useTypes = FALSE, stretchH = "all") %>%
            rhandsontable::hot_table(columnSorting = TRUE,
                                     highlightRow = TRUE,
                                     highlightCol = TRUE)
        }
      })

      observe({
        if(!is.null(input$editableSegmentsTable$changes$changes)){
          shinyjs::show("button_saveChanges")
        }
      })

      updateCloseups()

    } else {

        output$datatable_QC_flags <- DT::renderDataTable({
          filter_columns = c("homdel_filter", "diploid_seg_filter", "waterfall_filter",
                             "hyper_seg_filter", "high_ploidy_filter", "valid_purity_filter",
                             "em_cncf_icn_discord_filter", "dipLogR_too_low_filter",
                             "icn_allelic_state_concordance_filter", "subclonal_genome_filter", "contamination_filter")
          filter_names = c("Homozygous deletions", "Diploid segments (in dipLogR)", "No Waterfall pattern",
                           "No hyper segmentation", "Not high ploidy", "Has valid purity",
                           "em vs. cncf TCN/LCN discordance", "dipLogR not too low",
                           "ICN is discordant with allelic state ", "High % subclonal","contamination check")

          df <- data.frame(filter_name = filter_names,
                           passed = unlist(selected_run[, paste0(filter_columns, '_pass')], use.names = F),
                           note = unlist(selected_run[, paste0(filter_columns, '_note')], use.names = F))
          gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
          DT::datatable(df %>%
                          mutate(passed = ifelse(passed, gicon('ok'), gicon('remove'))),
                        selection=list(mode='single'),
                        options = list(columnDefs = list(list(className = 'dt-center', targets=0:2)),
                                       pageLength = 50, dom = 't', rownames= FALSE),
                        colnames = c("Filter" , selected_run$tumor_sample_id, "Note"),
                        escape=F)
        })

        output$datatable_cncf <- DT::renderDataTable({
          cncf_data <-
            get_cncf_table(input$radioGroupButton_fitType, selected_run)
          if ( dim(cncf_data)[1] == 0 ){
            showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
            return()
          }
          DT::datatable(cncf_data,
                        selection=list(mode='single'),
                        options = list(
                          columnDefs = list(
                            list(targets = "_all", className = 'dt-center')
                          ),
                          pageLength = 50,
                          rownames = FALSE
                        ))
        })

        output$datatable_armLevel <- DT::renderDataTable({
          data <- processed_armLevel_data()

          if (is.null(data)) return(NULL)

          # Subset the data based on selected columns
          selected_cols <- intersect(c(input$selectedColumns_sample1_armLevel, input$selectedColumns_sample2_armLevel), names(data))
          if (!is.null(selected_cols) && length(selected_cols) > 0) {
            data <- data[, selected_cols, drop = FALSE]
          }

          DT::datatable(data,
                        selection = list(mode = 'single'),
                        options = list(
                          columnDefs = list(
                            list(targets = "_all", className = 'dt-center')
                          ),
                          pageLength = 50,
                          rownames = FALSE
                        ))
        })


        output$datatable_geneLevel <- DT::renderDataTable({
          data <- processed_geneLevel_data()

          if (is.null(data)) return(NULL)

          # Subset the data based on selected columns, but only select columns that exist in the data
          selected_cols <- intersect(c(input$selectedColumns_sample1, input$selectedColumns_sample2), names(data))
          if (!is.null(selected_cols) && length(selected_cols) > 0) {
            data <- data[, selected_cols, drop = FALSE]
          }

          DT::datatable(data,
                        selection = list(mode = 'single'),
                        options = list(
                          columnDefs = list(
                            list(targets = "_all", className = 'dt-center')
                          ),
                          pageLength = 50,
                          rownames = FALSE
                        ))
        })



        output$editableSegmentsTable <- rhandsontable::renderRHandsontable({
          cncf_data <-
            get_cncf_table(input$radioGroupButton_fitType, selected_run) %>%
            data.frame()
          if ( dim(cncf_data)[1] == 0 ){
            showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
            return()
          }
          if (!is.null(cncf_data)) {
            rhandsontable::rhandsontable(cncf_data,
                                         useTypes=FALSE, stretchH = "all") %>%
              rhandsontable::hot_table(columnSorting = TRUE,
                                       highlightRow = TRUE,
                                       highlightCol = TRUE)
          }
        })

        observe({
          if(!is.null(input$editableSegmentsTable$changes$changes)){
            shinyjs::show("button_saveChanges")
          }
        })

        output$datatable_QC_metrics <- DT::renderDataTable({
          DT::datatable(selected_run %>%
                          select(-ends_with("note")) %>%
                          select(-ends_with("pass")) %>%
                          t,
                        options = list(
                          columnDefs = list(
                            list(targets = "_all", className = 'dt-center')
                          ),
                          pageLength = 200,
                          dom = 't',
                          rownames = FALSE
                        ),
                        colnames = c(""))
        })

        updateCloseups()

    }
  })


  observeEvent(input$compareFitsCheck, {
    data <- processed_geneLevel_data()
    if (is.null(data)) return()

    if (input$compareFitsCheck) {
      # Show the elements when compareFitsCheck is true
      shinyjs::showElement(id = "selectBox_compare", anim = TRUE, animType = "slide")
      shinyjs::showElement(id = "runType_compare", anim = TRUE, animType = "slide")
      shinyjs::show("div_imageOutput_pngImage2", anim = TRUE, animType = "fade", time = 1)
      shinyjs::runjs("$('#displayModeSwitchDiv_geneLevel').removeClass('hidden');")
      shinyjs::runjs("$('#displayModeSwitchDiv_armLevel').removeClass('hidden');")

      shinyWidgets::updateSwitchInput(session, "displayMode_geneLevel", value = input$displayMode_geneLevel)
      shinyWidgets::updateSwitchInput(session, "displayMode_armLevel", value = input$displayMode_armLevel)

      if (input$displayOptionsSwitch_geneLevel) {
        shinyjs::show("displayModeSwitchDiv_geneLevel", anim = TRUE, animType = "slide")
      }
      if (input$displayOptionsSwitch_armLevel) {
        shinyjs::show("displayModeSwitchDiv_armLevel", anim = TRUE, animType = "slide")
      }
    } else {
      # Hide the elements when compareFitsCheck is false
      shinyjs::hideElement(id = "selectBox_compare", anim = TRUE, animType = "slide")
      shinyjs::hideElement(id = "runType_compare", anim = TRUE, animType = "slide")
      shinyjs::hide("div_imageOutput_pngImage2", anim = TRUE, animType = "fade", time = 1)
      shinyjs::hide("displayModeSwitchDiv_geneLevel", anim = TRUE, animType = "slide")
      shinyjs::hide("displayModeSwitchDiv_armLevel", anim = TRUE, animType = "slide")
      shinyjs::runjs("$('#displayModeSwitchDiv_geneLevel').addClass('hidden');")
      shinyjs::runjs("$('#displayModeSwitchDiv_armLevel').addClass('hidden');")

      shinyWidgets::updateSwitchInput(session, "displayMode_geneLevel", value = TRUE)
      shinyWidgets::updateSwitchInput(session, "displayMode_armLevel", value = TRUE)
    }
  })


  observeEvent(input$use_remote_refit_switch, {
    if (input$use_remote_refit_switch) {
      shinyjs::show("remote_refit_options", anim = TRUE, animType = "slide")
    } else {
      shinyjs::hide("remote_refit_options", anim = TRUE, animType = "slide")
    }
  })

  observeEvent(input$button_addReview, {

    if (!((session_data$password_personal == 1 && !input$storageType) || session_data$password_valid == 1)) {
      showNotification("You are not authorized to review this sample.", type = "error", duration = 5)
      return()
    }

    #selected_run <- values$sample_runs[1,]
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    sample = selected_run$tumor_sample_id[1]
    path = selected_run$path[1]

    if (!verify_access_to_write(path)) {
      showModal(modalDialog(
        title = "Failed to add review",
        paste0("You do not have permissions to create/edit: ", path, "/facets_review.manifest")
      ))
      return(NULL)
    }

    facets_qc = as.character(selected_run$facets_qc[1])
    facets_qc_version = as.character(selected_run$facets_qc_version[1])
    facets_suite_version = as.character(selected_run$facets_suite_version[1])

    review_status = input$radioButtons_reviewStatus
    fit_name = input$selectInput_selectBestFit[1]
    signed_as = system('whoami', intern=T)
    note = input$textAreaInput_reviewNote[1]
    use_only_purity_run = input$checkbox_purity_only[1]
    use_edited_cncf = input$checkbox_use_edited_cncf[1]
    reviewer_set_purity = input$textInput_purity[1]

    ### reset review stauts fields
    #updateSelectInput(session, 'selectInput_selectBestFit', choices=c("Not selected"))
    updateCheckboxInput(session, 'checkbox_purity_only', value = F)
    updateCheckboxInput(session, 'checkbox_use_edited_cncf', value = F)
    updateRadioButtons(session, 'radioButtons_reviewStatus', selected='not_reviewed')
    updateTextInput(session, 'textInput_purity', value='')
    updateTextAreaInput(session, 'textAreaInput_reviewNote', value='')

    ### make sure the edited cncf file exists
    if (use_edited_cncf) {
      cncf_filename = paste0(path, fit_name, '/', sample, '_',
                             ifelse(use_only_purity_run, 'purity', 'hisens'),
                             '.cncf.edited.txt')
      if (!file.exists(cncf_filename)) {
        showModal(modalDialog(
          title = "Failed", paste0("Review not added. CNCF file assigned to this review does not exist. ",
                                             cncf_filename)
        ))
        return(NULL)
      }
    }

    df <- get_review_status(sample, path)

    if (nrow(df) > 0){
      ## check if the sample has been recently reviewed (in the past 1hr)
      cur_time = Sys.time()
      if (any(which(as.numeric(difftime(cur_time, df$date_reviewed, units="hours")) < 1))) {
        showModal(modalDialog(
          title = "ALERT", paste0("This sample has been reviewed within the past hour.
                                  Your review is added but make sure all is tight.")
        ))
      }
    }

    df <- data.frame(
      sample = c(sample),
      path = c(path),
      review_status = c(review_status),
      fit_name = c(fit_name),
      review_notes = c(note),
      reviewed_by = c(signed_as),
      date_reviewed = as.character(Sys.time()),
      facets_qc = c(facets_qc),
      use_only_purity_run = c(use_only_purity_run),
      use_edited_cncf = c(use_edited_cncf),
      reviewer_set_purity = c(reviewer_set_purity),
      facets_qc_version = c(facets_qc_version),
      facets_suite_version = c(facets_suite_version),
      stringsAsFactors=FALSE
    )
    update_review_status_file(path, df)

    update_best_fit_status(sample, path)

    refresh_review_status(sample, path, values$sample_runs)
  })

  observeEvent(input$radioGroupButton_fitType, {

    if (input$selectInput_selectFit == "Not selected") {
      return(NULL)
    }

    if (values$show_fit_type != "" & input$radioGroupButton_fitType != values$show_fit_type) {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType", selected=values$show_fit_type)
      return(NULL)
    }
    values$show_fit_type = ""

    output$verbatimTextOutput_runParams <- renderText({})
    output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)

    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

    shinyjs::hideElement("button_saveChanges")
    output$verbatimTextOutput_runParams <- renderText({
      if (input$radioGroupButton_fitType == "Hisens") {
        paste0("purity: ", selected_run$hisens_run_Purity[1], ", ",
               "ploidy: ", selected_run$hisens_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$hisens_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run$hisens_run_version[1])
      } else {
        paste0("purity: ", selected_run$purity_run_Purity[1], ", ",
               "ploidy: ", selected_run$purity_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run$purity_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run$purity_run_version[1], "\n",
               "alt dipLogR: ", selected_run$purity_run_alBalLogR[1])
      }
    })

    output$datatable_cncf <- DT::renderDataTable({
      cncf_data <-
        get_cncf_table(input$radioGroupButton_fitType, selected_run)
      if ( dim(cncf_data)[1] == 0 ){
        showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
        return()
      }
      DT::datatable(cncf_data,
                    selection=list(mode='single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })

    output$datatable_geneLevel <- DT::renderDataTable({
      data <- processed_geneLevel_data()

      if (is.null(data)) return(NULL)

      # Subset the data based on selected columns, but only select columns that exist in the data
      selected_cols <- intersect(c(input$selectedColumns_sample1, input$selectedColumns_sample2), names(data))
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        data <- data[, selected_cols, drop = FALSE]
      }

      DT::datatable(data,
                    selection = list(mode = 'single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })



    output$datatable_armLevel <- DT::renderDataTable({
      data <- processed_armLevel_data()

      if (is.null(data)) return(NULL)

      # Subset the data based on selected columns
      selected_cols <- intersect(c(input$selectedColumns_sample1_armLevel, input$selectedColumns_sample2_armLevel), names(data))
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        data <- data[, selected_cols, drop = FALSE]
      }

      DT::datatable(data,
                    selection = list(mode = 'single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })


    output$editableSegmentsTable <- rhandsontable::renderRHandsontable({
      cncf_data <-
        get_cncf_table(input$radioGroupButton_fitType, selected_run) %>%
        data.frame()
      if ( dim(cncf_data)[1] == 0 ){
        showModal(modalDialog( title = "CNCF file missing", "Invalid CNCF file" ))
        return()
      }
      if (!is.null(cncf_data)) {
        rhandsontable::rhandsontable(cncf_data,
                                     useTypes=FALSE, stretchH = "all") %>%
          rhandsontable::hot_table(columnSorting = TRUE,
                    highlightRow = TRUE,
                    highlightCol = TRUE)
      }
    })

    observe({
      if(!is.null(input$editableSegmentsTable$changes$changes)){
        shinyjs::show("button_saveChanges")
      }
    })

    output$imageOutput_pngImage1 <- renderImage({
      if (input$radioGroupButton_fitType == "Hisens") {
        png_filename = paste0(selected_run$hisens_run_prefix[1], ".CNCF.png")
      } else {
        png_filename = paste0(selected_run$purity_run_prefix[1], ".CNCF.png")
      }
      if (!file.exists(png_filename)) {
        png_filename = gsub("\\.CNCF", "", png_filename)
      }
      list(src = png_filename, contentType = 'image/png', width = 650, height = 800)
    },
    deleteFile = FALSE)

    #output$plotOutput_closeup <- renderPlot ({
    #  list(src="", width=0, height=0)
    #})
    updateCloseups()
  })


  observeEvent(input$radioGroupButton_fitType_compare, {

    if (input$selectInput_selectFit_compare == "Not selected") {
      return(NULL)
    }

    if (values$show_fit_type_compare != "" & input$radioGroupButton_fitType != values$show_fit_type_compare) {
      shinyWidgets::updateRadioGroupButtons(session, "radioGroupButton_fitType_compare", selected=values$show_fit_type_compare)
      return(NULL)
    }
    values$show_fit_type_compare = ""

    output$verbatimTextOutput_runParams_compare <- renderText({})
    output$imageOutput_pngImage2 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)

    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    selected_run_compare <- values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]

    shinyjs::hideElement("button_saveChanges")
    output$verbatimTextOutput_runParams_compare <- renderText({
      if (input$radioGroupButton_fitType_compare == "Hisens") {
        paste0("purity: ", selected_run_compare$hisens_run_Purity[1], ", ",
               "ploidy: ", selected_run_compare$hisens_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run_compare$hisens_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run_compare$hisens_run_version[1])
      } else {
        paste0("purity: ", selected_run_compare$purity_run_Purity[1], ", ",
               "ploidy: ", selected_run_compare$purity_run_Ploidy[1], ", ",
               "dipLogR: ", selected_run_compare$purity_run_dipLogR[1], "\n",
               "facets_lib: ", selected_run_compare$purity_run_version[1], "\n",
               "alt dipLogR: ", selected_run_compare$purity_run_alBalLogR[1])
      }
    })

    output$datatable_cncf <- DT::renderDataTable({
      cncf_data1 <- get_cncf_table(input$radioGroupButton_fitType, selected_run)
      cncf_data2 <- get_cncf_table(input$radioGroupButton_fitType_compare, selected_run_compare)

      if (dim(cncf_data1)[1] == 0 || dim(cncf_data2)[1] == 0) {
        showModal(modalDialog(title = "CNCF file missing", "Invalid CNCF file"))
        return()
      }

      combined_data <- rbind(cncf_data1, cncf_data2)

      DT::datatable(combined_data,
                    selection = list(mode = 'single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })


    output$datatable_geneLevel <- DT::renderDataTable({
      data <- processed_geneLevel_data()

      if (is.null(data)) return(NULL)

      # Subset the data based on selected columns, but only select columns that exist in the data
      selected_cols <- intersect(c(input$selectedColumns_sample1, input$selectedColumns_sample2), names(data))
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        data <- data[, selected_cols, drop = FALSE]
      }

      DT::datatable(data,
                    selection = list(mode = 'single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })


    output$datatable_armLevel <- DT::renderDataTable({
      data <- processed_armLevel_data()

      if (is.null(data)) return(NULL)

      # Subset the data based on selected columns
      selected_cols <- intersect(c(input$selectedColumns_sample1_armLevel, input$selectedColumns_sample2_armLevel), names(data))
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        data <- data[, selected_cols, drop = FALSE]
      }

      DT::datatable(data,
                    selection = list(mode = 'single'),
                    options = list(
                      columnDefs = list(
                        list(targets = "_all", className = 'dt-center')
                      ),
                      pageLength = 50,
                      rownames = FALSE
                    ))
    })


    output$editableSegmentsTable <- rhandsontable::renderRHandsontable({
      cncf_data1 <- get_cncf_table(input$radioGroupButton_fitType, selected_run) %>%
        data.frame()
      cncf_data2 <- get_cncf_table(input$radioGroupButton_fitType_compare, selected_run_compare) %>%
        data.frame()

      if (dim(cncf_data1)[1] == 0 || dim(cncf_data2)[1] == 0) {
        showModal(modalDialog(title = "CNCF file missing", "Invalid CNCF file"))
        return()
      }

      combined_cncf_data <- rbind(cncf_data1, cncf_data2)

      if (!is.null(combined_cncf_data)) {
        rhandsontable::rhandsontable(combined_cncf_data, useTypes = FALSE, stretchH = "all") %>%
          rhandsontable::hot_table(columnSorting = TRUE,
                                   highlightRow = TRUE,
                                   highlightCol = TRUE)
      }
    })

    observe({
      if(!is.null(input$editableSegmentsTable$changes$changes)){
        shinyjs::show("button_saveChanges")
      }
    })

    output$imageOutput_pngImage2 <- renderImage({
      if (input$radioGroupButton_fitType_compare == "Hisens") {
        png_filename = paste0(selected_run_compare$hisens_run_prefix[1], ".CNCF.png")
      } else {
        png_filename = paste0(selected_run_compare$purity_run_prefix[1], ".CNCF.png")
      }
      if (!file.exists(png_filename)) {
        png_filename = gsub("\\.CNCF", "", png_filename)
      }
      list(src = png_filename, contentType = 'image/png', width = 650, height = 800)
    },
    deleteFile = FALSE)

    updateCloseups()
  })


  observeEvent(input$button_saveChanges, {
    if(is.null(input$editableSegmentsTable$changes$changes)){
      return(NULL)
    }
    selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
    df <- rhandsontable::hot_to_r(input$editableSegmentsTable)

    if (input$radioGroupButton_fitType == "Hisens") {
      run_prefix = selected_run$hisens_run_prefix[1]
    } else {
      run_prefix = selected_run$purity_run_prefix[1]
    }
    if ( file.exists(paste0(run_prefix, ".cncf.edited.txt"))) {
      cncf_filename = paste0(run_prefix, ".cncf.edited.txt")
    } else {
      cncf_filename = paste0(run_prefix, ".cncf.txt")
    }
    df <-
      fread(cncf_filename) %>%
      dplyr::select(-(tcn:lcn.em)) %>%
      dplyr::left_join(df %>% data.table %>% dplyr::select(chrom, loc.start, loc.end, seg, tcn:lcn.em),
                by=c( "chrom", "loc.start", "loc.end", "seg"))
    write.table(df, paste0(run_prefix, ".cncf.edited.txt"), quote=F, row.names=F, sep="\t")

    shinyjs::hideElement("button_saveChanges")
  })


  observeEvent(input$button_closeUpView, {
    updateCloseups();
  })

  updateCloseups <- function() {

    if (input$selectInput_selectFit == "Not selected") {
      output$verbatimTextOutput_runParams <- renderText({})
      output$imageOutput_pngImage1 <- renderImage({ list(src="", width=0, height=0)}, deleteFile=FALSE)
      return(NULL)
    }

    selected_run <-
      values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]

    if(input$compareFitsCheck)
    {
      selected_run_compare <-
        values$sample_runs_compare[which(values$sample_runs_compare$fit_name == paste0(input$selectInput_selectFit_compare)),]
    }

    selected_gene = input$textInput_geneForCloseup
    values$selected_closeup_gene = input$textInput_geneForCloseup
    if (selected_gene == "") {
      #showModal(modalDialog( title = "No action", "Enter a gene name to get the closeup" ))
      return(NULL)
    }


    if(input$compareFitsCheck)
    {
      output$plotOutput_closeup <- renderPlot ({
        if (input$radioGroupButton_fitType == "Hisens") {
          rdata_file = paste0(selected_run$hisens_run_prefix[1], ".Rdata")
          rdata_file_compare = paste0(selected_run_compare$hisens_run_prefix[1], ".Rdata")
        } else {
          rdata_file = paste0(selected_run$purity_run_prefix[1], ".Rdata")
          rdata_file_compare = paste0(selected_run_compare$purity_run_prefix[1], ".Rdata")
        }

        load(rdata_file)
        out1 <- out
        fit1 <- fit

        load(rdata_file_compare)
        out2 <- out
        fit2 <- fit

        closeup_output1 <- close.up(out1, fit1, gene.name=selected_gene,
                                    cached.gene.path =
                                      system.file("data/Homo_sapiens.GRCh37.75.gene_positions.txt",
                                                  package="facetsPreview"))

        closeup_output2 <- close.up(out2, fit2, gene.name=selected_gene,
                                    cached.gene.path =
                                      system.file("data/Homo_sapiens.GRCh37.75.gene_positions.txt",
                                                  package="facetsPreview"))

        header1 <- grid::textGrob(selected_run$tumor_sample_id, gp=grid::gpar(fontsize=14, fontface="bold"))
        header2 <- grid::textGrob(selected_run_compare$tumor_sample_id,, gp=grid::gpar(fontsize=14, fontface="bold"))

        # Combine the plots from both datasets
        plot1 <- gridExtra::grid.arrange(
          closeup_output1$cnlr, closeup_output1$valor,
          closeup_output1$icnem, closeup_output1$cfem,
          ncol = 1,
          top = paste0(selected_run$tumor_sample_id,
                       ": ", selected_gene,
                       " | ", closeup_output1$chrom,
                       ":", closeup_output1$start,
                       "-", closeup_output1$end)
        )

        # Create the plot for Dataset 2
        plot2 <- gridExtra::grid.arrange(
          closeup_output2$cnlr, closeup_output2$valor,
          closeup_output2$icnem, closeup_output2$cfem,
          ncol = 1,
          top = paste0(selected_run_compare$tumor_sample_id,
                       ": ", selected_gene,
                       " | ", closeup_output2$chrom,
                       ":", closeup_output2$start,
                       "-", closeup_output2$end)
        )

        # Render both plots side by side with headers
        gridExtra::grid.arrange(plot1, plot2, ncol = 2)
      })
    }
    else
    {
      output$plotOutput_closeup <- renderPlot ({
        if (input$radioGroupButton_fitType == "Hisens") {
          rdata_file = paste0(selected_run$hisens_run_prefix[1], ".Rdata")
        } else {
          rdata_file = paste0(selected_run$purity_run_prefix[1], ".Rdata")
        }
        load(rdata_file)
        closeup_output <- close.up(out, fit, gene.name=selected_gene,
                                   cached.gene.path =
                                     system.file("data/Homo_sapiens.GRCh37.75.gene_positions.txt",
                                                 package="facetsPreview"))
        gridExtra::grid.arrange(closeup_output$cnlr,
                                closeup_output$valor,
                                closeup_output$icnem,
                                closeup_output$cfem,
                                ncol=1, nrow=4, top = paste0(selected_gene,
                                                             " ", closeup_output$chrom,
                                                             ":", closeup_output$start,
                                                             "-", closeup_output$end))
      })
    }
  }

  # Observe changes to the remote refit switch
  observeEvent(input$session_remote_refit, {
    if (is_vm_mode()) {
      shinyWidgets::updateSwitchInput(session, "session_remote_refit", value = FALSE)
      return()
    }

    if (!input$session_remote_refit && !validate_path(input$remote_refit_path)) {
      updateTextInput(session, "remote_refit_path", value = "/juno/work/ccs/shared/resources/fp/")
    }

    mount_refit_path <- input$mount_refit_path

    # Normalize the mount_refit_path by removing any trailing slash
    normalized_mount_refit_path <- sub("/+$", "", mount_refit_path)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    # Look for a match in mount paths
    matched_row <- mount_df[mount_df$normalized_local_path == normalized_mount_refit_path, ]

    # If no valid mount path is found, set the switch to false
    if (nrow(matched_row) == 0) {
      shinyWidgets::updateSwitchInput(session, "session_remote_refit", value = FALSE)
    }
  })


  # Observe changes to the impact switch
  observeEvent(input$session_switch_impact, {
    if (is_vm_mode()) {
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)
      return()
    }

    if (!input$session_switch_impact && !validate_path(input$remote_path_impact)) {
      updateTextInput(session, "remote_path_impact", value = "/juno/work/ccs/shared/resources/impact/facets/all/")
    }

    # Check if the mount_refit_path is invalid and set the switch to false
    repository_path_impact <- input$repository_path_impact
    print(c("repository_path_impact: ", repository_path_impact))


    # Normalize the repository_path_impact by removing any trailing slash
    normalized_repository_path_impact <- sub("/+$", "", repository_path_impact)
    print(c("repository_path_impact: ",repository_path_impact))
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)
    print(mount_df)

    # Look for a match in mount paths
    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_impact, ]

    # If no valid mount path is found, set the switch to false
    if (nrow(matched_row) == 0) {
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = FALSE)
    }
  })



  # Observe changes to the tempo switch
  observeEvent(input$session_switch_tempo, {
    if (is_vm_mode()) {
      shinyWidgets::updateSwitchInput(session, "session_switch_tempo", value = FALSE)
      return()
    }

    if (!input$session_switch_tempo && !validate_path(input$remote_path_tempo)) {
      updateTextInput(session, "remote_path_tempo", value = "/juno/work/ccs/")
    }

    # Check if the mount_refit_path is invalid and set the switch to false
    repository_path_tempo <- input$repository_path_tempo

    # Normalize the repository_path_tempo by removing any trailing slash
    normalized_repository_path_tempo <- sub("/+$", "", repository_path_tempo)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    # Look for a match in mount paths
    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_tempo, ]

    # If no valid mount path is found, set the switch to false
    if (nrow(matched_row) == 0) {
      shinyWidgets::updateSwitchInput(session, "session_switch_tempo", value = FALSE)
    }
  })


  # Observe changes to the tcga switch
  observeEvent(input$session_switch_tcga, {
    if (is_vm_mode()) {
      shinyWidgets::updateSwitchInput(session, "session_switch_tcga", value = FALSE)
      return()
    }

    if (!input$session_switch_tcga && !validate_path(input$remote_path_tcga)) {
      updateTextInput(session, "remote_path_tcga", value = "/juno/work/ccs/shared/resources/tcga/facets/all/")
    }

    # Check if the mount_refit_path is invalid and set the switch to false
    repository_path_tcga <- input$repository_path_tcga

    # Normalize the repository_path_tcga by removing any trailing slash
    normalized_repository_path_tcga <- sub("/+$", "", repository_path_tcga)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    # Look for a match in mount paths
    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_tcga, ]

    # If no valid mount path is found, set the switch to false
    if (nrow(matched_row) == 0) {
      shinyWidgets::updateSwitchInput(session, "session_switch_tcga", value = FALSE)
    }
  })


  # Regular expression for validating paths, considering empty paths as valid
  valid_path_regex <- "^(/[^/ ]*)+/?$|^~(/[^/ ]*)*|^$"

  # Function to validate a path
  validate_path <- function(path) {
    grepl(valid_path_regex, path)
  }

  ensure_trailing_slash <- function(path) {
    if (nchar(path) > 0 && substr(path, nchar(path), nchar(path)) != "/") {
      return(paste0(path, "/"))
    }
    return(path)
  }

  clean_and_ensure_trailing_slash <- function(path) {
    path <- trimws(path)  # Strip whitespace from both sides
    if (nchar(path) > 0 && substr(path, nchar(path), nchar(path)) != "/") {
      return(paste0(path, "/"))
    }
    return(path)
  }

  get_mount_info <- function() {
    if (identical(Sys.getenv("FP_MODE", ""), "vm")) {
      # In VM we purposely report NO mounts so nothing tries to translate paths
      return(data.frame(remote_path=character(), local_path=character(), stringsAsFactors=FALSE))
    }

    if (is_vm_mode()) {
      return(data.frame(remote_path=character(), local_path=character(), stringsAsFactors=FALSE))
    }

    # Define the path to the .fp_mount.dat file
    fp_mount_file <- path.expand("~/.fp_mount.dat")

    # Initialize a data frame to store the parsed paths
    mount_df <- data.frame(remote_path = character(), local_path = character(), stringsAsFactors = FALSE)

    # Function to parse a single line into remote_path and local_path
    parse_mount_line <- function(line) {
      path_regex <- "(^.+):(/juno/[^ ]+) on ([^ ]+) "
      matches <- regmatches(line, regexec(path_regex, line))
      if (length(matches[[1]]) == 4) {  # Ensure we have the correct number of parts
        remote_path <- ensure_trailing_slash(matches[[1]][3])
        local_path <- ensure_trailing_slash(matches[[1]][4])
        return(data.frame(remote_path = remote_path, local_path = local_path, stringsAsFactors = FALSE))
      }
      return(NULL)
    }

    # Check if ~/.fp_mount.dat exists and read it if it does
    if (file.exists(fp_mount_file)) {
      file_output <- readLines(fp_mount_file)
      for (line in file_output) {
        if (grepl("/juno/work/", line)) {
          parsed_data <- parse_mount_line(line)
          if (!is.null(parsed_data)) {
            mount_df <- rbind(mount_df, parsed_data)
          }
        }
      }
    }

    # Execute the "mount" command and capture its output
    mount_output <- system("mount", intern = TRUE)

    # Loop through each line in the mount output and append to the data frame
    for (line in mount_output) {
      if (grepl("/juno/work/", line)) {
        parsed_data <- parse_mount_line(line)
        if (!is.null(parsed_data)) {
          mount_df <- rbind(mount_df, parsed_data)
        }
      }
    }

    #print(mount_df)
    return(mount_df)
  }

  # Function to normalize paths and update input fields so we can handle relative paths.
  normalize_and_update_path <- function(input_id) {
    path <- input[[input_id]]
    if (startsWith(path, "~")) {
      path <- normalizePath(path, mustWork = FALSE)
    }
    path <- clean_and_ensure_trailing_slash(path)
    updateTextInput(session, input_id, value = path)
    return(path)
  }


  observeEvent(input$update_session, {
    # -------- helpers --------
    `%||%` <- function(a, b) if (!is.null(a) && nzchar(a)) a else b
    is_vm_mode <- function() identical(Sys.getenv("FP_MODE", ""), "vm")

    # Use your normalizer if present to reflect cleaned values back into the UI
    norm <- function(id) {
      if (exists("normalize_and_update_path", mode = "function")) {
        return(normalize_and_update_path(id))
      }
      input[[id]] %||% ""
    }

    is_valid_path <- function(p) {
      p <- p %||% ""
      if (!nzchar(p)) return(FALSE)
      if (exists("validate_path", mode = "function")) {
        return(isTRUE(tryCatch(validate_path(p), error = function(e) FALSE)))
      }
      dir.exists(p) || file.exists(p)
    }

    # Restricted roots:
    # - VM: come from global.config (put into options("fp.restricted_paths") during init)
    # - Local: your legacy restricted_paths vector (if present), else sane default
    restricted_roots <- if (is_vm_mode()) {
      getOption("fp.restricted_paths", default = character(0))
    } else {
      if (exists("get_restricted_paths", mode = "function")) get_restricted_paths() else {
        if (exists("restricted_paths")) restricted_paths else c("/juno/work/ccs/shared/resources/")
      }
    }

    is_restricted <- function(p) {
      if (!nzchar(p) || !length(restricted_roots)) return(FALSE)
      pp <- normalizePath(p, mustWork = FALSE)
      any(startsWith(pp, normalizePath(restricted_roots, mustWork = FALSE)))
    }

    # VM repo defaults placed into options("fp.default_repo_paths") during init
    get_vm_repo_defaults <- function() {
      d <- getOption("fp.default_repo_paths", default = NULL)
      if (is.null(d)) list(impact = "", tcga = "", tempo = "") else d
    }

    # -------- read + normalize inputs; reflect to UI --------
    personal_storage_path  <- norm("personal_storage_path")
    mount_refit_path       <- norm("mount_refit_path")
    remote_refit_path      <- norm("remote_refit_path")

    repository_path_impact <- norm("repository_path_impact")
    remote_path_impact     <- norm("remote_path_impact")

    repository_path_tempo  <- norm("repository_path_tempo")
    remote_path_tempo      <- norm("remote_path_tempo")

    repository_path_tcga   <- norm("repository_path_tcga")
    remote_path_tcga       <- norm("remote_path_tcga")

    # Mirror normalized values (no-ops if norm() already did it)
    updateTextInput(session, "personal_storage_path",  value = personal_storage_path)
    updateTextInput(session, "mount_refit_path",       value = mount_refit_path)
    updateTextInput(session, "remote_refit_path",      value = remote_refit_path)

    updateTextInput(session, "repository_path_impact", value = repository_path_impact)
    updateTextInput(session, "remote_path_impact",     value = remote_path_impact)

    updateTextInput(session, "repository_path_tempo",  value = repository_path_tempo)
    updateTextInput(session, "remote_path_tempo",      value = remote_path_tempo)

    updateTextInput(session, "repository_path_tcga",   value = repository_path_tcga)
    updateTextInput(session, "remote_path_tcga",       value = remote_path_tcga)

    # Current toggle states
    sw_use_mount_impact <- isTRUE(input$session_switch_impact)
    sw_use_mount_tempo  <- isTRUE(input$session_switch_tempo)
    sw_use_mount_tcga   <- isTRUE(input$session_switch_tcga)
    sw_use_mount_refit  <- isTRUE(input$session_remote_refit)

    auth_password <- input$auth_password %||% ""

    # -------- VM short-circuit: fill from global.config; identity mapping; switches OFF --------
    if (is_vm_mode()) {
      defs <- get_vm_repo_defaults()

      if (!nzchar(repository_path_impact)) repository_path_impact <- defs$impact
      if (!nzchar(repository_path_tcga))   repository_path_tcga   <- defs$tcga
      if (!nzchar(repository_path_tempo))  repository_path_tempo  <- defs$tempo

      # Remote == Local in VM
      remote_path_impact <- repository_path_impact
      remote_path_tcga   <- repository_path_tcga
      remote_path_tempo  <- repository_path_tempo
      remote_refit_path  <- mount_refit_path

      # Force all switches OFF in VM
      sw_use_mount_impact <- FALSE
      sw_use_mount_tempo  <- FALSE
      sw_use_mount_tcga   <- FALSE
      sw_use_mount_refit  <- FALSE
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)
      shinyWidgets::updateSwitchInput(session, "session_switch_tempo",  value = FALSE)
      shinyWidgets::updateSwitchInput(session, "session_switch_tcga",   value = FALSE)
      shinyWidgets::updateSwitchInput(session, "session_remote_refit",  value = FALSE)

      # Reflect possibly-filled defaults
      updateTextInput(session, "repository_path_impact", value = repository_path_impact)
      updateTextInput(session, "remote_path_impact",     value = remote_path_impact)
      updateTextInput(session, "repository_path_tempo",  value = repository_path_tempo)
      updateTextInput(session, "remote_path_tempo",      value = remote_path_tempo)
      updateTextInput(session, "repository_path_tcga",   value = repository_path_tcga)
      updateTextInput(session, "remote_path_tcga",       value = remote_path_tcga)
      updateTextInput(session, "remote_refit_path",      value = remote_refit_path)

    } else {
      # -------- LOCAL: auto-detect mounts; if repo == a local mount root, flip switch ON & fill remote --------
      if (exists("get_mount_info", mode = "function")) {
        mt <- tryCatch(get_mount_info(), error = function(e) NULL)
        if (is.data.frame(mt) && nrow(mt) > 0) {
          # Helper: if local repo matches a row, flip ON and set remote
          promote_map <- function(repo_path, remote_id, switch_id, switch_flag_name) {
            if (!nzchar(repo_path)) return(invisible(NULL))
            hit <- which(mt$local_path == repo_path)[1]
            if (length(hit) == 1 && !is.na(hit)) {
              rp <- mt$remote_path[hit] %||% ""
              if (nzchar(rp)) {
                updateTextInput(session, remote_id, value = rp)
                shinyWidgets::updateSwitchInput(session, switch_id, value = TRUE)
                # set the local variable switch flags too
                if (switch_flag_name == "impact") sw_use_mount_impact <<- TRUE
                if (switch_flag_name == "tempo")  sw_use_mount_tempo  <<- TRUE
                if (switch_flag_name == "tcga")   sw_use_mount_tcga   <<- TRUE
                if (switch_flag_name == "refit")  sw_use_mount_refit  <<- TRUE
                assign(paste0("remote_path_", switch_flag_name), rp, inherits = TRUE)
              }
            }
          }

          promote_map(repository_path_impact, "remote_path_impact", "session_switch_impact", "impact")
          promote_map(repository_path_tempo,  "remote_path_tempo",  "session_switch_tempo",  "tempo")
          promote_map(repository_path_tcga,   "remote_path_tcga",   "session_switch_tcga",   "tcga")
          promote_map(mount_refit_path,       "remote_refit_path",  "session_remote_refit",  "refit")
        }
      }
    }

    # -------- VALIDATIONS --------
    # Personal storage: allow empty. If provided in LOCAL mode, it must NOT be a mounted remote.
    valid_personal_storage <- (personal_storage_path == "") || is_valid_path(personal_storage_path)
    if (!is_vm_mode() && valid_personal_storage && exists("is_remote_file", mode = "function")) {
      if (isTRUE(tryCatch(is_remote_file(personal_storage_path), error = function(e) FALSE))) {
        showNotification("Personal repository cannot be a mounted directory. Use a local directory instead.",
                         type = "error")
        personal_storage_path <- ""
        updateTextInput(session, "personal_storage_path", value = "")
        valid_personal_storage <- TRUE
      }
    }

    # For each repo group in LOCAL mode:
    # - If Use Mount is OFF, both repo & remote are considered valid (even empty).
    # - If Use Mount is ON, both repo and remote must be non-empty & valid paths.
    # In VM mode we already short-circuited: remote mirrors repo and switches are OFF.
    validate_pair <- function(repo, remote, sw_on) {
      if (!sw_on) return(list(repo = TRUE, remote = TRUE))
      list(
        repo   = nzchar(repo)   && is_valid_path(repo),
        remote = nzchar(remote) && is_valid_path(remote)
      )
    }

    if (!is_vm_mode()) {
      vi <- validate_pair(repository_path_impact, remote_path_impact, sw_use_mount_impact)
      vt <- validate_pair(repository_path_tempo,  remote_path_tempo,  sw_use_mount_tempo)
      vc <- validate_pair(repository_path_tcga,   remote_path_tcga,   sw_use_mount_tcga)
      vr <- validate_pair(mount_refit_path,       remote_refit_path,  sw_use_mount_refit)
    } else {
      # VM: lenient (switches are OFF anyway). Just check repo exists if provided.
      vi <- list(repo = (repository_path_impact == "" || is_valid_path(repository_path_impact)), remote = TRUE)
      vt <- list(repo = (repository_path_tempo  == "" || is_valid_path(repository_path_tempo)),  remote = TRUE)
      vc <- list(repo = (repository_path_tcga   == "" || is_valid_path(repository_path_tcga)),   remote = TRUE)
      vr <- list(repo = is_valid_path(mount_refit_path) || (mount_refit_path == ""),
                 remote = TRUE)
    }

    # Build error message if anything invalid
    if (!all(c(
      valid_personal_storage,
      vi$repo, vi$remote, vt$repo, vt$remote, vc$repo, vc$remote, vr$repo, vr$remote
    ))) {
      msg <- paste(
        c(
          if (!valid_personal_storage) "Invalid or missing local storage path." else NULL,
          if (!vi$repo) "Invalid or missing IMPACT repository path." else NULL,
          if (!vi$remote) "Invalid or missing IMPACT remote path when Use Mount is enabled." else NULL,
          if (!vt$repo) "Invalid or missing TEMPO repository path." else NULL,
          if (!vt$remote) "Invalid or missing TEMPO remote path when Use Mount is enabled." else NULL,
          if (!vc$repo) "Invalid or missing TCGA repository path." else NULL,
          if (!vc$remote) "Invalid or missing TCGA remote path when Use Mount is enabled." else NULL,
          if (!vr$repo) "Invalid or missing mount refit path." else NULL,
          if (!vr$remote) "Invalid or missing remote refit path when Use Mount is enabled." else NULL
        ),
        collapse = "\n"
      )
      showNotification(msg, type = "error", duration = 8)
      return()
    }

    # -------- RESTRICTED PATHS + PASSWORD (LOCAL ONLY) --------
    restricted_hits <- character(0)
    if (!is_vm_mode()) {
      if (sw_use_mount_impact && nzchar(remote_path_impact) && is_restricted(remote_path_impact)) {
        restricted_hits <- c(restricted_hits, "IMPACT remote")
      }
      if (sw_use_mount_tempo && nzchar(remote_path_tempo) && is_restricted(remote_path_tempo)) {
        restricted_hits <- c(restricted_hits, "TEMPO remote")
      }
      if (sw_use_mount_tcga && nzchar(remote_path_tcga) && is_restricted(remote_path_tcga)) {
        restricted_hits <- c(restricted_hits, "TCGA remote")
      }
      if (sw_use_mount_refit && nzchar(remote_refit_path) && is_restricted(remote_refit_path)) {
        restricted_hits <- c(restricted_hits, "Refit remote")
      }

      # Your original password behavior: only check if a password was provided AND any Use Mount is ON
      if (nzchar(auth_password) && (sw_use_mount_impact || sw_use_mount_tempo || sw_use_mount_tcga)) {
        hashed_password <- digest::digest(auth_password, algo = "sha256")

        session_data$auth_password     <- hashed_password
        session_data$password_valid    <- if (identical(hashed_password, valid_hashed_password)) 1 else 0
        session_data$password_personal <- if (identical(hashed_password, valid_personal_password)) 1 else 0

        if (session_data$password_valid == 1) {
          showNotification("Authenticated for full access.", type = "message")
        }
        if (session_data$password_personal == 1) {
          showNotification("Authenticated for personal refits.", type = "message")
        }
        if (session_data$password_valid != 1 && session_data$password_personal != 1) {
          showNotification("Invalid password.", type = "error")
        }
      } else {
        session_data$password_valid    <- 0
        session_data$password_personal <- 0
      }

      # If there are restricted hits and the user is NOT authenticated, show a single error
      if (length(restricted_hits) > 0 && session_data$password_valid != 1) {
        showNotification(
          paste0("Restricted path(s): ", paste(restricted_hits, collapse = ", "),
                 ". Authenticate on the Session tab to enable access."),
          type = "error", duration = 8
        )
        # We still allow saving the session file below; this just informs about restricted areas.
      }
    } else {
      # VM: reset auth flags (no sshfs / restricted checks here for now)
      session_data$password_valid    <- 0
      session_data$password_personal <- 0
    }

    # -------- persist reactive + write TSV session file --------
    session_data$personal_storage_path <- personal_storage_path

    session_data$mount_refit_path       <- mount_refit_path
    session_data$remote_refit_path      <- remote_refit_path
    session_data$session_remote_refit   <- sw_use_mount_refit

    session_data$repository_path_impact <- repository_path_impact
    session_data$remote_path_impact     <- remote_path_impact
    session_data$session_switch_impact  <- sw_use_mount_impact

    session_data$repository_path_tempo  <- repository_path_tempo
    session_data$remote_path_tempo      <- remote_path_tempo
    session_data$session_switch_tempo   <- sw_use_mount_tempo

    session_data$repository_path_tcga   <- repository_path_tcga
    session_data$remote_path_tcga       <- remote_path_tcga
    session_data$session_switch_tcga    <- sw_use_mount_tcga

    # Store plain text password in the TSV like before (your reader re-hashes later where needed)
    session_data$auth_password <- auth_password

    out_df <- data.frame(
      personal_storage_path = session_data$personal_storage_path %||% "",
      mount_refit_path      = session_data$mount_refit_path %||% "",
      remote_refit_path     = session_data$remote_refit_path %||% "",
      session_remote_refit  = session_data$session_remote_refit %||% FALSE,

      repository_path_impact = session_data$repository_path_impact %||% "",
      remote_path_impact     = session_data$remote_path_impact %||% "",
      session_switch_impact  = session_data$session_switch_impact %||% FALSE,

      repository_path_tempo  = session_data$repository_path_tempo %||% "",
      remote_path_tempo      = session_data$remote_path_tempo %||% "",
      session_switch_tempo   = session_data$session_switch_tempo %||% FALSE,

      repository_path_tcga   = session_data$repository_path_tcga %||% "",
      remote_path_tcga       = session_data$remote_path_tcga %||% "",
      session_switch_tcga    = session_data$session_switch_tcga %||% FALSE,

      auth_password          = session_data$auth_password %||% "",
      password_valid         = session_data$password_valid %||% 0,
      password_personal      = session_data$password_personal %||% 0,
      stringsAsFactors = FALSE
    )

    write.table(out_df,
                file = fp_session_path(),
                sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

    showNotification(paste0("Session data saved to ", fp_session_path()),
                     type = "message", duration = 3)
  })





  observeEvent(input$continue_session, {
    # Simulate a click on the update_session button
    shinyjs::click("update_session")

    if (is_vm_mode()) {
      updateTextInput(session, "remote_path_impact", value = input$repository_path_impact %||% "")
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)

      session_data$repository_path_impact <- input$repository_path_impact %||% ""
      session_data$remote_path_impact     <- input$repository_path_impact %||% ""
      session_data$session_switch_impact  <- FALSE
    }

    # Move to the next tab after a short delay to ensure the update_session logic runs first
    later::later(function() {
      updateTabsetPanel(session, "navbarPage1", selected = "tabPanel_sampleInput")
    }, delay = 0.1)  # Adjust the delay as necessary
  })

  observeEvent(input$personal_storage_path, {
    personal_storage_path <- input$personal_storage_path

    if (personal_storage_path == "") {
      shinyjs::hide("invalid_path_message")
      return(NULL)
    }

    # Validate the path and show/hide the error message and create folder button
    if (!validate_path(personal_storage_path) || !dir.exists(personal_storage_path)) {
      shinyjs::show("invalid_path_message")
      shinyjs::show("create_folder_button_container")
    } else {
      shinyjs::hide("invalid_path_message")
      shinyjs::hide("create_folder_button_container")
    }
  })


  set_default_countFile <- function() {
    selected_run <- get_selected_run(values$sample_runs)

    run_path <- selected_run$path[1]
    sample_id <- input$selectInput_selectSample

    # Update the select counts file button.
    roots <- c(current_run = run_path)
    shinyFiles::shinyFileChoose(input, "fileInput_pileup", roots = roots, filetypes = c('dat', 'gz'))

    # Try to find files that match the expected pattern for countsMerged files
    counts_file_name <- glue::glue("{run_path}/countsMerged____{sample_id}.dat.gz")

    if (file.exists(counts_file_name)) {
      # Set the selected counts file path in the reactive value if the exact file exists
      selected_counts_file(counts_file_name)
    } else {
      # If no file matches the exact pattern, look for any file with "count" in the name
      files_in_directory <- list.files(run_path, pattern = "count", full.names = TRUE, ignore.case = TRUE)

      if (length(files_in_directory) > 0) {
        # Use the first matching file
        selected_counts_file(files_in_directory[1])
      } else {
        # If no "count" file is found, look for any .gz file
        gz_files_in_directory <- list.files(run_path, pattern = "\\.gz$", full.names = TRUE)

        if (length(gz_files_in_directory) > 0) {
          # Use the first .gz file if available
          selected_counts_file(gz_files_in_directory[1])
        } else {
          # If no .gz file is found, use any file in the directory
          all_files_in_directory <- list.files(run_path, full.names = TRUE)

          if (length(all_files_in_directory) > 0) {
            # Use the first file in the directory as a fallback
            selected_counts_file(all_files_in_directory[1])
          }

          # Show a notification that no suitable counts files were found, at this point its the user's problem.
          showNotification(paste0("No suitable countsMerged or .gz files found in the run directory at ",run_path), type = "warning")
        }
      }
    }
  }


  get_selected_run <- function(sample_runs) {
    # Check the number of rows in sample_runs
    if (nrow(sample_runs) == 1) {
      # If only one row, return that one
      selected_run <- sample_runs
    } else {
      # If more than one row, try to filter for 'default'
      selected_run <- sample_runs %>%
        filter(fit_name == 'default') %>%
        head(n = 1)

      # If no exact 'default' match, look for any fit_name containing 'default'
      if (nrow(selected_run) == 0) {
        selected_run <- sample_runs %>%
          filter(grepl('default', fit_name, ignore.case = TRUE)) %>%
          head(n = 1)
      }

      # If still no match, use the first row
      if (nrow(selected_run) == 0) {
        selected_run <- sample_runs %>% head(n = 1)
      }
    }

    return(selected_run)
  }


  observeEvent(input$fileInput_pileup, {
    selected_run <- get_selected_run(values$sample_runs)


    # Check if the selected row has a valid path and is not NA
    if (!is.null(selected_run$path) && length(selected_run$path) > 0 && !is.na(selected_run$path[1])) {
      run_path <- selected_run$path[1]  # Get the first path
      # Set up shinyFileChoose with the current run path as the root
      shinyFiles::shinyFileChoose(input, "fileInput_pileup", roots = c(current_run = run_path), session = session)

      # Parse the file path selected by the user
      file_info <- shinyFiles::parseFilePaths(roots = c(current_run = run_path), input$fileInput_pileup)

      # Extract the file path as a character string
      file_path <- as.character(file_info$datapath)

      # Check if file_path is valid and update the reactive value
      if (!is.null(file_path) && length(file_path) > 0 && file_path != "") {
        selected_counts_file(file_path)  # Set the selected file path
        showNotification(paste("Selected counts file is", selected_counts_file()), type = "message")
      }
    } else {
      showNotification("No valid run path found", type = "error")
    }
  })




  observeEvent(input$mount_refit_path, {
    mount_refit_path <- input$mount_refit_path

    # Return immediately if the repository path is empty
    if (mount_refit_path == "") {
      return(NULL)
    }

    # Normalize the mount_refit_path by removing any trailing slash
    normalized_mount_refit_path <- sub("/+$", "", mount_refit_path)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    matched_row <- mount_df[mount_df$normalized_local_path == normalized_mount_refit_path, ]

    if (nrow(matched_row) > 0) {
      updateTextInput(session, "remote_refit_path", value = matched_row$remote_path)
      session_data$remote_refit_path <- matched_row$remote_path
      shinyWidgets::updateSwitchInput(session, "session_remote_refit", value = TRUE)
    } else {
      updateTextInput(session, "remote_refit_path", value = "")
      session_data$remote_refit_path <- ""
      shinyWidgets::updateSwitchInput(session, "session_remote_refit", value = FALSE)
    }
  })


  observeEvent(input$repository_path_impact, {
    repository_path_impact <- input$repository_path_impact

    # Return immediately if the repository path is empty
    if (repository_path_impact == "") {
      return(NULL)
    }

    # Normalize the repository_path_impact by removing any trailing slash
    normalized_repository_path_impact <- sub("/+$", "", repository_path_impact)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)
    print(mount_df)

    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_impact, ]


    if (nrow(matched_row) > 0) {
      updateTextInput(session, "remote_path_impact", value = matched_row$remote_path)
      session_data$remote_path_impact <- matched_row$remote_path
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = TRUE)
      print("No matched rows")
    }
    else
    {
      updateTextInput(session, "remote_path_impact", value = "")
      session_data$remote_path_impact <- ""
      shinyWidgets::updateSwitchInput(session, "session_switch_impact", value = FALSE)
    }
  })

  observeEvent(input$repository_path_tempo, {
    repository_path_tempo <- input$repository_path_tempo

    # Return immediately if the repository path is empty
    if (repository_path_tempo == "") {
      return(NULL)
    }

    # Normalize the repository_path_tempo by removing any trailing slash
    normalized_repository_path_tempo <- sub("/+$", "", repository_path_tempo)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_tempo, ]

    if (nrow(matched_row) > 0) {
      updateTextInput(session, "remote_path_tempo", value = matched_row$remote_path)
      session_data$remote_path_tempo <- matched_row$remote_path
      shinyWidgets::updateSwitchInput(session, "session_switch_tempo", value = TRUE)
    }
    else
    {
      updateTextInput(session, "remote_path_tempo", value = "")
      session_data$remote_path_tempo <- ""
      shinyWidgets::updateSwitchInput(session, "session_switch_tempo", value = FALSE)
    }
  })

  observeEvent(input$repository_path_tcga, {
    repository_path_tcga <- input$repository_path_tcga

    # Return immediately if the repository path is empty
    if (repository_path_tcga == "") {
      return(NULL)
    }

    # Normalize the repository_path_tcga by removing any trailing slash
    normalized_repository_path_tcga <- sub("/+$", "", repository_path_tcga)
    mount_df <- get_mount_info()
    mount_df$normalized_local_path <- sub("/+$", "", mount_df$local_path)

    matched_row <- mount_df[mount_df$normalized_local_path == normalized_repository_path_tcga, ]

    if (nrow(matched_row) > 0) {
      updateTextInput(session, "remote_path_tcga", value = matched_row$remote_path)
      session_data$remote_path_tcga <- matched_row$remote_path
      shinyWidgets::updateSwitchInput(session, "session_switch_tcga", value = TRUE)
    }
    else
    {
      updateTextInput(session, "remote_path_tcga", value = "")
      session_data$remote_path_tcga <- ""
      shinyWidgets::updateSwitchInput(session, "session_switch_tcga", value = FALSE)
    }
  })


  observeEvent(input$button_impactSamplesInput, {

    # Define the regex pattern for valid full input format
    valid_input_pattern_full <- "^[A-Z]-\\d{7}-T\\d{2}-IM\\d{1}_[A-Z]-\\d{7}-N\\d{2}-IM\\d{1}$"
    # Define the regex pattern for valid shortened input format
    valid_input_pattern_short <- "^[A-Z]-\\d{7}-T\\d{2}-IM\\d{1}$"

    # Get the input from the textAreaInput
    impact_samples_input <- input$textAreaInput_impactSamplesInput

    # Split the input string by comma, tab, space, or newline into individual sample IDs
    sample_ids <- unlist(strsplit(impact_samples_input, "[,\t \n]+"))

    # Check if remote_path_impact is not empty
    if (nzchar(session_data$remote_path_impact)) {
      combined_values <- list()

      for (sample_id in sample_ids) {

        # Trim leading/trailing whitespace from sample_id
        sample_id <- trimws(sample_id)

        # Skip if sample_id is empty
        if (nzchar(sample_id)) {

          # Check if the sample ID matches either the full or shortened format
          if (grepl(valid_input_pattern_full, sample_id) || grepl(valid_input_pattern_short, sample_id)) {

            # Extract the first 7 characters from the input string
            extracted_part <- substr(sample_id, 1, 7)

            # Build the search path
            search_path <- paste0(session_data$repository_path_impact, "/all/", extracted_part, "/")

            # Check if the sample ID is in the full format
            if (grepl(valid_input_pattern_full, sample_id)) {
              # If it's in full format, use it directly
              combined_value <- paste0(search_path, sample_id)

              # Verify that the folder exists for the full format sample ID
              if (!dir.exists(combined_value)) {
                showNotification(paste("Folder does not exist for:", sample_id), type = "error", duration = 5)
                next  # Skip to the next sample_id
              }

            } else if (grepl(valid_input_pattern_short, sample_id)) {
              # If it's in shortened format, search for matching folders

              matching_folders <- list.dirs(search_path, full.names = FALSE, recursive = FALSE)
              found_folder <- matching_folders[grepl(paste0("^", sample_id), matching_folders)]

              # Check if we found at least one matching folder
              if (length(found_folder) > 0) {
                # Use the first match found
                combined_value <- paste0(search_path, found_folder[1])
              } else {
                showNotification(paste("No matching folder found for:", sample_id), type = "error", duration = 5)
                next  # Skip to the next sample_id
              }
            }

            combined_values <- append(combined_values, combined_value)

          } else {
            showNotification(paste("Invalid format for sample ID:", sample_id), type = "error", duration = 5)
            next  # Skip to the next sample_id
          }
        }
      }

      # Proceed with further processing of combined_values if any valid ones were found
      if (length(combined_values) > 0) {
        current_paths <- unlist(strsplit(input$textAreaInput_samplesInput, "[,\t \n]+"))
        all_paths <- unique(c(current_paths, combined_values))
        updateTextAreaInput(session, "textAreaInput_samplesInput", value = paste(all_paths, collapse = "\n"))
      }

    } else {
      showNotification("No repository loaded! Please choose a repository before retrieving samples.", type = "error", duration = 5)
    }
  })


  observeEvent(input$button_tcgaSamplesInput, {

    # Define the regex pattern for valid full input format (long form)
    valid_input_pattern_full_tcga <- "^[A-Z0-9\\-]+_[A-Z0-9\\-]+$"
    # Define the regex pattern for valid shortened input format (short form)
    valid_input_pattern_short_tcga <- "^[A-Z0-9\\-]+$"

    # Get the input from the textAreaInput
    tcga_samples_input <- input$textAreaInput_tcgaSamplesInput

    # Split the input string by comma, tab, space, or newline into individual sample IDs
    sample_ids <- unlist(strsplit(tcga_samples_input, "[,\t \n]+"))

    # Check if remote_path_tcga is not empty
    if (nzchar(session_data$remote_path_tcga)) {

      combined_values <- list()

      # Loop over each sample ID
      for (sample_id in sample_ids) {

        # Trim leading/trailing whitespace from sample_id
        sample_id <- trimws(sample_id)

        # Skip if sample_id is empty
        if (nzchar(sample_id)) {

          # Check if the sample ID matches either the full or shortened format
          if (grepl(valid_input_pattern_full_tcga, sample_id) || grepl(valid_input_pattern_short_tcga, sample_id)) {

            # Extract the first part of the input string (until the second hyphen).
            extracted_part <- sub("^([A-Z0-9]+-[A-Z0-9]+).*", "\\1", sample_id)

            # Build the search path
            search_path <- paste0(session_data$repository_path_tcga, "/all3/", extracted_part, "/")

            # Check if the sample ID is in the full format
            if (grepl(valid_input_pattern_full_tcga, sample_id)) {
              # If it's in full format, use it directly
              combined_value <- paste0(search_path, sample_id)

              # Verify that the folder exists for the full format sample ID
              if (!dir.exists(combined_value)) {
                showNotification(paste("Folder does not exist for:", sample_id), type = "error", duration = 5)
                next  # Skip to the next sample_id
              }

            } else if (grepl(valid_input_pattern_short_tcga, sample_id)) {
              # If it's in shortened format, search for matching folders

              # List the directories in the search_path
              matching_folders <- list.dirs(search_path, full.names = FALSE, recursive = FALSE)

              # Find any folder that starts with the shortened input
              found_folder <- matching_folders[grepl(paste0("^", sample_id), matching_folders)]

              # Check if we found at least one matching folder
              if (length(found_folder) > 0) {
                # Use the first match found
                combined_value <- paste0(search_path, found_folder[1])
              } else {
                showNotification(paste("No matching folder found for:", sample_id), type = "error", duration = 5)
                next  # Skip to the next sample_id
              }
            }

            # Store the combined value in the list
            combined_values <- append(combined_values, combined_value)

          } else {
            showNotification(paste("Invalid format for sample ID:", sample_id), type = "error", duration = 5)
            next  # Skip to the next sample_id
          }
        }
      }

      # Proceed with further processing of combined_values if any valid ones were found
      if (length(combined_values) > 0) {
        current_paths <- unlist(strsplit(input$textAreaInput_samplesInput, "[,\t \n]+"))
        all_paths <- unique(c(current_paths, combined_values))
        updateTextAreaInput(session, "textAreaInput_samplesInput", value = paste(all_paths, collapse = "\n"))
      }

    } else {
      showNotification("No repository loaded! Please choose a repository before retrieving samples.", type = "error", duration = 5)
    }
  })



  library(httr)

  # GitHub base URL for the help files
  base_url <- "https://raw.githubusercontent.com/mskcc/facets-preview/master/www/help_files/"
  api_url <- "https://api.github.com/repos/mskcc/facets-preview/contents/www/help_files"

  # Function to fetch the list of HTML files from the GitHub repository
  get_help_files <- function(api_url) {
    response <- httr::GET(api_url)

    if (httr::status_code(response) == 200) {
      # Parse the JSON response into a structured list
      files_info <- jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"))

      # Filter and return only HTML files
      html_files <- files_info$name[grep("\\.html$", files_info$name)]

      return(html_files)
    } else {
      return(NULL)  # Return NULL in case of an error
    }
  }

  # Dynamically generate the list of help files
  help_files <- get_help_files(api_url)

  # Dynamically generate links for each HTML file from GitHub
  output$help_links <- renderUI({
    if (is.null(help_files) || length(help_files) == 0) {
      return(HTML("<p>Error: Could not fetch help files or no HTML files found.</p>"))
    }

    # Create a link for each HTML file
    links <- lapply(help_files, function(file) {
      file_name <- gsub("_", " ", tools::file_path_sans_ext(basename(file)))  # Create readable link text
      tags$li(
        actionLink(inputId = file, label = file_name, style = "cursor: pointer;")
      )
    })

    # Return the unordered list of links with increased font size
    tags$ul(
      style = "font-size: 18px;",  # Increase font size
      do.call(tagList, links)
    )
  })

  # Create an observer for each file link
  observe({
    lapply(help_files, function(file) {
      observeEvent(input[[file]], {
        # Construct the full URL for the file on GitHub
        file_url <- paste0(base_url, file)

        # Try fetching the file from the GitHub URL
        response <- httr::GET(file_url)

        if (httr::status_code(response) == 200) {
          # If the request was successful, get the content and display it
          help_content <- content(response, as = "text", encoding = "UTF-8")

          # Display the HTML content in the help_content UI
          output$help_content <- renderUI({
            HTML(help_content)
          })
        } else {
          # Handle error if the file could not be fetched
          output$help_content <- renderUI({
            HTML("<p>Error: Help file could not be loaded.</p>")
          })
        }
      })
    })
  })


  observeEvent(input$button_refit, {

    if (input$selectInput_selectFit == "Not selected") {
      showModal(modalDialog(
        title = "Cannot submit refit", paste0("select 'any' fit first and then click 'Run'")
      ))
      return(NULL)
    }

    # Enforce required values for all parameters.
    if (input$textInput_newPurityCval == "" || input$textInput_newHisensCval == "" ||
        input$textInput_newPurityMinNHet == "" || input$textInput_newHisensMinNHet == "" ||
        input$textInput_newNormalDepth == "" || input$textInput_newSnpWindowSize == "" ||
        input$selectInput_newFacetsLib == "") {
      showModal(modalDialog(
        title = "Cannot submit refit", "All refit parameters are required."
      ))
      return(NULL)
    }

    # make sure all the parameters are numeric
    if (!suppressWarnings(all(!is.na(as.numeric(c(input$textInput_newPurityCval,
                                                 input$textInput_newHisensCval,
                                                 input$textInput_newPurityMinNHet,
                                                 input$textInput_newHisensMinNHet,
                                                 input$textInput_newNormalDepth,
                                                 input$textInput_newSnpWindowSize)))))) {
      showModal(modalDialog(
        title = "Cannot submit refit", paste0("Non-numeric characters are found in re-fit parameters")
      ))
      return(NULL)
    }

    with_dipLogR = T
    refit_note = ""
    if (input$textInput_newDipLogR == "") {
      with_dipLogR = F
      refit_note = "Refit job is submitted without a dipLogR and therefore will be determined by purity run."
    }

    sample_id = values$sample_runs$tumor_sample_id[1]

    ## get best fit if exists; other-wise default
    selected_run <- get_selected_run(values$sample_runs)
    #selected_run = values$sample_runs %>% filter(fit_name=='default') %>% head(n=1)

    #if (nrow(selected_run) == 0) {
    #  selected_run <- values$sample_runs[which(values$sample_runs$fit_name == paste0(input$selectInput_selectFit)),]
   # }

    mount_df <- get_mount_info()
    print(selected_run)

    # Check if selected_sample_path contains any local_path entries
    matched_row <- mount_df[sapply(mount_df$local_path, function(local_path) {
      grepl(local_path, selected_run$path[1])
    }), ]


    if (nrow(matched_row) > 0) {
      # Check if the remote_path contains "/juno/work/"
      if (is_restricted_path(matched_row$remote_path)) {
        if (session_data$password_valid == 1 || !is_remote_file(selected_sample_path)) {
          showNotification("Authorized Refit.", type = "message")
        } else {
          showNotification("You are not authorized to perform refits for this sample. Authenticate on the session tab to unlock.", type = "error")
          return(NULL)
        }
      }
    }

    run_path = selected_run$path[1]
    new_purity_c = input$textInput_newPurityCval
    new_hisens_c = input$textInput_newHisensCval
    new_purity_m = input$textInput_newPurityMinNHet
    new_hisens_m = input$textInput_newHisensMinNHet
    new_normal_depth = input$textInput_newNormalDepth
    new_snp_window_size = input$textInput_newSnpWindowSize
    new_facets_lib = input$selectInput_newFacetsLib
    new_diplogR = input$textInput_newDipLogR

    default_run_facets_version = selected_run$hisens_run_version[1]
    if(is.na(default_run_facets_version)) {
      default_run_facets_version = selected_run$purity_run_version[1]
    }

    facets_version_to_use = new_facets_lib
    if (grepl('use current', new_facets_lib)) {
      facets_version_to_use = default_run_facets_version
    }

    supported_facets_versions = values$config$facets_lib %>% data.table
    if (!(facets_version_to_use %in% supported_facets_versions$version)) {
      showModal(modalDialog(
        title="Not submitted",
        paste0("Current version of facets-preview does not support refits using facets version: ", facets_version_to_use)
      ))
      return(NULL)
    }

    name_tag = (paste0("c{new_hisens_c}_pc{new_purity_c}",
                       ifelse(with_dipLogR, '_diplogR_{new_diplogR}', ''),
                       ifelse(new_purity_m != selected_run$purity_run_nhet, '_pm{new_purity_m}', ''),
                       ifelse(new_hisens_m != selected_run$hisens_run_nhet, '_m{new_hisens_m}', ''),
                       ifelse(new_normal_depth != selected_run$purity_run_ndepth, '_nd{new_normal_depth}', ''),
                       ifelse(new_snp_window_size != selected_run$purity_run_snp_nbhd, '_n{new_snp_window_size}', ''),
                       ifelse(new_facets_lib != selected_run$purity_run_version, '_v{facets_version_to_use}', '')
    ))

    name_tag = glue(name_tag)
    refit_name <- glue('/refit_{name_tag}')

    cmd_script_pfx = paste0(run_path, "/refit_jobs/facets_refit_cmd_")

    refit_dir <- paste0(run_path, refit_name)

    facets_lib_path = supported_facets_versions[version==facets_version_to_use]$lib_path

    #counts_file_name = glue("{run_path}/countsMerged____{sample_id}.dat.gz")
    counts_file_name = selected_counts_file()
    if(is.null(counts_file_name))
    {
      set_default_countFile()
      counts_file_name = selected_counts_file()
    }

    refit_cmd_file <- glue("{cmd_script_pfx}{sample_id}_{name_tag}.sh")

    refit_cmd = glue(paste0('{values$config$r_script_path}  ',
                           '{values$config$facets_suite_run_wrapper} ',
                           '--facets-lib-path {facets_lib_path} ',
                           '--counts-file {counts_file_name} ',
                           '--sample-id {sample_id} ',
                           '--snp-window-size {new_snp_window_size} ',
                           '--normal-depth {new_normal_depth} ',
                           ifelse(with_dipLogR, '--dipLogR {new_diplogR} ', ''),
                           '--min-nhet {new_hisens_m} ',
                           '--purity-min-nhet {new_purity_m} ',
                           '--seed 100 ',
                           '--cval {new_hisens_c} --purity-cval {new_purity_c} --legacy-output T -e ',
                           '--genome hg19 --directory {refit_dir} '))



    # If the switch is on, submit as the LSF job.
    if (input$use_remote_refit_switch) {

      #Validate our inputs.
      if (!grepl("^\\d{1,2}:\\d{2}$", input$textInput_timeLimit)) {
        showNotification("Time Limit must be in the format H:MM.", type = "error")
        return(NULL)
      }

      if (!grepl("^\\d+$", input$textInput_cores)) {
        showNotification("Num. Cores must be an integer.", type = "error")
        return(NULL)
      }

      if (!grepl("^\\d+$", input$textInput_memory)) {
        showNotification("Memory must be an integer.", type = "error")
        return(NULL)
      }

      counts_file_name = selected_counts_file()
      if(is.null(counts_file_name))
      {
        set_default_countFile()
        counts_file_name = selected_counts_file()
      }
      refit_dir <- paste0(run_path, refit_name)

      #Build our command for submitting to bsub.
      counts_file_name = get_remote_path(counts_file_name)
      refit_dir_remote = get_remote_path(refit_dir)

      refit_cmd = glue(paste0('/opt/common/CentOS_7/R/R-3.6.3/bin/Rscript  ',
                              '{input$remote_refit_path}lib/facets-suite-2.0.8/run-facets-wrapper.R ',
                              '--facets-lib-path {input$remote_refit_path}lib/  ',
                              '--counts-file {counts_file_name} ',
                              '--sample-id {sample_id} ',
                              '--snp-window-size {new_snp_window_size} ',
                              '--normal-depth {new_normal_depth} ',
                              ifelse(with_dipLogR, '--dipLogR {new_diplogR} ', ''),
                              '--min-nhet {new_hisens_m} ',
                              '--purity-min-nhet {new_purity_m} ',
                              '--seed 100 ',
                              '--cval {new_hisens_c} --purity-cval {new_purity_c} --legacy-output T -e ',
                              '--genome hg19 --directory {refit_dir_remote} '))


      base_refit_name <- basename(refit_cmd_file)

      lsf_cmd <- glue('bsub -J "refit_{base_refit_name}" ',
                      '-R "rusage[mem={input$textInput_memory}G]" ',
                      '-We {input$textInput_timeLimit} ',
                      '-n {input$textInput_cores} ',
                      '-o {input$remote_refit_path}log/{base_refit_name}_bsub.out ',
                      '-e {input$remote_refit_path}log/{base_refit_name}_bsub.err ',
                      '{refit_cmd}')

      #Write the command to our listener queue directory.
      refit_cmd <- lsf_cmd
      output_file_path <- glue("{input$mount_refit_path}queue/refit_{base_refit_name}")
      writeLines(refit_cmd, con = output_file_path)
      system(glue("chmod 775 {output_file_path}"))
    }

    #print("REFITTT")
    print(refit_cmd)
    #print("REFITTT2")

    showModal(modalDialog(
      title = "Job submitted!",
      paste0(ifelse(refit_note != '', paste('Warning: ', refit_note, '\n\n'), ''),
             "Check back in a few minutes. Logs: ", refit_cmd_file, ".*")
    ))
    values$submitted_refit <- c(values$submitted_refit, refit_dir)

    if (!input$use_remote_refit_switch) {
      system(refit_cmd, intern = TRUE)
    }

    selected_counts_file()
  })

}
