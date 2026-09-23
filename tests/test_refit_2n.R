#!/usr/bin/env Rscript
### Fixture tests for the 2n refit path: reference-normal resolution and the
### flat -> clinical/research split that runs after the wrapper.
###
### Run: Rscript tests/test_refit_2n.R
### Exits non-zero on the first failure. Synthetic ids only -- no patient data.
###
### resolve_reference_normals_2n is defined inside server(), so it is lifted out
### of server.R by source text rather than sourced directly.

suppressPackageStartupMessages({ suppressWarnings({ library(glue) }) })

repo <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])), ".."))

src   <- readLines(file.path(repo, "inst", "application", "server.R"))
start <- grep("^  resolve_reference_normals_2n <- function", src)
stopifnot(length(start) == 1)
end   <- start + which(src[(start + 1):length(src)] == "  }")[1]
eval(parse(text = paste(src[start:end], collapse = "\n")), envir = globalenv())

n_pass <- 0; n_fail <- 0
check <- function(label, cond) {
  if (isTRUE(cond)) { n_pass <<- n_pass + 1; cat("PASS:", label, "\n") }
  else { n_fail <<- n_fail + 1; cat("FAIL:", label, "\n") }
}

## ---------------------------------------------------------------------------
## 1. Reference normal pools (keyed on the tumor id's assay + panel version)
## ---------------------------------------------------------------------------

lib <- tempfile("refit2n_lib_"); dir.create(lib)
for (f in c("cv6_solid_reference_normals_r1.snp_pileup.gz",
            "cv6_solid_reference_normals_r1.loess.txt",
            "cv6_picard_targets.interval_list",
            "cv4_heme_reference_normals_r1.snp_pileup.gz",
            "cv4_heme_reference_normals_r1.loess.txt",
            "cv4_heme_picard_targets.interval_list",
            "cv3_solid_reference_normals_r1.snp_pileup.gz",
            "cv3_solid_reference_normals_r1.loess.txt")) {
  invisible(file.create(file.path(lib, f)))
}

r <- resolve_reference_normals_2n("P-0000000-T01-IM6_P-0000000-N01-IM6", lib)
check("IM6 resolves the cv6 solid set", is.null(r$error) && r$panel == "solid" && r$version == "6")
check("solid falls back to the bare picard targets name",
      basename(r$targets) == "cv6_picard_targets.interval_list")

r2 <- resolve_reference_normals_2n("P-0000000-T01-IH4_P-0000000-N01-IH4", lib)
check("IH4 resolves the cv4 heme set", is.null(r2$error) && r2$panel == "heme" && r2$version == "4")
check("heme uses the panel-infixed targets",
      basename(r2$targets) == "cv4_heme_picard_targets.interval_list")

check("an incomplete set is an error, never a partial set",
      !is.null(resolve_reference_normals_2n("P-0000000-T01-IM3_P-0000000-N01-IM3", lib)$error))
check("heme is never offered the solid targets fallback",
      !is.null(resolve_reference_normals_2n("P-0000000-T01-IH9_P-0000000-N01-IH9", lib)$error))
check("an unparseable tumor id is reported, not guessed",
      grepl("assay/panel", resolve_reference_normals_2n("SOMETHING_ODD", lib)$error))

## ---------------------------------------------------------------------------
## 2. The split: flat 5-fit output -> <pair>/{clinical,research}/<fit>/
## ---------------------------------------------------------------------------

splitter <- file.path(repo, "inst", "scripts", "split_facets_2n.py")
check("the split script ships with the package", file.exists(splitter))

pair    <- tempfile("refit2n_pair_")
fit     <- "refit_c50_pc100"
staging <- file.path(pair, fit)
dir.create(staging, recursive = TRUE)

tag <- "T_SYNTH_N_SYNTH"
for (tok in c("_clinical_purity", "_clinical_hisens", "_research_purity",
              "_research_hisens", "_research_ultra_hisens")) {
  for (ext in c(".Rdata", ".cncf.txt", ".CNCF.png")) {
    invisible(file.create(file.path(staging, paste0(tag, tok, ext))))
  }
  writeLines(c("# INPUT PARAMETERS GIVEN", paste0("# TAG = ", tag, tok), "# cval = 50"),
             file.path(staging, paste0(tag, tok, ".out")))
}
invisible(file.create(file.path(staging, paste0(tag, ".clinical.gene_level.txt"))))
invisible(file.create(file.path(staging, paste0(tag, ".research.gene_level.txt"))))
invisible(file.create(file.path(staging, paste0(tag, ".facets2n_normal_selection.txt"))))

rc <- system2("python3", c(shQuote(splitter), shQuote(tag), shQuote(staging),
                           shQuote(pair), shQuote(fit)),
              stdout = NULL, stderr = NULL)
check("the split script runs cleanly", rc == 0)

cl <- file.path(pair, "clinical", fit)
rs <- file.path(pair, "research", fit)
ul <- file.path(rs, "ultra")

check("clinical subtree holds the de-infixed purity/hisens runs",
      all(file.exists(file.path(cl, paste0(tag, c("_purity.Rdata", "_hisens.Rdata",
                                                  "_purity.out", "_hisens.out"))))))
check("research subtree holds the de-infixed purity/hisens runs",
      all(file.exists(file.path(rs, paste0(tag, c("_purity.Rdata", "_hisens.Rdata"))))))
check("ultra is nested under the research fit, hisens only",
      file.exists(file.path(ul, paste0(tag, "_hisens.Rdata"))) &&
        !file.exists(file.path(ul, paste0(tag, "_purity.Rdata"))))
check("class-infixed aggregates lose their infix",
      file.exists(file.path(cl, paste0(tag, ".gene_level.txt"))) &&
        file.exists(file.path(rs, paste0(tag, ".gene_level.txt"))))
check("the shared normal-selection marker is copied into BOTH classes",
      file.exists(file.path(cl, paste0(tag, ".facets2n_normal_selection.txt"))) &&
        file.exists(file.path(rs, paste0(tag, ".facets2n_normal_selection.txt"))))
tag_line <- grep("^# TAG", readLines(file.path(rs, paste0(tag, "_purity.out"))), value = TRUE)
check("the .out TAG is rewritten to match the renamed files",
      identical(trimws(tag_line), paste0("# TAG = ", tag, "_purity")))
ultra_tag <- grep("^# TAG", readLines(file.path(ul, paste0(tag, "_hisens.out"))), value = TRUE)
check("the ultra .out TAG is rewritten too",
      identical(trimws(ultra_tag), paste0("# TAG = ", tag, "_hisens")))
check("the flat staging dir is removed once emptied", !dir.exists(staging))

## ---------------------------------------------------------------------------
## 3. Single-class split (--only): what an app refit of one class produces
## ---------------------------------------------------------------------------

make_staging <- function(pair, fit, tag) {
  staging <- file.path(pair, fit)
  dir.create(staging, recursive = TRUE)
  for (tok in c("_clinical_purity", "_clinical_hisens", "_research_purity",
                "_research_hisens", "_research_ultra_hisens")) {
    invisible(file.create(file.path(staging, paste0(tag, tok, ".Rdata"))))
    writeLines(c("# INPUT PARAMETERS GIVEN", paste0("# TAG = ", tag, tok)),
               file.path(staging, paste0(tag, tok, ".out")))
  }
  invisible(file.create(file.path(staging, paste0(tag, ".clinical.gene_level.txt"))))
  invisible(file.create(file.path(staging, paste0(tag, ".research.gene_level.txt"))))
  invisible(file.create(file.path(staging, paste0(tag, ".facets2n_normal_selection.txt"))))
  staging
}
run_split <- function(pair, fit, tag, only = NULL) {
  system2("python3", c(shQuote(splitter), shQuote(tag), shQuote(file.path(pair, fit)),
                       shQuote(pair), shQuote(fit), if (!is.null(only)) c("--only", only)),
          stdout = NULL, stderr = NULL)
}

pair_r <- tempfile("refit2n_only_r_"); staging_r <- make_staging(pair_r, fit, tag)
check("--only research runs cleanly", run_split(pair_r, fit, tag, "research") == 0)
check("--only research: research subtree present with nested ultra",
      file.exists(file.path(pair_r, "research", fit, paste0(tag, "_purity.Rdata"))) &&
        file.exists(file.path(pair_r, "research", fit, "ultra", paste0(tag, "_hisens.Rdata"))))
check("--only research: no clinical subtree is created",
      !dir.exists(file.path(pair_r, "clinical")))
check("--only research: the shared marker lands in research only",
      file.exists(file.path(pair_r, "research", fit, paste0(tag, ".facets2n_normal_selection.txt"))))
check("--only research: the staging dir is emptied and removed", !dir.exists(staging_r))

pair_c <- tempfile("refit2n_only_c_"); staging_c <- make_staging(pair_c, fit, tag)
check("--only clinical runs cleanly", run_split(pair_c, fit, tag, "clinical") == 0)
check("--only clinical: clinical subtree present",
      all(file.exists(file.path(pair_c, "clinical", fit,
                                paste0(tag, c("_purity.Rdata", "_hisens.Rdata", ".gene_level.txt",
                                              ".facets2n_normal_selection.txt"))))))
check("--only clinical: research fits (incl. ultra) are discarded, not placed",
      !dir.exists(file.path(pair_c, "research")))
check("--only clinical: the staging dir is emptied and removed", !dir.exists(staging_c))

pair_b <- tempfile("refit2n_only_b_"); staging_b <- make_staging(pair_b, fit, tag)
check("--only with an unknown class is rejected and leaves the staging dir alone",
      run_split(pair_b, fit, tag, "bogus") != 0 && dir.exists(staging_b))

## ---------------------------------------------------------------------------
## 4. build_refit_cmd_2n: one class per refit, flags keyed by class
## ---------------------------------------------------------------------------

start <- grep("^  build_refit_cmd_2n <- function", src)
stopifnot(length(start) == 1)
end   <- start + which(src[(start + 1):length(src)] == "  }")[1]
eval(parse(text = paste(src[start:end], collapse = "\n")), envir = globalenv())

# The server looks the splitter up in the installed package first; force the
# in-repo fallback so the test pins THIS tree's script.
system.file <- function(...) ""
old_wd <- setwd(file.path(repo, "inst", "application"))
sif <- tempfile("fake_", fileext = ".sif"); invisible(file.create(sif))
Sys.setenv(FP_IRIS_2N_SIF = sif, FP_IRIS_2N_REF_LIB_DIR = lib)

ptag <- "P-0000000-T01-IM6_P-0000000-N01-IM6"
pair <- file.path(tempfile("refit2n_cmd_"), ptag); dir.create(pair, recursive = TRUE)
counts <- file.path(pair, paste0("countsMerged____", ptag, ".dat.gz"))

r_nocounts <- build_refit_cmd_2n(ptag, pair, "research", "c50_pc100", FALSE, NA, 25, 15, 250, 35,
                                 NULL, 100, 50)
check("no counts file for the pair is an error", !is.null(r_nocounts$error))

invisible(file.create(counts))
rr <- build_refit_cmd_2n(ptag, pair, "research", "c50_pc100_diplogR_0.1", TRUE, 0.1, 25, 15, 250, 35,
                         NULL, 100, 50)
check("research: builds without error", is.null(rr$error))
sr <- paste(rr$script, collapse = "\n")
check("research: --clinical is NOT passed", !grepl("--clinical", sr, fixed = TRUE))
check("research: dipLogR goes to the research flag", grepl("--dipLogR 0.1 ", sr, fixed = TRUE))
check("research: cvals go to the research flags",
      grepl("--research-purity-cval 100 --research-hisens-cval 50", sr, fixed = TRUE))
check("research: the split keeps research only", grepl("--only research", sr, fixed = TRUE))
check("research: the pair-level counts file is used", grepl(counts, sr, fixed = TRUE))
check("research: exactly one refit dir, under research/",
      identical(rr$refit_dirs, file.path(pair, "research", "refit_c50_pc100_diplogR_0.1")))

rc <- build_refit_cmd_2n(ptag, pair, "clinical", "c75_pc150_diplogR_-0.2", TRUE, -0.2, 25, 15, 250, 35,
                         pair,   # a DIRECTORY as counts file must not be accepted
                         150, 75)
check("clinical: builds without error", is.null(rc$error))
sc <- paste(rc$script, collapse = "\n")
check("clinical: --clinical is passed", grepl("--legacy-output --clinical --MandUnormal", sc, fixed = TRUE))
check("clinical: dipLogR goes to the clinical flag", grepl("--clinical-dipLogR -0.2 ", sc, fixed = TRUE))
check("clinical: the research dipLogR flag is absent", !grepl(" --dipLogR ", sc, fixed = TRUE))
check("clinical: cvals go to the clinical flags",
      grepl("--clinical-purity-cval 150 --clinical-hisens-cval 75", sc, fixed = TRUE))
check("clinical: the split keeps clinical only", grepl("--only clinical", sc, fixed = TRUE))
check("clinical: a directory passed as counts file falls back to the pair's file",
      grepl(paste0("--counts-file ", counts), sc, fixed = TRUE))
check("clinical: exactly one refit dir, under clinical/",
      identical(rc$refit_dirs, file.path(pair, "clinical", "refit_c75_pc150_diplogR_-0.2")))
check("the note names the class subtree the fits go to",
      grepl("clinical fits will be written to", rc$note, fixed = TRUE))
setwd(old_wd)

cat("\n", n_pass, "passed,", n_fail, "failed\n")
if (n_fail > 0) quit(status = 1)
