#Run this in RStudio from the fp-docker top level directory.

setwd("/Users/aprice/mskcc/pipelines/facets-preview/")
library(Cairo)
library(bit64)
library(shinyjs)
library(data.table)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(glue)
library(purrr)
library(tidyr)
library(tibble)
library(doParallel)
library(parallel)
library(configr)
library(R.utils)
library(shiny)
library(DT)
library(shinyWidgets)
library(shinyFiles)
library(digest)
library(httr)

#source(here::here('R', 'global.R'))
source(file.path(getwd(), 'R', 'global.R'))

setwd("/Users/aprice/mskcc/pipelines/facets-preview/inst/application/")

facets_preview_config_file = "../../Docker/fp_config_local.json"

runApp()
