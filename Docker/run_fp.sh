#!/bin/bash
Rscript -e "facets_preview_config_file = '/usr/bin/facets-preview/fp_config.json' ; options(shiny.port = 3838, shiny.host = '0.0.0.0', shiny.launch.browser = FALSE) ; library(facetsPreview); facetsPreview::launch_application()"

