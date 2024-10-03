#' helper function for app
#'
#' @return runs ui
#' @export ui
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom DT datatable
#' @import shinyjs
#' @import rhandsontable
#' @import shinyWidgets
#' @import shinyFiles

## to keep the text boxes inline.
textInputRow<-function (inputId, label, value = "") {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value, class="input-group"))
}

ui <-
  fluidPage(
   shinyjs::useShinyjs(),
   tags$script('
    Shiny.addCustomMessageHandler("openURL", function(url) {
      window.open(url, "_blank");
    });
  '),
   tags$style(HTML("
    .glyphicon-ok-sign {color:#2b8ee5}
    .glyphicon-remove {color:darkred}
    .glyphicon-ok, .glyphicon-thumbs-up {color:green}
    .glyphicon-remove, .glyphicon-thumbs-down {color:darkred}
    #yValueIndicator { position: absolute; color: green; font-weight: bold; background: white; padding: 2px; }
    #hoverCanvas { position: absolute; top: 0; left: 0; width: 100%; height: 100%; pointer-events: none; }
    #overlayDiv {
        position: absolute;
        top: 3.1%;  /* top of the overlay area */
        height: 18.35%; /* height of the overlay area - note these have to be perfect to scale correctly with the image. */
        width: 100%;
        background-color: rgba(255, 0, 0, 0.0);  /* add some color here to see the overlay for debugging */
        cursor: crosshair;
    }
  ")),
   tags$script(HTML("
    $(document).on('shiny:connected', function() {
        var overlay = document.getElementById('overlayDiv');
        var canvas = document.getElementById('hoverCanvas');
        var ctx = canvas.getContext('2d');
        var indicator = document.getElementById('yValueIndicator');

        overlay.width = overlay.clientWidth;
        overlay.height = overlay.clientHeight;

        // Calculate y-axis range
        var yAxisMin = -3;
        var yAxisMax = 3;

        overlay.addEventListener('mousemove', function(event) {
            var rect = overlay.getBoundingClientRect();
            var y = event.clientY - rect.top;

            // Clear the canvas
            ctx.clearRect(0, 0, canvas.width, canvas.height);

            // Draw the horizontal line directly at the mouse's Y position
            ctx.beginPath();
            ctx.moveTo(0, y);
            ctx.lineTo(canvas.width, y);
            ctx.strokeStyle = 'green';
            ctx.lineWidth = 2;
            ctx.stroke();

            // Calculate the corresponding y-value directly from the Y position within the overlayDiv
            var adjustedY = y / overlay.clientHeight;
            var yValue = yAxisMax - (adjustedY * (yAxisMax - yAxisMin));

            // Display the value at the adjusted position
            indicator.style.top = (y - 10) + 'px';  // Adjust the position of the indicator
            indicator.style.left = '80px'; // Adjust the position of the indicator
            indicator.textContent = yValue.toFixed(2);
        });

        // Handle click to set the dipLogR value
        overlay.addEventListener('click', function() {
            // Set the input value using jQuery
            var finalValue = indicator.textContent;
            $('#textInput_newDipLogR').val(finalValue).trigger('change');
        });

        overlay.addEventListener('mouseleave', function() {
            // Clear the canvas when the mouse leaves the image
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            indicator.textContent = '';
        });

        // React to the dynamic_dipLogR switch being toggled
        Shiny.addCustomMessageHandler('toggleOverlay', function(value) {
            if (value) {
                $('#overlayDiv').show();
            } else {
                $('#overlayDiv').hide();
            }
        });
    });
"))
   ,
    navbarPage(
      "FACETS Preview",
      id="navbarPage1",

      tabPanel("Session",
               value = "tabPanel_session",
               mainPanel(
                 wellPanel(
                   style = "background-color: #EEEEEE; border-color: #DDDDDD; padding: 20px;",
                   div(
                     style = "text-align:left; padding: 20px;",
                     h4("Session Configuration", style = "font-weight: bold; color: black;"),
                     fluidRow(
                       column(10,
                              # Repository Configuration: IMPACT
                              div(
                                h5("Repository Configuration: IMPACT", style = "font-weight: bold; color: black;"),
                                div(
                                  style = "padding-left: 20px;",
                                  div("Local Repository Path:"),
                                  textInput(inputId = "repository_path_impact", label = NULL, value = "", width = "100%"),
                                  shinyWidgets::switchInput(
                                    inputId = "session_switch_impact",
                                    label = "Use Mount",
                                    value = FALSE,  # Set default to "No"
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    size = "small",
                                    inline = TRUE,
                                    labelWidth = "100px"
                                  ),
                                  conditionalPanel(
                                    condition = "input.session_switch_impact == true",
                                    div("Remote Repository Path:", style = "padding-top: 10px;"),
                                    div(
                                      class = "form-group shiny-input-container",
                                      tags$input(id = "remote_path_impact", type = "text", class = "form-control", value = "/juno/work/ccs/shared/resources/impact/facets/all/", disabled = TRUE)
                                    )
                                  )
                                )
                              ),

                              #div(style = "padding-top: 20px;"),

                              # Hidden Repository Configuration: TEMPO
                              conditionalPanel(
                                condition = "false",  # Always hides the panel
                                div(
                                  h5("Repository Configuration: TEMPO", style = "font-weight: bold; color: black;"),
                                  div(
                                    style = "padding-left: 20px;",
                                    div("Local Repository Path:"),
                                    textInput(inputId = "repository_path_tempo", label = NULL, value = "", width = "100%"),
                                    shinyWidgets::switchInput(
                                      inputId = "session_switch_tempo",
                                      label = "Use Mount",
                                      value = FALSE,  # Set default to "No"
                                      onLabel = "Yes",
                                      offLabel = "No",
                                      size = "small",
                                      inline = TRUE,
                                      labelWidth = "100px"
                                    ),
                                    conditionalPanel(
                                      condition = "input.session_switch_tempo == true",
                                      div("Remote Repository Path:", style = "padding-top: 10px;"),
                                      div(
                                        class = "form-group shiny-input-container",
                                        tags$input(id = "remote_path_tempo", type = "text", class = "form-control", value = "/juno/work/ccs/shared/resources/tempo/facets/all/", disabled = TRUE)
                                      )
                                    )
                                  )
                                )
                              ),

                              div(style = "padding-top: 20px;"),

                              # Repository Configuration: TCGA
                              div(
                                h5("Repository Configuration: TCGA", style = "font-weight: bold; color: black;"),
                                div(
                                  style = "padding-left: 20px;",
                                  div("Local Repository Path:"),
                                  textInput(inputId = "repository_path_tcga", label = NULL, value = "", width = "100%"),
                                  shinyWidgets::switchInput(
                                    inputId = "session_switch_tcga",
                                    label = "Use Mount",
                                    value = FALSE,  # Set default to "No"
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    size = "small",
                                    inline = TRUE,
                                    labelWidth = "100px"
                                  ),
                                  conditionalPanel(
                                    condition = "input.session_switch_tcga == true",
                                    div("Remote Repository Path:", style = "padding-top: 10px;"),
                                    div(
                                      class = "form-group shiny-input-container",
                                      tags$input(id = "remote_path_tcga", type = "text", class = "form-control", value = "/juno/work/ccs/shared/resources/tcga/facets/all/", disabled = TRUE)
                                    )
                                  )
                                )
                              ),

                              div(style = "padding-top: 20px;"),

                              # Remote Refit Path
                              div(
                                h5("Mounted Refits: ", style = "font-weight: bold; color: black;"),
                                div(
                                  style = "padding-left: 20px;",
                                  div("Local Refit Path:"),
                                  textInput(inputId = "mount_refit_path", label = NULL, value = "", width = "100%"),
                                  shinyWidgets::switchInput(
                                    inputId = "session_remote_refit",
                                    label = "Connected",
                                    value = FALSE,  # Set default to "No"
                                    onLabel = "Yes",
                                    offLabel = "No",
                                    size = "small",
                                    inline = TRUE,
                                    labelWidth = "100px"
                                  ),
                                  conditionalPanel(
                                    condition = "input.session_remote_refit == true",
                                    div("Remote Refit Path:", style = "padding-top: 10px;"),
                                    div(
                                      class = "form-group shiny-input-container",
                                      tags$input(id = "remote_refit_path", type = "text", class = "form-control", value = "/juno/work/ccs/shared/resources/fp/", disabled = TRUE)
                                    )
                                  )
                                )
                              ),

                              div(style = "padding-top: 20px;"),

                              # Personal Storage Location
                              div(
                                h5("Personal Storage Location", style = "font-weight: bold; color: black;"),
                                div(
                                  style = "padding-left: 20px;",
                                  textInput(inputId = "personal_storage_path", label = NULL, value = "", width = "100%"),
                                  div(
                                    id = "invalid_path_message",
                                    style = "color: red; display: none; padding-top: 10px;",
                                    "Folder does not exist.",
                                    div(
                                      id = "create_folder_button_container",
                                      style = "padding-top: 10px; display: none;",  # Initially hidden
                                      actionButton(inputId = "create_folder_button", label = "Create Folder", class = "btn-primary")
                                    )
                                  )
                                )
                              )
                       )
                     ),

                     # Password and red text section at the bottom
                     conditionalPanel(
                       condition = "input.session_switch_impact == true || input.session_switch_tempo == true || input.session_switch_tcga == true",
                       div(
                         style = "color: red; padding-left: 20px; padding-top: 10px;",
                         "This is a restricted location. Refits and best fits must be authenticated."
                       ),
                       div(
                         style = "padding-left: 20px; padding-top: 10px; width: 50%;",  # Adjust the width of the password field
                         passwordInput(inputId = "auth_password", label = "Password", value = "", width = "100%")
                       )
                     ),

                     # Update Session and Continue buttons
                     div(
                       style = "padding-left: 20px; padding-top: 20px;",
                       actionButton(inputId = "update_session", label = "Update Session", class = "btn-primary"),
                       actionButton(inputId = "continue_session", label = "Continue", class = "btn-primary", style = "margin-left: 10px;")
                     )
                   )
                 ),
                 width = 12
               )
      )
      ,

      tabPanel("Load Samples",
               value="tabPanel_sampleInput",
               mainPanel(
                 shinyjs::hidden(
                   wellPanel(id = "wellPanel_mountFail",
                             style = "background-color: white; border-color: white; ",
                             HTML(
                               paste0("<div style=\"font-size: 16px; font-family: georgia; color: red; display: inline-block\">
                                      <i class=\"fa fa-exclamation-triangle\" aria-hidden=\"true\"></i><strong> /juno/ not mounted.
                                      Nothing is gonna work!  </strong></div>")
                             ),
                             actionButton("button_mountFailRefresh", "Check for mount again", icon = icon("refresh"))
                   )
                 ),
                 HTML(
                   "<div style=\"background-color: #EEEEEE; padding: 20px;font-size: 16px; \">
                   <strong>USAGE</strong> <br> Enter facets directory for
                   each of the samples to be reviewed. This facets output directory is of the standard format generated by
                   facetsSuite v2. The directory should have the counts file as well as the one or many run directories
                   (eg: default, facets_c50p100R0.5.6, refit_dipLogR_1.0, etc)</div><br>"
                 ),

                 conditionalPanel(
                   condition = "input.session_switch_impact == true",  # Show only if session_switch_impact is TRUE
                   wellPanel(
                     h4("Add samples by DMP-ID: "),
                     h5("Accepts short format (Eg: P-0009137-T01-IM5) or long format (Eg: P-0009137-T01-IM5_P-0009137-N01-IM5)."),
                     textAreaInput("textAreaInput_impactSamplesInput", label = NULL, value = "", rows = 4),
                     actionButton("button_impactSamplesInput", "Add to Load Manifest", class = "btn-primary")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.session_switch_tcga == true",  # Show only if session_switch_tcga is TRUE
                   wellPanel(
                     h4("Add samples by TCGA ID: "),
                     h5("Accepts short format (Eg: TCGA-94-7033-10A-01D-1946-08) or long format (Eg: TCGA-94-7033-10A-01D-1946-08_TCGA-94-7033-01A-11D-1945-08)."),
                     textAreaInput("textAreaInput_tcgaSamplesInput", label = NULL, value = "", rows = 4),
                     actionButton("button_tcgaSamplesInput", "Add to Load Manifest", class = "btn-primary")
                   )
                 ),
                 wellPanel(
                   h4("Load Manifest"),
                   h5("Full paths to sample directories: Eg: /juno/work/ccs/path/P-00091/P-0009137-T01-IM5_P-0009137-N01-IM5"),
                   textAreaInput("textAreaInput_samplesInput", label=NULL, value="", rows=4),
                   actionButton("button_samplesInput", "Load Samples", class = "btn-primary")
                 ),
                 width = 12
                 )
      ),
      tabPanel("Samples Manifest",
               value="tabPanel_samplesManifest",
               mainPanel(
                 wellPanel(downloadLink("download_mapping_file", "Download Mapping File"),
                           style = "background: gray90"),
                 wellPanel(DT::dataTableOutput("datatable_samples"), style = "background: gray90"),
                 width=12
               )
      ),
      tabPanel("Review Fits",
               value="tabPanel_reviewFits",
               fluidRow(
                 column(3,
                        wellPanel(
                          h4(strong("Select Sample:")),
                          selectInput(inputId = "selectInput_selectSample", label = NULL,
                                      choices = c("Not selected"), selected = NA),

                          # Combine "Select Fit" heading and the trophy div on the same line
                          div(style = "display: flex; align-items: center;",
                              h4(strong("Select Fit:")),
                              shinyjs::hidden(
                                div(id = "div_bestFitTrophy",
                                    HTML("<center><font size=4>
                  <i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> Best Fit.</center></font>"),
                                    style = "color: darkgreen; margin-left: 10px;"  # Adjust margin to create space between the heading and the trophy
                                )
                              )
                          ),

                          selectInput(inputId = "selectInput_selectFit", label = NULL,
                                      choices = c("Not selected"), selected = NA),

                          div(id = "storageTypeDiv",  # Add an ID for the div containing the switchInput
                              div(style = "display: inline-block; vertical-align: middle; margin-top: 10px;",
                                  div(style = "position: relative; left: 10px; top: -5px; transform: scale(1);",
                                      shinyWidgets::switchInput(
                                        inputId = "storageType",
                                        label = "Storage Type",
                                        value = FALSE,  # Set default to off
                                        onLabel = "Remote",
                                        offLabel = "Personal",
                                        size = "small",
                                        labelWidth = "100px"
                                      )
                                  )
                              )
                          ),

                          h4(strong("Select Run:")),
                          shinyWidgets::radioGroupButtons(inputId = "radioGroupButton_fitType",
                                                          choices = c("Purity", "Hisens"),
                                                          selected = NA,
                                                          status = "primary", size = 'normal', width = '100%', justified = TRUE),
                          actionButton("button_copyClipPath", "Copy path to clipboard.",
                                       icon = icon("clipboard"),
                                       style = "height:28px;font-size:8pt;vertical-align: middle; color:darkblue"),
                          h5(em('run parameters')),
                          verbatimTextOutput("verbatimTextOutput_runParams", placeholder = TRUE),

                          style = "padding: 10px; padding-left: 5px; padding-right: 5px;"
                        )

                        ,
                        wellPanel(
                          style = "padding-left: 5px; padding-right: 5px; padding-top: 5px; padding-bottom: 5px;", # Adjust padding as needed

                          div(style = "display: inline-block; vertical-align: middle; margin-top: 10px;",
                              div(style = "position: relative; left: 10px; top: -5px; transform: scale(1);",
                                  shinyWidgets::switchInput(
                                    inputId = "compareFitsCheck",
                                    label = "Compare Fits",
                                    value = FALSE,  # Set default to off
                                    onLabel = "On",
                                    offLabel = "Off",
                                    size = "small",
                                    labelWidth = "100px"
                                  )
                              )
                          )
                        ),
                        shinyjs::hidden(
                          wellPanel(
                            h4(strong("Select Sample:")),
                            selectInput(inputId = "selectInput_selectSample_compare", label = NULL,
                                        choices = c("Not selected"), selected = NA),

                            # Combine "Select Fit" heading and the trophy div on the same line
                            div(style = "display: flex; align-items: center;",
                                h4(strong("Select Fit:")),
                                shinyjs::hidden(
                                  div(id = "div_bestFitTrophy_compare",
                                      HTML("<center><font size=4>
                    <i class=\"fa fa-thumbs-up\" aria-hidden=\"true\"></i> Best Fit.</center></font>"),
                                      style = "color: darkgreen; margin-left: 10px;"  # Adjust margin to create space between the heading and the trophy
                                  )
                                )
                            ),

                            selectInput(inputId = "selectInput_selectFit_compare", label = NULL,
                                        choices = c("Not selected"), selected = NA),

                            div(id = "storageTypeDiv_compare",  # Add an ID for the div containing the switchInput
                                div(style = "display: inline-block; vertical-align: middle; margin-top: 10px;",
                                    div(style = "position: relative; left: 10px; top: -5px; transform: scale(1);",
                                        shinyWidgets::switchInput(
                                          inputId = "storageType_compare",
                                          label = "Storage Type",
                                          value = FALSE,  # Set default to off
                                          onLabel = "Remote",
                                          offLabel = "Personal",
                                          size = "small",
                                          labelWidth = "100px"
                                        )
                                    )
                                )
                            ),

                            h4(strong("Select Run:")),
                            shinyWidgets::radioGroupButtons(inputId = "radioGroupButton_fitType_compare",
                                                            choices = c("Purity", "Hisens"),
                                                            selected = NA,
                                                            status = "primary", size = 'normal', width = '100%', justified = TRUE),
                            actionButton("button_copyClipPath", "Copy path to clipboard.",
                                         icon = icon("clipboard"),
                                         style = "height:28px; font-size:8pt; vertical-align: middle; color:darkblue"),
                            h5(em('run parameters')),
                            verbatimTextOutput("verbatimTextOutput_runParams_compare", placeholder = TRUE),

                            style = "padding-left: 5px; padding-top: 10px; padding-right: 5px; padding-bottom: 10px;",
                            id = "selectBox_compare"
                          )
                        )

                        ,
                        wellPanel(
                          id = "fitPanel",
                          h4(strong("Generate Refits:")),

                          column(12,
                                 textInput("textInput_newDipLogR", value = "", "dipLogR")
                          ),
                          column(12,
                                 div(style = "display: flex; justify-content: center;",  # Center the switch
                                     shinyWidgets::switchInput(
                                       inputId = "dynamic_dipLogR",
                                       label = "Dynamic DipLogR",
                                       value = FALSE,  # Default to off
                                       size = "small",
                                       inline = TRUE
                                     )
                                 )
                          ),
                          column(6,
                                 textInput("textInput_newPurityCval", value = 100, "Purity cVal "),
                                 textInput("textInput_newPurityMinNHet", "Purity Min nHet ")
                          ),
                          column(6,
                                 textInput("textInput_newHisensCval", value = 50, "Hisens cVal "),
                                 textInput("textInput_newHisensMinNHet", "Hisens Min nHet ")
                          ),
                          column(12,
                                 textInput("textInput_newSnpWindowSize", "SNP Window Size "),
                                 textInput("textInput_newNormalDepth", "Normal Depth "),
                                 selectInput("selectInput_newFacetsLib", "FACETS Version:",
                                             c("use current run's facets version" = "use current run's facets version"))
                          ),

                          # Wrap the switchInput in a div with width: 100%
                          div(style = "width: 100%;",
                              div(style = "display: flex; justify-content: center; width: 100%;",
                                  shinyWidgets::switchInput(
                                    inputId = "use_remote_refit_switch",
                                    label = "Execute On",
                                    value = FALSE,  # Default to "Local"
                                    onLabel = "Remote",
                                    offLabel = "Local",
                                    size = "small",
                                    inline = TRUE,
                                    labelWidth = "100px"
                                  )
                              )
                          ),

                          # New inputs to be shown/hidden
                          div(id = "remote_refit_options", style = "display: none;",  # Initially hidden
                              column(12,
                                     textInput("textInput_timeLimit", value = "3:59", "Time Limit (H:MM)"),
                                     textInput("textInput_memory", value = "8", "Memory (in GB)"),
                                     textInput("textInput_cores", value = "2", "Num. Cores")
                              ),
                          ),

                          div(style = "width: 100%;",
                              shinyFiles::shinyFilesButton("fileInput_pileup", "Select Counts File",
                                               "Please select a file", class = "btn-primary",
                                               style = "width: 100%;", multiple = FALSE)
                          ),
                          div(style = "height: 10px;"),
                          actionButton("button_refit", "Run", class = "btn-primary", width = '100%'),
                          style = "padding: 10px"
                        )


                 ),
                 column(9,
                        mainPanel(
                          tabsetPanel(
                            id="reviewTabsetPanel",
                            tabPanel("PNG images",
                                     value = "png_image_tabset",
                                     fluidRow(
                                       column(width = 12,
                                              div(
                                                style = "overflow-x: auto; white-space: nowrap;",  # Only horizontal scrolling
                                                tags$div(
                                                  style = "display: inline-block; vertical-align: top; position: relative;",  # Align images to the top
                                                  imageOutput("imageOutput_pngImage1", width = "650px", height = "auto"),
                                                  tags$div(
                                                    id = "overlayDiv",
                                                    tags$canvas(id = "hoverCanvas"),
                                                    tags$div(id = "yValueIndicator")
                                                  )
                                                ),
                                                tags$div(
                                                  id = "div_imageOutput_pngImage2",
                                                  style = "display: inline-block; margin-left: 10px; vertical-align: top;",  # Align images to the top
                                                  imageOutput("imageOutput_pngImage2", width = "650px", height = "auto")
                                                )
                                              )
                                       )
                                     )
                            )
                            ,
                            tabPanel("QC Summary",
                                     value = "tabPanel_QC",
                                      h3("Showing QC for:"),
                                      verbatimTextOutput("verbatimTextOutput_name_of_qc_fit"),
                                      h3("facets QC flags:"),
                                      h4(p(id = "element_facets_qc_version1", ""),
                                         style="color: #de2e07; font-weight: bold; font-style: italic; padding-top: 0px; padding-bottom: 0px"),
                                      DT::dataTableOutput("datatable_QC_flags"),
                                      h3("QC metrics (NOTE: purity run):"),
                                      DT::dataTableOutput("datatable_QC_metrics")
                                     ),
                            tabPanel("Close Ups",
                                     value="closeup_tabset",
                                     wellPanel(
                                       textInput("textInput_geneForCloseup", label = NULL,
                                                 placeholder = "Hugo Symbol"),
                                       actionButton("button_closeUpView", "View Gene", class = "btn-primary",
                                                    width='100%')),
                                     plotOutput(outputId = "plotOutput_closeup", height="600px")
                                     ),
                            tabPanel("Gene-Level",
                                     fluidRow(
                                       column(12,  # Use the full width of the tab
                                              div(
                                                id = "displayOptionsContainer_geneLevel",
                                                style = "width: 100%; background-color: #f5f5f5; padding: 10px; box-sizing: border-box; margin-bottom: 20px;",  # Added margin-bottom for space before the data table
                                                shinyWidgets::switchInput(
                                                  inputId = "displayOptionsSwitch_geneLevel",
                                                  label = "Display Options",
                                                  value = FALSE,
                                                  onLabel = "Show",
                                                  offLabel = "Hide",
                                                  size = "small",
                                                  inline = TRUE,
                                                  width = "100%"
                                                ),
                                                div(
                                                  id = "displayModeSwitchDiv_geneLevel",
                                                  style = "width: 100%; margin-top: 10px; display: none;",  # Initially hidden
                                                  shinyWidgets::switchInput(
                                                    inputId = "displayMode_geneLevel",
                                                    label = "Display Mode",
                                                    value = TRUE,  # Default to "Long"
                                                    onLabel = "Long",
                                                    offLabel = "Short",
                                                    size = "small",
                                                    inline = TRUE,
                                                    width = "100%"
                                                  )
                                                ),
                                                div(
                                                  id = "selectColumnsDiv_geneLevel",
                                                  style = "width: 100%; margin-top: 10px; display: none;",  # Initially hidden
                                                  h5("Select Columns:"),
                                                  fluidRow(
                                                    column(6,
                                                           div(style = "padding-left: 20px;",  # Add padding to indent the checkboxes
                                                               checkboxGroupInput(
                                                                 inputId = "selectedColumns_sample1",
                                                                 label = "",
                                                                 choices = NULL,  # Choices will be dynamically updated based on the data
                                                                 selected = NULL  # Initially, no columns are selected
                                                               )
                                                           )
                                                    ),
                                                    column(6,
                                                           div(style = "padding-left: 20px;",  # Add padding to indent the checkboxes
                                                               checkboxGroupInput(
                                                                 inputId = "selectedColumns_sample2",
                                                                 label = "",
                                                                 choices = NULL,  # Choices will be dynamically updated based on the data
                                                                 selected = NULL  # Initially, no columns are selected
                                                               )
                                                           )
                                                    )
                                                  )
                                                )
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              mainPanel(
                                                DT::dataTableOutput("datatable_geneLevel"),
                                                width = 12
                                              )
                                       )
                                     )
                            ),
                            tabPanel("Arm-Level",
                                     fluidRow(
                                       column(12,
                                              div(
                                                id = "displayOptionsContainer_armLevel",
                                                style = "width: 100%; background-color: #f5f5f5; padding: 10px; box-sizing: border-box; margin-bottom: 20px;",  # Added margin-bottom for space before the data table
                                                shinyWidgets::switchInput(
                                                  inputId = "displayOptionsSwitch_armLevel",
                                                  label = "Display Options",
                                                  value = FALSE,
                                                  onLabel = "Show",
                                                  offLabel = "Hide",
                                                  size = "small",
                                                  inline = TRUE,
                                                  width = "100%"
                                                ),
                                                div(
                                                  id = "displayModeSwitchDiv_armLevel",
                                                  style = "width: 100%; margin-top: 10px; display: none;",  # Initially hidden
                                                  shinyWidgets::switchInput(
                                                    inputId = "displayMode_armLevel",
                                                    label = "Display Mode",
                                                    value = TRUE,  # Default to "Long"
                                                    onLabel = "Long",
                                                    offLabel = "Short",
                                                    size = "small",
                                                    inline = TRUE,
                                                    width = "100%"  # Set switch input width to 100%
                                                  )
                                                ),
                                                div(
                                                  id = "selectColumnsDiv_armLevel",
                                                  style = "width: 100%; margin-top: 10px; display: none;",  # Initially hidden
                                                  h5("Select Columns:"),
                                                  fluidRow(
                                                    column(6,
                                                           div(style = "padding-left: 20px;",  # Add padding to indent the checkboxes
                                                               checkboxGroupInput(
                                                                 inputId = "selectedColumns_sample1_armLevel",
                                                                 label = "",
                                                                 choices = NULL,  # Choices will be dynamically updated based on the data
                                                                 selected = NULL  # Initially, no columns are selected
                                                               )
                                                           )
                                                    ),
                                                    column(6,
                                                           div(style = "padding-left: 20px;",  # Add padding to indent the checkboxes
                                                               checkboxGroupInput(
                                                                 inputId = "selectedColumns_sample2_armLevel",
                                                                 label = "",
                                                                 choices = NULL,  # Choices will be dynamically updated based on the data
                                                                 selected = NULL  # Initially, no columns are selected
                                                               )
                                                           )
                                                    )
                                                  )
                                                )
                                              )
                                       )
                                     ),
                                     fluidRow(
                                       column(12,
                                              mainPanel(
                                                DT::dataTableOutput("datatable_armLevel"),
                                                width = 12
                                              )
                                       )
                                     )
                            )
                            ,
                            tabPanel("Segments",
                                     mainPanel(DT::dataTableOutput("datatable_cncf"), width=12)),
                            tabPanel("Segments (editable)",
                                     wellPanel(
                                       HTML(
                                         "<div style=\"padding: 1px;font-size: 14px; \">
                                         <u>Note:</u> This table is editable and 'Save Changes' button saves the edits to file ending
                                         with .cncf.edited.txt. This '.cncf.edited.txt' file (if exists) will be loaded
                                         automatically next time. (Only tcn/lcn/tcn.em/lcn.em are saved)</div><br>"
                                       ),
                                       actionButton("button_saveChanges", "Save changes to this file",
                                                    width='100%', class="btn-primary"),
                                       rhandsontable::rHandsontableOutput("editableSegmentsTable"))

                                     ),
                            tabPanel("Review notes",
                                     wellPanel(
                                       h4(strong("facets QC summary:")),
                                       h4(p(id = "element_facets_qc_version2", ""),
                                          style="color: #de2e07; font-weight: bold; font-style: italic; padding-top: 0px; padding-bottom: 0px"),
                                       DT::dataTableOutput("datatable_fitReviews")
                                     ),
                                     wellPanel(
                                       h4(strong("Review History:")),
                                       DT::dataTableOutput("datatable_reviewHistory")
                                     ),
                                     wellPanel(
                                       h4(strong("Add Manual Review:")),
                                       radioButtons("radioButtons_reviewStatus", label=NULL,
                                                          c("Not Reviewed" = "not_reviewed",
                                                            "No fit available" = "reviewed_no_fit",
                                                            "Acceptable fit" = "reviewed_acceptable_fit",
                                                            "Best fit" = "reviewed_best_fit"),
                                                    inline=TRUE,
                                                    selected = "not_reviewed"),
                                       conditionalPanel(
                                         condition = "input.radioButtons_reviewStatus == 'reviewed_acceptable_fit' | input.radioButtons_reviewStatus == 'reviewed_best_fit'",
                                         h5(strong("Select best fit (applicable only if best fit selected):")),
                                         selectInput(inputId = "selectInput_selectBestFit", label=NULL,
                                                     choices=c("Not selected")),
                                         checkboxInput(inputId = "checkbox_purity_only", label = "Use purity run for CCF estimation", value = FALSE),
                                         checkboxInput(inputId = "checkbox_use_edited_cncf", label = "Use edited.cncf.txt", value = FALSE),
                                         textInput("textInput_purity", label = 'Use purity (only to be set when facets purity is NA/0.3):')
                                       ),
                                       h4(strong("Review Notes:")),
                                       textAreaInput("textAreaInput_reviewNote", label=NULL, value="", rows=1),
                                       h4(strong("Reviewed By:")),
                                       verbatimTextOutput("verbatimTextOutput_signAs"),
                                       actionButton("button_addReview", "Submit Review", class = "btn-primary", width='100%')
                                       )
                                  ),
                            tabPanel("cBioPortal")
                          ),width = '100%'
                      )
               )
      )
    ),
    tabPanel("Help",
             value = "tabPanel_help",
             fluidRow(
               column(12,
                      wellPanel(
                        h4("Help"),
                        uiOutput("help_links"),  # Dynamically generated links for HTML files
                        br(),
                        htmlOutput("help_content")  # Dynamically loaded HTML content
                      )
               )
             )
    )
    ,
    tabPanel("[sessionInfo]",
             value="tabPanel_sessionInfo",
             mainPanel(
               verbatimTextOutput("verbatimTextOutput_sessionInfo")
             )
    )
  )
)
