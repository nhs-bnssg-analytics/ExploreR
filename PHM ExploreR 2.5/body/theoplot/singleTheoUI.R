s1tab <- fluidRow(column(width = 12,
                column(3,
                       bsCollapse(id = "theoSingleHelpBox",
                                  bsCollapsePanel("Theoplot Info",
                                                  fluidRow(
                                                    column(width = 12,
                                                           p("Visualise individual patient activity. Choose a specific individual, or filter on provided categories.",
                                                             actionLink(inputId = "fourTheoSingleInfo1",
                                                                        label = HTML("More information"),
                                                                        icon = icon("info-circle"),
                                                                        style='font-size:100%')),
                                                           
                                                           bsModal(id = "modal_viz",
                                                                   title = HTML("<h2><strong>Theoplot Help</strong></h2>"),
                                                                   trigger = "viz_help",
                                                                   size = "large",
                                                                   h4(strong("FAQs")),
                                                                   br()
                                                           ),         
                                                    )
                                                  ),
                                                  style = "info")
                       ),
                       wellPanel(
                               p(h4("Select Date Range")),
                               dateRangeInput("fourTheoDate", NULL, start = "2016-04-01", min = "2016-04-01")
                       ),
                       wellPanel(
                               p(h4("Select Individual")),
                               tabsetPanel(id = "body5",
                                           tabPanel("By Global Group",
                                                    pickerInput("fourTheoSingleGlobalGroup", "Select Individual By:", choices = c(1,2)),
                                                    pickerInput("fourTheoSingleGlobalGroupValues", "Sample Using Values:", choices = c(1,2), multiple = T, options = list(`actions-box` = TRUE, noneSelectedText = "Please select at least 1 option")),
                                                    actionButton("fourTheoSingleGlobalGroupGo", HTML("Go!"))
                                           ),
                                           tabPanel("By Filter",
                                                    
                                                    sliderInput("fourTheoFilterAgeSlider", label = ("Filter by Age"), min = 0,
                                                                max = 110, value = c(10, 100)),
                                                    sliderInput("fourTheoFilterLTCSlider", label = ("Filter by Long Term Conditions"), min = 0,
                                                                max = 20, value = c(0, 11)),
                                                    actionButton("fourTheoFilterGo", HTML("Go!"))
                                           ),
                                           # TODO RM External
                                           tabPanel("Upload IDs",
                                                    br(),
                                                    fileInput("theoFileInput", "Select a .csv file", accept = ".csv"),
                                                    uiOutput("theoFileUI")
                                           ),
                                           type = "tabs")
                       )
                ), # column end
                column(width = 9,
                       fluidRow(
                         column(3,
                                div(style ='width:200px; overflow-x: auto;height:200px;overflow-y: auto',
                                    p("Display activity type(s)"),
                                    shinyTree("fourTheotree", checkbox = TRUE, themeIcons = FALSE)
                                ),
                                h3("Attributes"),
                                htmlOutput("fourTheoSingleTheoAttributes")
                         ),
                         column(9,
                                plotlyOutput("figureplotly"),
                                span(textOutput("fourTheoSingleErr"), style="color:red")
                         )
                       ),
                       br()
                )
))