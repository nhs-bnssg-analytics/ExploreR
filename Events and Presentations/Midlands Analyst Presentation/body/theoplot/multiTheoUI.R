s2tab <- fluidRow(column(width = 12,
                column(3,
                       bsCollapse(id = "theoMultiHelpBox",
                                  bsCollapsePanel("Theoplot Info",
                                                  fluidRow(
                                                    column(width = 12,
                                                           p("Visualise multiple patient activity. Filter on provided categories.",
                                                             actionLink(inputId = "fourTheoMultiInfo1",
                                                                        label = HTML("More information"),
                                                                        icon = icon("info-circle"),
                                                                        style='font-size:100%')
                                                             )
                                                    )
                                                  ),
                                                  style = "info")
                       ),
                       
                       
                       wellPanel(
                         p(h4("Select date range")),
                         dateRangeInput("dateidMulti", NULL, start = "2016-04-01", min = "2016-04-01")
                       ),
                       
                       wellPanel(
                         p(h4("Select individuals")),
                         numericInput("fourTheoMultiPatentNum", "Number of individuals to sample", min = 1, step = 1, max = 200, value = 20),
                         tabsetPanel(id = "body5Multi",
                                     tabPanel("By Global Group",
                                              pickerInput("fourTheoMultiGlobalGroup", "Select Individual By:", choices = c(1,2)),
                                              pickerInput("fourTheoMultiGlobalGroupValues", "Use Values", choices = c(1,2), multiple = T, options = list(`actions-box` = TRUE, noneSelectedText = "Please select at least 1 option")),
                                              actionButton("fourTheoMultiGlobalGroupGo", HTML("Go!"))# 
                                     ),
                                     tabPanel("By filter",
                                              sliderInput("fourTheoFilterAgeSliderMulti", label = ("Filter by Age"), min = 0,
                                                          max = 110, value = c(10, 100)),
                                              sliderInput("fourTheoFilterLTCSliderMulti", label = ("Filter by Long Term Conditions"), min = 0,
                                                          max = 20, value = c(0, 10)),
                                              actionButton("fourTheoFilterGoMulti", HTML("Go!"))
                                     ),
                                     tabPanel("Upload IDs",
                                              br(),
                                              fileInput("theoFileInputMulti", "Select a .csv file", accept = ".csv"),
                                              uiOutput("theoFileUIMulti")
                                     ),
                                     type = "tabs")
                       ),
                ), # column end
                column(width = 9,
                       tabsetPanel(id = "multiOptions",
                                   type = "tabs",
                                   tabPanel("Theoplot",
                                            br(),
                                            textOutput("fourTheoMultiPlotText"),
                                            uiOutput("fourTheoMultiPlot"),
                                            textOutput("fourTheoMultiDownloadText")
                                            ),
                                   tabPanel("Options",
                                            fluidRow(
                                              column(width = 12,
                                                     br(),
                                                     fluidRow(
                                                       column(width = 4,
                                                              p(strong("Tick activity type(s) to include")),
                                                              shinyTree("fourTheoMultiActivityToPlot", checkbox = TRUE, themeIcons = FALSE),
                                                       ),
                                                       column(width = 8,
                                                              uiOutput("fourTheoMultiActivityToPlotColours")
                                                       )
                                                     )
                                              )
                                            )
                                            )
                       )
                )
))