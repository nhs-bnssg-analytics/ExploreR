analysisDataPage <- tabItem(tabName="analysisData",

                   fluidRow(
                     box(h4("Analysis Dateset Identification")
                     )
                   ),
                   
                   
                   fluidRow(
                     column(width = 9,
                            fluidRow(
                              box(width = 12, title = "Create new analysis dataset",
                                  fluidRow(
                                    column(width = 6,
                                           textInput("zeroAnalysisDatasetName", "Dataset Name:", value = "Dataset1")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           bsCollapse(id = "zeroAnalysisDataUploadFile",
                                                      bsCollapsePanel("Upload IDs",
                                                                      fileInput("zeroAnalysisDataUploadIds", "Select File", accept = ".csv"),
                                                                      actionButton("zeroAnalysisDataUploadIdsButton", "Upload"),
                                                                      uiOutput("zeroAnalysisDataFileUploadRrror"),
                                                                      style = "info")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           wellPanel(
                                             uiOutput("zeroAnalysisDataUI")
                                           )
                                    ),
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           selectInput("zeroAnalysisDataUIandor", "AND/OR:", choices = c("AND","OR"), selected = "OR")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           actionButton("zeroAnalysisDataNewUI","Add New Field")
                                    ),
                                    br(),
                                    br(),
                                  ),
                                  br(),
                                  hr(),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           actionButton("zeroAnalysisDatasetGetIDs","Get Analysis Dataset")
                                    ),
                                    column(width = 4,
                                           textOutput("zeroAnalysisDatasetGetIDsMessage")
                                    )
                                  ),
                                  br(),
                                  hr(),
                                  fluidRow(
                                    column(width = 12,
                                           actionButton("zeroAnalysisDatasetBack","Return to Landing Page")
                                           )
                                  )
                              )
                            ),
                            fluidRow(
                              box(width = 12, title = "Remove analysis dataset",
                                  column(width = 6,
                                         fluidRow(
                                           pickerInput("zeroAnalysisDatasetToRemove", "Select a dataset", choices = c(1,2), options = list(size = 12)),
                                           hr()
                                         )
                                  ),
                                  column(width = 3,style = "margin-top: 25px;",align="center",
                                         fluidRow(
                                           actionButton("zeroAnalysisDatasetRemove", "Remove Dataset"),
                                           hr()
                                         )
                                  ),
                                  column(width = 3,style = "margin-top: 25px;",align="center",
                                         fluidRow(
                                           hr()
                                         )
                                  ),
                                  column(width = 12,
                                         fluidRow(
                                           textOutput("zeroAnalysisDatasetRemoveMessage")
                                         )
                                  )
                              )
                            )
                     ),
                     
                     column(width = 3, 
                            fluidRow(
                              div(
                                box(width = 12,
                                    title = "About Analysis Datasets",
                                    
                                    p(actionLink(inputId = "zeroAnalysis1",
                                                 label = HTML("What is an Analysis Dataset?"),
                                                 icon = icon("info-circle"),
                                                 style='font-size:100%')),
                                    p(actionLink(inputId = "zeroAnalysis2",
                                                 label = HTML("How to use this page?"),
                                                 icon = icon("info-circle"),
                                                 style='font-size:100%')),
                                    p(actionLink(inputId = "zeroAnalysis3",
                                                 label = HTML("Uploading IDs"),
                                                 icon = icon("info-circle"),
                                                 style='font-size:100%')),
                                    p("Please note empty cells or type mismatches in inputs may cause unintended behaviour.")
                                ), style = "position:fixed;  width:inherit;")
                            )
                     )
                   )
)