cohort1 <- tabItem(tabName="threeID",
                   fluidRow(
                     box(h4("Cohort Identification"), width = 4,
                     ),
                     box(background = "green",width = 4,
                         uiOutput("oneCoverBoxs17"),
                     ),
                     box(background = "green", width = 4,
                         uiOutput("oneCoverBoxDate12")
                         # h4("Year: 2020-21"),
                     )
                   ),
                   
                   
                   fluidRow(
                     column(width = 9,
                            fluidRow(
                              box(width = 12, title = "Identification",
                                  fluidRow(
                                    column(width = 6,
                                           textInput("threeCohortIDName", "Cohort Name:", value = "Cohort1")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           bsCollapse(id = "threeCohortUploadFile",
                                                      bsCollapsePanel("Upload IDs",
                                                                      fileInput("threeCohortUploadIds", "Select File", accept = ".csv"),
                                                                      actionButton("threeCohortUploadIdsButton", "Upload"),
                                                                      uiOutput("threeFileUploadRrror"),
                                                                      style = "info")
                                           )
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           wellPanel(
                                             uiOutput("threeCohortUI")
                                           )
                                    ),
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           selectInput("threeCohortUIandor", "AND/OR:", choices = c("AND","OR"), selected = "OR")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           actionButton("threeCohortNewUI","Add New Field")
                                    ),
                                    br(),
                                    br(),
                                  ),
                                  br(),
                                  hr(),
                                  
                                  fluidRow(
                                    column(width = 4,
                                           actionButton("threeCohortGetIDs","Get Cohort")
                                    ),
                                    
                                    column(width = 4,
                                           textOutput("threeCohortGetIDsMessage")
                                    )
                                  ),
                                  br(),
                                  fluidRow(
                                    uiOutput("threeCohortDownload")
                                  )
                              )
                            )
                     ),
                     
                     column(width = 3, 
                       fluidRow(
                         div(
                           box(width = 12,
                               title = "Identifying Cohorts",
                               
                               p(actionLink(inputId = "threeCohort1",
                                            label = HTML("What is a Cohort?"),
                                            icon = icon("info-circle"),
                                            style='font-size:100%')),
                               p(actionLink(inputId = "threeCohort2",
                                            label = HTML("How to use this page?"),
                                            icon = icon("info-circle"),
                                            style='font-size:100%')),
                               p(actionLink(inputId = "threeCohort3",
                                            label = HTML("Uploading IDs"),
                                            icon = icon("info-circle"),
                                            style='font-size:100%')),
                               p("Please note empty cells or type mismatches in inputs may cause unintended behaviour."),
                               
                               p(" ", actionLink(inputId = "viz_helpthreeCohort",
                                                 label = HTML("More information: definitions"),
                                                 icon = icon("info-circle"),
                                                 style='font-size:100%')),
                               p(" ", actionLink(inputId = "modalGraphInfoThreeCohort",
                                                 label = HTML("General/Graph Tips"),
                                                 icon = icon("info-circle"),
                                                 style='font-size:100%'))

                           ), style = "position:fixed;  width:inherit;")
                       )
                     )
                   )
)