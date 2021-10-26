cohort1 <- tabItem(tabName="threeID",
                   # If you want to stick to funds, go for Vanguard. If you instead decide to dive into stocks, Degiro is a solid, low cost platform.
                   fluidRow(
                     box(h4("Cohort Identification"),
                     ),
                     box(background = "green",width = 4,
                         uiOutput("oneCoverBoxs17"),
                     ),
                     box(background = "green", width = 2,
                         h4("Year: 2020-21"),
                     )
                   ),
                   
                   
                   fluidRow(
                     column(width = 9,
                            fluidRow(
                              box(width = 12, title = "Identification",
                                  fluidRow(
                                    column(width = 6,
                                           textInput("threeIDName", "Cohort Name:", value = "Cohort")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           bsCollapse(id = "threeUploadFile",
                                                      bsCollapsePanel("Upload IDs",
                                                                      fileInput("threeUploadIds", "Select File", accept = ".csv"),
                                                                      actionButton("threeUploadIdsButton", "Upload"),
                                                                      uiOutput("threeFileUploadRrror"),
                                                                      style = "info")
                                           ),
                                           
                                           # 
                                    )
                                  ),
                                  fluidRow(
                                    column(width = 12,
                                           wellPanel(
                                             uiOutput("threeUI")
                                           )
                                    ),
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           selectInput("threeUIandor", "AND/OR:", choices = c("AND","OR"), selected = "OR")
                                    ),
                                    column(width = 6, style = "margin-top: 25px;",
                                           actionButton("threeNewUI","Add New Field")
                                    ),
                                    # column(width = 4),
                                    br(),
                                    br(),
                                  ),
                                  br(),
                                  br(),
                                  p("========================================================"),
                                  
                                  fluidRow(
                                    # column(width = 4,
                                    #        # checkboxInput("threeIDAddToGraphs", "Tick To Add Cohort To Summary Graphs", value = FALSE)
                                    # ),
                                    column(width = 4,
                                           actionButton("threeGetCohort","Get Cohort")
                                    ),
                                    
                                    column(width = 4,
                                           textOutput("threeGetCohortMessage")
                                    ),
                                    # style = "margin-top: 25px;",
                                    
                                  ),
                                  fluidRow(
                                    uiOutput("threeCohortDownload")
                                  )
                                  
                              ),
                              
                            ),
                     ),
                     
                     column(width = 3, 
                       fluidRow(
                         div(
                         box(width = 12,
                             column(width = 12,
                                    fluidRow(
                                      p("You can use this page to identify a cohort from the population by their clinical attributes."),
                                      p("TO use this page:"),
                                      p("1. Determine what criteria defines the cohort."),
                                      p("2. Split this criteria into simple statements, such as 'age > 18', and 'has hypertension'"),
                                      p("3. For each of these statements, create a new line (button that says 'Add new field'). Note these clauses are joined either by OR/AND. 
                                        This can be changed by the dropdown to the left of the button."),
                                      p("4. For each clause, select the appropriate field name, then the values to include in the cohort.",
                                        "There are multiple ways of selecting these values."),
                                      p("5. Click 'Get Cohort' to select these individuals. Refer to the other pages in the tool to compare the selected cohort to the rest of the population."),
                                      p("Please note empty values in cells can have unintended consequences."),
                                      # p(" ", actionLink(inputId = "threeCohortInfo1",
                                      #                   label = HTML("About this predictive model"),
                                      #                   icon = icon("info-circle"),
                                      #                   style='font-size:100%')),
                                      
                                      p(" ", actionLink(inputId = "viz_helpthreeCohort",
                                                        label = HTML("More information: definitions"),
                                                        icon = icon("info-circle"),
                                                        style='font-size:100%')),
                                      p(" ", actionLink(inputId = "modalGraphInfoThreeCohort",
                                                        label = HTML("General/Graph Tips"),
                                                        icon = icon("info-circle"),
                                                        style='font-size:100%'))
                                    )
                             ),
                             # "Helptext",
                             # p("Warning! Entering an empty value will default to last non-empty value in the cell. This will have unintended consequences.")
                         ), style = "position:fixed;  width:inherit;")
                       )
                     )
        
                     
                     
                   )
)