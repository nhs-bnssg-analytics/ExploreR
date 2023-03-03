cover <- tabItem(tabName="oneTab",
                 
                 fluidRow(
                   box(width = 4, h4("Descriptive Summaries"),
                       # box(h4(tags$b("PCN: "), input$pcn)
                   ),
                   
                   box(background = "green", width = 4,
                       uiOutput("oneCoverBoxs0"),
                   ),
                   box(background = "green", width = 4,
                       # h4("Year: 2020-21"),
                       uiOutput("oneCoverBoxDate1")
                   )
                 ),
                 fluidRow(
                     column(width = 10,
                            fluidRow(
                              box(width = 4, title = "Demographic Overview",
                                  plotlyOutput("pyramid")
                              ),
                              
                              box(width = 4, title = "Clinical Characteristics",
                                  plotlyOutput("pyramidClinical")
                              ),
                              box(width = 4, title = "Activity Overview",
                                  plotlyOutput("pyramidActivity")
                              )
                          )
                          ,
                          fluidRow(
                            box(width = 4, title = "Deprivation",
                                uiOutput("uipyramidDeprivation")
                            ),
                            box(width = 4, title = "Geography",
                                uiOutput("uipyramidGeography")
                            ),
                            box(width = 4, title = "Wider Determinants",
                                uiOutput("uipyramidDeterminants")
                            )
                          )
                   ),
                   
                   column(width = 2, div(
                     fluidRow(
                       box(width = 10, # Higher width would end up under the scrollbar
                           p("Each box represents one of the pages from the Descriptive Summaries section. Use the sidebar on the left to navigate to these pages."),
                           br(),
                           p(" ", actionLink(inputId = "modalDefinitionCover",
                                             label = HTML("More information: definitions"),
                                             icon = icon("info-circle"),
                                             style='font-size:100%')),
                           p(" ", actionLink(inputId = "modalGraphInfoCover",
                                             label = HTML("General/Graph Tips"),
                                             icon = icon("info-circle"),
                                             style='font-size:100%')),
                       ),
                     ), style = "position:fixed;  width:inherit;")
                   )
                   
                   
                   
                 )
)