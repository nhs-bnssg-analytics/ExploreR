theoTab <- tabItem(tabName="fourTab",
                   fluidRow(
                     box(h4("Theoplots"),
                     ),
                     
                     box(background = "green",width = 4,
                         uiOutput("oneCoverBoxs16"),
                     ),
                     box(background = "green", width = 2,
                         h4("Year: 2020-21"),
                     )
                   ),
                   fluidRow(
                     box(width = 12,
                       
                       # tags$head(
                       #   tags$style(HTML("
                       #      .shiny-output-error-validation {
                       #      text-align: center;
                       #      color: green;
                       #      font-size: 30px; } "))
                       # ),
                       
                       tabsetPanel(id = "TheoType",
                                   tabPanel("Individual Theoplots",
                                            source("./body/theoplot/singleTheoUI.R", local = T)
                                   ),
                                   tabPanel("Group Theoplots",
                                            source("./body/theoplot/multiTheoUI.R", local = T)
                                   ),
                                   type = "tabs"
                       )
                     )
                   )
)# close Theoplot tab
