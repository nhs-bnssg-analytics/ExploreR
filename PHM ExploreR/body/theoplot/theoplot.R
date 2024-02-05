theoTab <- tabItem(tabName="fourTab",
                   fluidRow(
                     box(h4("Theoplots"), width = 4,
                     ),
                     
                     box(background = "green",width = 4,
                         uiOutput("oneCoverBoxs16"),
                     ),
                     box(background = "green", width = 4,
                         h4("Year: 2020-21"),
                     )
                   ),
                   fluidRow(
                     box(width = 12,
                         
                       tabsetPanel(id = "TheoType",
                                   tabPanel("Individual Theoplots",
                                            source("./body/theoplot/singleTheoUI.R", local = T, echo = F)
                                   ),
                                   tabPanel("Group Theoplots",
                                            source("./body/theoplot/multiTheoUI.R", local = T)
                                   ),
                                   type = "tabs"
                       )
                     )
                   )
)# close Theoplot tab
