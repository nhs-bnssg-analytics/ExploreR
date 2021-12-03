demog <- tabItem(tabName="oneDemog",
                 fluidRow(
                   box(h4("Demographic Overview"), width = 4,
                       # box(h4(tags$b("PCN: "), input$pcn)
                   ),
                   box(background = "green", width = 4,
                       uiOutput("oneCoverBoxs1"),
                   ),
                   box(background = "green", width = 4,
                       # h4("Year: 2020-21"),
                       uiOutput("oneCoverBoxDate2")
                   )
                   
                 ),
                 fluidRow(
                 column(width = 10,
                        fluidRow(
                          box(width = 12,#title = "Demographic Overview",
                              fluidRow(
                                column(width = 6,
                                       selectizeInput("oneDemogGroupBy", "Optional split by GlobalGroups:", choices = c(1,2),
                                                      options = list(
                                                        placeholder = 'Please select an option below',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                       ),
                                       selectInput("oneDemogStackedSeparate", "Stacked or Separate Graph", choices = list("Stacked" = TRUE, "Separate" = FALSE)),
                                       selectInput("oneDemogCountPercentage", "Counts or % of population", choices = list("Percentage" = FALSE, "Count" = TRUE), selected = TRUE),
                                       selectizeInput("oneDemogClinNeed", "Select a clinical condition to see prevalence", choices = c(1,2),
                                                      options = list(
                                                        placeholder = 'Please select an option below',
                                                        onInitialize = I('function() { this.setValue(""); }')
                                                      )
                                       ),
                                ),
                                
                                column(width = 6,
                                       uiOutput("collapseFilterOutput1")
                                ),
                              ),
                                plotlyOutput("oneDemogPlot1", height = "900px"),
                              
                              br(),
                              br()
                          ),
                        ),
                 ),
                 column(width = 2,
                        fluidRow(
                          div(
                            box(width = 12,
                                p("A population pyramid, or an age-gender-pyramid, is a graphically illustrated data that shows the distribution of various age groups and gender in a population."),
                                p("This is useful for spotting trends or differences when looking at various attributes of population groups."),
                                p("Use the dropdowns to change the data, and its display format."),
                                p(" ",  actionLink(inputId = "modalDropdownOneDemog",
                                                   label = HTML("More Information: dropdowns"),
                                                   icon = icon("info-circle"),
                                                   style='font-size:100%')),
                                
                                p(" ", actionLink(inputId = "viz_help1",
                                                  label = HTML("More information: definitions"),
                                                  icon = icon("info-circle"),
                                                  style='font-size:100%')),
                                p(" ", actionLink(inputId = "modalGraphInfoOneDemog",
                                                  label = HTML("General/Graph Tips"),
                                                  icon = icon("info-circle"),
                                                  style='font-size:100%'))
                            ), style = "position:fixed;  width:inherit;"
                          )
                        )
                 )
                 )
)