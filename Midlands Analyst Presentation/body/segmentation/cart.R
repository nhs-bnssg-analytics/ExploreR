
cart <- tabItem(tabName="twoCART",
                fluidRow(
                  box(h4("Decision Trees"), width = 4,
                  ),
                  
                  box(background = "green",width = 4,
                      uiOutput("oneCoverBoxs9"),
                  ),
                  box(background = "green", width = 4,
                      uiOutput("oneCoverBoxDate10")
                      # h4("Year: 2020-21"),
                  )
                ),
                fluidRow(
                  column(width = 9,
                         fluidRow(
                           box(width = 12, collapsible = TRUE, title = "Variable Setup",
                               column(width = 5,
                                      pickerInput(
                                        inputId = "twoCARTVar1",
                                        label = "Select all variables/data field to regress on (use to build the tree)",
                                        choices = c(1,2),
                                        selected = 1,
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 option")
                                      ),
                               ),
                               
                               column(width = 5,
                                      pickerInput(
                                        inputId = "twoCARTVar2",
                                        label = "Target Variable (to cluster by)",
                                        choices = c(1,2),
                                        selected = 1,
                                        options = list(`actions-box` = TRUE, size = 3, noneSelectedText = "Please select at least 1 option")
                                      ),
                               ),
                               column(width = 2,
                                      textInput("twoCARTName", "Name for GlobalGroups:", value = "CARTSegments")
                               ),
                               column(width = 12,
                                      br(),
                                      fluidRow(
                                        column(width = 6,
                                               h4("Algorithm Parameters"),
                                        ),
                                        column(width = 6, style = "margin-top: 10px;",
                                               p(" ", actionLink(inputId = "twoCARTText6",
                                                                 icon = icon("exclamation-triangle"),#triangle-exclamation
                                                                 label = HTML(""))),
                                               bsTooltip("twoCARTText6", title = "It is recommended these settings are left as default.")
                                        ),
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               numericInput("twoCARTmaxdepth", label = "Maximum Depth", value = 3, min = 1, max = 50, step = 1)
                                        ),
                                        column(width = 6, style = "margin-top: 25px;",
                                               p(" ", actionLink(inputId = "twoCARTText2",
                                                                 icon = icon("info-circle"),
                                                                 label = HTML(""))),
                                               bsTooltip("twoCARTText2", title = "Maximum depth of tree (how many splits are allowed)"),
                                        ),
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               numericInput("twoCARTcp", label = "CP", value = 0.00001, min = 0, step = 0.00001),
                                        ),
                                        column(width = 6, style = "margin-top: 25px;",
                                               p(" ", actionLink(inputId = "twoCARTText3",
                                                                 icon = icon("info-circle"),
                                                                 label = HTML(""))),
                                               bsTooltip("twoCARTText3", title = "Complexity parameter. Lower value can allow more splits."),
                                        ),
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               numericInput("twoCARTminbucket", label = "Minbucket", value = 20, min = 1, step = 1),
                                        ),
                                        column(width = 6, style = "margin-top: 25px;",
                                               p(" ", actionLink(inputId = "twoCARTText4",
                                                                 icon = icon("info-circle"),
                                                                 label = HTML(""))),
                                               bsTooltip("twoCARTText4", title = "Minimum number of individual that must be in a leaf node at the end of the algorithm."),
                                        ),
                                      ),
                                      fluidRow(
                                        column(width = 6,
                                               actionButton("twoCARTgo",label = "Go!")
                                        ),
                                        column(width = 6,
                                               
                                        ),
                                      )
                               ),
                               
                           ),
                         ),
                         fluidRow(
                           box(width = 12, title = "Decision Tree",
                               plotOutput("twoCARTTree"),
                               column(width = 3,
                                      checkboxInput("twoCARTTreeRules", "Tick to show rules", value = FALSE),
                               ),
                               column(width = 9, style = "margin-top: 10px;",
                                      p(" ", actionLink(inputId = "twoCARTText5",
                                                        icon = icon("info-circle"),
                                                        label = HTML(""))),
                                      bsTooltip("twoCARTText5", title = "If the labels overlap, it may be useful to view the rules seperately from the graph. Please note the CART algorithm clusters based on averages of the target variable; these are used to label the leaf nodes."),
                               ),
                               
                               uiOutput("twoCARTTreeRulesTableUI"),
                               textOutput("eCART")
                               
                           ),
                           box(width = 12, title = "Relative Segment Sizes",
                               pickerInput(
                                 inputId = "twoCARTTreeMapOptions",
                                 label = "Plot segment size by",
                                 choices = c("Number of individuals", "Total Cost", "Total Activity"),
                                 selected = "Number of individuals"
                               ),
                               plotlyOutput("twoCARTTreeMap"),
                               
                               plotlyOutput("twoCARTPie", height = '800px'),
                           ),
                         )
                  ),
                  column(width = 3,
                         fluidRow(
                           div(
                             box(width = 12, id = "CARTInfo",style = "overflow-y: scroll;", title = "CART Info",
                                 column(width = 12,
                                        fluidRow(
                                          p("Classification and Regression Tree (CART), referred to as 'Decision Trees' on this page, is a data-driven regression technique."),
                                          p("See ", a("Tree-Based Models", href="https://blog.dataiku.com/tree-based-models-how-they-work-in-plain-english"), " for how regression trees work."),
                                          
                                          p(" ", actionLink(inputId = "twoCARTInfoD1",
                                                            label = HTML("Why should I use decision trees?"),
                                                            icon = icon("info-circle"),
                                                            style='font-size:100%')),
                                          p(""),
                                          p(" ", actionLink(inputId = "twoCARTInfoD2",
                                                            label = HTML("I have a decision tree, but I don't understand the numbers."),
                                                            icon = icon("info-circle"),
                                                            style='font-size:100%')),
                                          p("Use decision trees either to segment the population, or to discover what attributes are most statictically significant in explaining differences (of the selected target variable)."),
                                          
                                          p(" ", actionLink(inputId = "viz_helpcart",
                                                            label = HTML("More information: definitions"),
                                                            icon = icon("info-circle"),
                                                            style='font-size:100%')),
                                          p(" ", actionLink(inputId = "modalGraphInfoCART",
                                                            label = HTML("General/Graph Tips"),
                                                            icon = icon("info-circle"),
                                                            style='font-size:100%'))
                                        )
                                 )
                             ), style = "position:fixed;  width:inherit;")
                         ),
                  ),
                ),
)