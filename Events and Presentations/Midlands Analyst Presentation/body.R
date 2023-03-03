
# mandatory_data_text <- c("Age", "Sex")
# mandatory_data_keyword <- c("age", "sex")
# # mandatory_data_meta <- c("demog.age","demog.sex")
# 
# optional_data_text <- c("Deprivation (IMD or other)", "Ethnicity", "Area")
# optional_data_keyword <- c("index", "ethn", "localit") # "deprivation", 
# # optional_data_meta <- c("socio.imd_decile", "demog.ethnicity", "area.main")

mandatory_data_text <- c("Age", "Sex", "Deprivation (IMD or other)", "Ethnicity", "Area")
mandatory_data_keyword <- c("age", "sex", "index", "ethn", "localit")



boxWithElements<-function(width, id, elements) {
  column(width = width,
         fluidRow(
           div(
             box(width = 12, id = id, style = "overflow-y: scroll;",
                 elements
             ), style = "position:fixed;  width:inherit;")
           
         )
  )
}




# Load tabs

# Section 0

source("./body/loading/setField.R", local = TRUE)


source("./body/loading/LandingPage.R", local = TRUE)
source("./body/loading/analysisDataset.R", local = TRUE)
# Summary graphs
source("./body/section1/cover.R", local = TRUE)
source("./body/section1/demog.R", local = TRUE)

# Segmentation
source("./body/segmentation/bth.R", local = TRUE)
source("./body/segmentation/cart.R", local = TRUE)

source("./body/cohort/cohortid.R", local = TRUE)



source("./body/theoplot/singleTheoUI.R", local = T, echo = F)
source("./body/theoplot/multiTheoUI.R", local = T, echo = F)

body <- dashboardBody(tags$script(HTML("$('body').addClass('fixed');")),
                      # tags$script('$(".sidebar-menu a[data-toggle=\'tab\']").click(function(){window.scrollTo({top: 0});})'),
                      tabItems(
                        
                        landingPage,
                        analysisDataPage,
                        
                        ## Tab 0.2 Data Loading ("zeroData")
                        tabItem(tabName="zeroData",
                                fluidRow(
                                  box(h4("Population Dataset Upload"),
                                  ),
                                  
                                ),
                                fluidRow(
                                  box(width = 9, 
                                      fluidRow(
                                        column(width = 12,
                                               radioButtons("zeroRad", "Select Data Upload Type", choices = c("Local Files", "SQL Query"), inline = TRUE)
                                        )
                                      ),
                                      uiOutput("ZeroDataUI"),
                                      column(width = 12,
                                             fluidRow(
                                               # h3("Status"),
                                               br(),
                                               textOutput("zeroLoadMessage")
                                             )
                                      )
                                  ),
                                  box(width = 3,
                                      p("Please upload the attributes / activity files. Click 'Load Data' once the files have been uploaded."),
                                      p("The tables must follow the format outlined in the supporting documentation."),
                                      p("Due to security concerns, the function to load the tables via SQL has been removed.")
                                  )
                                ),
                        ),
                        
                        ## Tab 0.3 Selecting Data Fields ("zeroField")
                        setField,
                        
                        ## Navigation Page
                        tabItem(tabName="navTab",
                                tags$head(tags$script('
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();
        var boxHeight = window_height - header_height - 30 - 20 - 80;

        $("#navInfo").height(boxHeight);
      };
      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });
      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
                                fluidRow(
                                  box(h4("Navigation"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs12"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate0")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 9,
                                         fluidRow(
                                           box(width = 12, title = "Select a Button",
                                               column(width = 12,
                                                      fluidRow(
                                                        actionButton("navTabJCohort", "Jump to Cohorts"),
                                                        hr()
                                                      ),
                                                      fluidRow(
                                                        actionButton("navTabJSG", "Jump to Summary Graphs"),
                                                        hr()
                                                      ),
                                                      fluidRow(
                                                        actionButton("navTabJSeg", "Jump to Segmentation"),
                                                        hr()
                                                      ),
                                                      fluidRow(
                                                        actionButton("navTabJTheo", "Jump to Theoplots"),
                                                        hr()
                                                      ),
                                                      fluidRow(
                                                        actionButton("navTabJRiskStrat", "Jump to Risk Stratification"),
                                                        hr()
                                                      )
                                               )
                                           ), # May need to add functions about adding to Global Groups TODO
                                           box(width = 12, title = "Edit GlobalGroups",
                                               column(width = 6,
                                                      fluidRow(
                                                        pickerInput("navTabCurrentGlobalVar", "Fields in GlobalGroup", choices = c(1,2), options = list(size = 12)),
                                                        hr()
                                                      )
                                               ),
                                               column(width = 3,style = "margin-top: 25px;",align="center",
                                                      fluidRow(
                                                        actionButton("navTabGlobalVarRemove", "Remove"),
                                                        hr()
                                                      )
                                               ),
                                               column(width = 3,style = "margin-top: 25px;",align="center",
                                                      fluidRow(
                                                        hr()
                                                      )
                                               ),
                                               column(width = 6,
                                                      fluidRow(
                                                        pickerInput("navTabGlobalVar", "Select Field of Interest", choices = c(1,2), options = list(size = 12)),
                                                        hr()
                                                      )
                                               ),
                                               column(width = 3,style = "margin-top: 25px;",align="center",
                                                      fluidRow(
                                                        actionButton("navTabGlobalVarAdd", "Add"),
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
                                                      textOutput("navTabMessage")
                                                      )
                                                      )
                                           )
                                         )
                                  ),
                                  column(width = 3,
                                                fluidRow(
                                                  div(
                                                    box(width = 12, id = "navInfo", style = "overflow-y: scroll;",
                                                        title = "Navigation Page",
                                                        p("The ExploreR is not designed to be a linear dashboard. 
                                                          Select a button, or an item in the sidebar, to jump to the relevant page."),
                                                        br(),
                                                        p(" ", actionLink(inputId = "navTabGlobal",
                                                                          label = HTML("GlobalGroups Info"),
                                                                          icon = icon("info-circle"),
                                                                          style='font-size:100%')),
                                                    ), style = "position:fixed;  width:inherit;")
                                                  
                                                )
                                )
                        ),
                        ),
                        
                        ## Summary graphs
                        cover,
                        
                        ## Demographic Page
                        demog,
                        
                        tabItem(tabName="oneClinic",
                                fluidRow(
                                  box(h4("Clinical Characteristics"),width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs2"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate3")
                                  )
                                  
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12, #title = "Clinical Characteristics",
                                               fluidRow(
                                                 column(width = 6,
                                                        selectInput("oneClinY", "Y Axis Field:", choices = c(1,2)),
                                                        selectizeInput("oneClinGroupBy", "Optional split by GlobalGroups:", choices = c(1,2),
                                                                       options = list(
                                                                         placeholder = 'Please select an option below',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                       )
                                                        ),
                                                 ),
                                                 
                                                 column(width = 6,
                                                 ),
                                               ),
                                               plotlyOutput("oneClinPlot1"),
                                               textOutput("oneClinPlot1E"),
                                               
                                               column(width = 12,
                                                      fluidRow(
                                                        br(),
                                                        br(),
                                                        h3("Condition Prevalence"),
                                                        selectizeInput("oneClinMulti", "Select clinical conditions to plot", choices = c(1,2), multiple = TRUE,
                                                                       options = list(
                                                                         placeholder = 'Please select at least one option below',
                                                                         onInitialize = I('function() { this.setValue(""); }')
                                                                       )
                                                        ),
                                                        plotlyOutput("graph1")
                                                      )
                                               )
                                           )
                                         )
                                  ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12,
                                                 p("Page for the distribution of clinical condition / multimorbidity within the population."),
                                                 p(" "),
                                                 p(" ", actionLink(inputId = "viz_help2",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoClin",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%'))
                                             )
                                             , style = "position:fixed;  width:inherit;"
                                           ))
                                  ),
                                  
                                ),     
                        ),
                        
                        tabItem(tabName="oneActivity",
                                fluidRow(
                                  box(h4("Activity Overview"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs3"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate4")
                                  )
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12, #title = "Activity Overview",
                                               selectInput("oneActCoA", "Cost or Activity Count", choices = c("Spend (Total, £s)", "Spend per Capita (£s)", "Activity", "Activity per Capita")),
                                               
                                               selectInput("oneActVertical", "Y Axis", choices = c(1,2)),
                                               checkboxGroupInput("oneActX", "Untick to exclude POD"),
                                               column(width = 12,
                                                      plotlyOutput("oneActivityPlot2", height = "800px")
                                               ),
                                               uiOutput("oneActTable1")
                                           ),
                                           box(width = 12,
                                               title = "Cumulative Costs",
                                               plotlyOutput("oneActGraph2"),
                                               textOutput("oneActGraph2E")
                                           )),
                                         ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12,
                                                 p("These graphs show resource allocation (spending) and activity in the system. Use the dropdowns to change the information displayed."),
                                                 p("The second graph on this page is a Lorenz Curve of spending, useful for observations such as '50% of the resources are consumed by 5% of the population'."),
                                                 p("Please note spending per capita refers to active service user (patient with relevant activity - i.e. individuals with no activity in the system are not accounted for in the per capita scaling)"),
                                                 p(" ", actionLink(inputId = "viz_helpActDataCalc",
                                                                   label = HTML("How is the data calculated?"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "viz_help3",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoAct",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                             ), style = "position:fixed;  width:inherit;")
                                         ))
                                )
                        ),
                        tabItem(tabName="oneDeprivation",
                                fluidRow(
                                  box(h4("Deprivation"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs4"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate5")
                                  )
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12, #title = "Deprivation",
                                               p("On this page, 'Deprivation' refers to the Index of Multiple Deprivation Deciles, where 1 indicates most deprived, 10 least."),
                                               p("Data on this page is always split by IMD. The x-axis on this page cannot be changed."),
                                               selectInput("oneDeprivationY", "Y Axis Field:", choices = c(1,2)),
                                               selectizeInput("oneDeprivationGroupBy", "Optional split by GlobalGroups:", choices = c(1,2),
                                                              options = list(
                                                                placeholder = 'Please select an option below',
                                                                onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                               ),
                                               uiOutput("uioneDeprivationPlot1")
                                           ),
                                         ),
                                  ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12, 
                                                 p("Deprivation is often a good indicator of health inequalities. This page is for highlighting these differences."),
                                                 p("Use the dropdowns to change the information displayed."),
                                                 p(" ", actionLink(inputId = "viz_help4",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoDep",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%'))
                                             ), style = "position:fixed;  width:inherit;")
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName="oneGeo",
                                fluidRow(
                                  box(h4("Geography"), width = 4,
                                      # box(h4(tags$b("PCN: "), input$pcn)
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs5"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate6")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12, #title = "Geography",
                                               selectInput("oneGeoX", "Selected Area/Geographical Field:", choices = c(1,2)),
                                               selectInput("oneGeoY", "Y Axis Field:", choices = c(1,2)),
                                               selectizeInput("oneGeoGroupBy", "Optional split by GlobalGroups:", choices = c(1,2),
                                                              options = list(
                                                                placeholder = 'Please select an option below',
                                                                onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                               ),
                                               uiOutput("uioneGeoPlot1")
                                           ),
                                         )
                                  ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12,
                                                 p("This page is for highlighting differences between geographical areas."),
                                                 p("Use the dropdowns to change the information displayed."),
                                                 p(" ", actionLink(inputId = "viz_help5",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoArea",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%'))
                                             ), style = "position:fixed;  width:inherit;")
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName="oneWidDet",
                                fluidRow(
                                  box(h4("Wider Determinants"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs6"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate7")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12, # title = "Wider Determinants",
                                               # have an option to compare determinants? have a map? how to handle continous variables
                                               selectInput("oneWidDetX", "Please Select a Determinant (X-axis):", choices = c(1,2)),
                                               selectInput("oneWidDetY", "Y Axis Field:", choices = c(1,2)),
                                               selectizeInput("oneWidDetGroupBy", "Optional split by GlobalGroups:", choices = c(1,2),
                                                              options = list(
                                                                placeholder = 'Please select an option below',
                                                                onInitialize = I('function() { this.setValue(""); }')
                                                              )
                                               ),
                                               uiOutput("uioneWidDetPlot1")
                                           )
                                         )
                                  ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12,
                                                 p("There are many factors that either contribute to, or act as an indicator of inequality. This page can be used to explore the distribution of some of these determinants."),
                                                 p("Use the dropdowns to change the information displayed."),
                                                 p("When looking at deciles, 1 indicates most deprived, whereas 10 means least ",
                                                   "deprived. Please note explanations of the determinants are not available."),
                                                 p("Changing the selected determinant to a different measure will update the 'Wider Determinant' field in dropdowns on other pages to refer to the new selection."),
                                                 p(" ", actionLink(inputId = "viz_help6",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoWider",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                             ), style = "position:fixed;  width:inherit;")
                                         )
                                  )
                                )
                        ),
                        tabItem(tabName="oneGBars",
                                fluidRow(
                                  box(h4("Generalised Barcharts"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs15"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate16")
                                  )
                                ),
                                fluidRow(
                                  column(width = 10,
                                         fluidRow(
                                           box(width = 12,
                                               pickerInput("twoClusterVar1", "Segment bars by", choices = c(1,2), options = list(size = 12)),
                                               pickerInput("twoClusterVar2", "Y Axis", choices = c(1,2), options = list(size = 12)),
                                               
                                               selectInput(inputId = "twoClustersegment_layout",
                                                           label = "Compare segments together or individually",
                                                           choices = list(Stacked = "stacked",
                                                                          Separate = "separate")),
                                               
                                               selectInput(inputId = "twoClustersegment_percent",
                                                           label = "Percent or count of population",
                                                           choices = list(Percent="percent",Count="freq")),
                                               plotlyOutput("twoClusterTestPlot"),
                                           )
                                         )
                                  ),
                                  column(width = 2,
                                         fluidRow(
                                           div(
                                             box(width = 12,
                                                 p("Select two fields to generate flexible graphs."),
                                                 p("Selected fields must not be the same, and must have fewer than 20 unique elements each (otherwise graph will not update)."),
                                                 p("This restriction is in place due to plotting time contraints."),
                                                 # p(" ", actionLink(inputId = "viz_help6",
                                                 #                   label = HTML("More information: definitions"),
                                                 #                   icon = icon("info-circle"),
                                                 #                   style='font-size:100%')),
                                                 # p(" ", actionLink(inputId = "modalGraphInfoWider",
                                                 #                   label = HTML("General/Graph Tips"),
                                                 #                   icon = icon("info-circle"),
                                                 #                   style='font-size:100%')),
                                             ), style = "position:fixed;  width:inherit;")
                                         )
                                  )
                                )
                        ),
                        
                        
                        
                        ## Section 2 Segmentation
                        tabItem(tabName="twoLTCAge",
                                fluidRow(
                                  box(h4("Chronic Conditions by Age"), width = 4,
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxs8"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate8")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 9,
                                         fluidRow(
                                           box(width = 12,
                                               fluidRow(
                                                 column(width = 3, numericInput("clinAge1", "Pediatric age limit: 0 to _", value = 17, min = 1, step = 1)),# Pediatric age limit
                                                 column(width = 3, numericInput("clinAge2", "Elderly age limit: _ to max", value = 74, min = 1, step = 1)),#Elderly age limit
                                                 column(width = 3, numericInput("clinLTC1", "Low complexity limit: 0 to _", value = 2, min = 1, step = 1)),#Low complexity limit
                                                 column(width = 3, numericInput("clinLTC2", "High complexity limit: _ to max", value = 4, min = 1, step = 1)),# High complexity limit   
                                                 
                                                 ),
                                               fluidRow(
                                                 column(width = 3, 
                                                        # pickerInput(
                                                        #   inputId = "ltcSelection",
                                                        #   label = "Select Clinical Conditions To Display",
                                                        #   choices = c("All Conditions","Major Conditions", "Minor Conditions"),
                                                        #   selected = "All Conditions",
                                                        # ),
                                                        selectInput("ltcSelection", "Type of Clinical Conditions To Display", choices = c("All Conditions","Major Conditions", "Minor Conditions"), selected = "All Conditions")
                                                 ),
                                                 column(width = 3,
                                                        pickerInput(
                                                          inputId = "twoLTCAgeLTCSelection",
                                                          label = "Clinical Conditions To Include",
                                                          choices = c("All Conditions","Major Conditions", "Minor Conditions"),
                                                          selected = "All Conditions",
                                                          multiple = TRUE,
                                                          options = list(`actions-box` = TRUE, size = 9, noneSelectedText = "Please select at least 1 option")
                                                        ),
                                                 ),
                                                 column(width = 3,
                                                        textInput("twoClinAgeName", "Name for GlobalGroups:", value = "ConditionsByAge")
                                                 ),
                                                 column(width = 3, style = "margin-top: 25px;",
                                                        actionButton("clin3x3Plot", "Go!")
                                                 ),
                                                 
                                                 column(width = 12,
                                                        
                                                        plotOutput("clin3x3graph"),
                                                        
                                                        pickerInput(
                                                          inputId = "two3x3gTreeMapOptions",
                                                          label = "Plot segment size by",
                                                          choices = c("Number of individuals", "Total Cost", "Total Activity"),
                                                          selected = "Number of individuals"
                                                        ),
                                                        plotlyOutput("two3x3TreeMap"),
                                                        
                                                        plotlyOutput("two3x3Pie", height = '800px'),
                                                 )
                                               ),
                                           )
                                         )
                                  ),
                                  
                                  column(width = 3,
                                         fluidRow(
                                           div(
                                             box(width = 12, title = "Segmenting by Age and Multimorbidity",
                                                 # p("There many ways to segment a population. 
                                                 p("This page provides an intuitive 3 by 3 matrix, using age and complexity (multimorbidity) to segment the population."),
                                                 p("Use the available options to configure the bounds for the matrix row and columns."),
                                                 p("Numbers entered are used as an upper bound (<= is used when calculating)."),# If this is confusing you, compare the default numbers to the graph headers."),
                                                 p("Please note this graph is not interactive."),
                                                 p(" ", actionLink(inputId = "viz_helpM",
                                                                   label = HTML("More information: definitions"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                                 p(" ", actionLink(inputId = "modalGraphInfoM",
                                                                   label = HTML("General/Graph Tips"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%'))
                                             ), style = "position:fixed;  width:inherit;"
                                           )
                                         )
                                  )
                                )
                        ),
                        
                        
                        bth,
                        
                        cart,
                        
                        tabItem(tabName="twoClusters",
                                fluidRow(
                                  box(h4("Clustering"), width = 4,
                                  ),
                                  
                                  box(background = "green",width = 4,
                                      uiOutput("oneCoverBoxs10"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate11")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 9,
                                         fluidRow(
                                           box(width = 12, collapsible = TRUE, title = "Variable Setup",
                                               column(width = 6,
                                                      pickerInput(
                                                        inputId = "twoClusterVars",
                                                        label = "Select variables to use",
                                                        choices = c(1,2),
                                                        selected = 1,
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable")
                                                      ),
                                               ),
                                               column(width = 6,
                                                      textInput("twoClusterName", "Name for GlobalGroups:", value = "KMeansClusters")
                                               ),
                                               column(width = 12,
                                                      
                                                      fluidRow(
                                                        column(width = 6,
                                                               numericInput("twoClusterCenter", label = "Number of clusters", value = 6, min = 1, max = 50, step = 1)
                                                        ),
                                                        column(width = 6, style = "margin-top: 25px;",
                                                               p(" ", actionLink(inputId = "twoKMeansText1",
                                                                                 icon = icon("info-circle"),
                                                                                 label = HTML(""))),
                                                               bsTooltip("twoKMeansText1", title = "How many clusters you would like K-Means to produce. Some trial and error may be required - factors such as expected result and the data used affect the optimal number."),
                                                        ),
                                                      ),
                                                      fluidRow(
                                                        column(width = 6,
                                                               actionButton("twoClusterGo",label = "Go!")
                                                        ),
                                                        column(width = 6,
                                                               
                                                        )
                                                      ),
                                                      fluidRow(
                                                        textOutput("twoEE")
                                                      )
                                               )
                                           )
                                         ),
                                         fluidRow(
                                           box(width = 12, title = "Relative Segment Sizes",
                                               pickerInput(
                                                 inputId = "twoClusterTreeMapOptions",
                                                 label = "Plot segment size by",
                                                 choices = c("Number of individuals", "Total Cost", "Total Activity"),
                                                 selected = "Number of individuals"
                                               ),
                                               
                                               plotlyOutput("twoClusterTreeMap"),
                                               
                                               plotlyOutput("twoClusterPie", height = '800px'),
                                           ),
                                         )
                                  ),
                                  column(width = 3,
                                         fluidRow(
                                           div(
                                             box(width = 12, id = "CARTInfo",style = "overflow-y: scroll;", title = "K-Means Info",
                                                 column(width = 12,
                                                        fluidRow(
                                                          p("The objective of K-Means is simple: group similar data points (in this case patients) together and discover underlying patterns.
                                                            To achieve this objective, K-Means looks for a fixed number of clusters in a dataset (specified at the start).
                                                            A cluster refers to a collection of data points aggregated together because of certain similarities."),
                                                          p("Due to data potentially containing quantitative and qualitative variables Factor analysis of mixed data (FAMD) is used to pre-process the data."),
                                                          p(" ", actionLink(inputId = "viz_helpKMeans",
                                                                            label = HTML("More information: definitions"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%')),
                                                          p(" ", actionLink(inputId = "modalGraphInfoKMeans",
                                                                            label = HTML("General/Graph Tips"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%'))
                                                        )
                                                 )
                                             ), style = "position:fixed;  width:inherit;")
                                         ),
                                  ),
                                ),
                        ),

                        ## Tab 3 Cohort ID
                        #
                        cohort1,
                        
                        ## Tab 4 Theoplot
                        #
                        tabItem(tabName="fourTab",
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
                                                           # source("./body/theoplot/singleTheoUI.R", local = T, echo = F)
                                                           s1tab
                                                  ),
                                                  tabPanel("Group Theoplots",
                                                           s2tab
                                                           # source("./body/theoplot/multiTheoUI.R", local = T)
                                                  ),
                                                  type = "tabs"
                                      )
                                  )
                                )
                        ),
                        
                        tabItem(tabName="fiveTab",
                                fluidRow(
                                  box(h4("Risk Stratification and Forecasting via Regression"), width = 4,
                                  ),
                                  box(background = "green",width = 4,
                                      uiOutput("oneCoverBoxs14"),
                                  ),
                                  box(background = "green", width = 4,
                                      uiOutput("oneCoverBoxDate14")
                                      # h4("Year: 2020-21"),
                                  )
                                ),
                                fluidRow(
                                  column(width = 9,
                                         fluidRow(
                                           box(width = 12, collapsible = TRUE, title = "Variable Setup",
                                               column(width = 5,
                                                      pickerInput(
                                                        inputId = "twoRisk1Var1",
                                                        label = "Using variables",
                                                        choices = c(1,2),
                                                        selected = 1,
                                                        multiple = TRUE,
                                                        options = list(`actions-box` = TRUE, size = 9, noneSelectedText = "Please select at least 1 option")
                                                      ),
                                               ),
                                               column(width = 3,
                                                      pickerInput(
                                                        inputId = "twoRisk1Var2",
                                                        label = "Predict",
                                                        choices = c("Likelihood of activity [0,1]", "Expected activity (count)","Expected cost (£s)"),# 
                                                        selected = 1
                                                      ),
                                               ),
                                               column(width = 4,
                                                      HTML("<b>Of activity type(s)</b>"),
                                                      shinyTree("fiveRiskTargetTree", checkbox = TRUE, themeIcons = FALSE),
                                               ),
                                               column(width = 12,
                                                      br(),
                                                      
                                                      # Using a hurdle model: first, find all individuals who are expected to have some activity/cost
                                                      # THEN, for those who we expect a non-0 result, feed into a model
                                                      fluidRow(
                                                        column(width = 6,
                                                               numericInput("fiveRiskMinProb", "Logistic model cut-off value", value = 0.5, min = 0, max = 1, step = 0.0001)
                                                        ),
                                                        column(width = 6,
                                                               p(" ", actionLink(inputId = "fiveRisk1Text1",
                                                                                 icon = icon("info-circle"),
                                                                                 label = HTML(""))),
                                                               bsTooltip("fiveRisk1Text1", title = "Information about logistic cut-off value.")
                                                        ),
                                                      ),
                                                      fluidRow(
                                                        column(width = 6,
                                                               actionButton("fiveRisk1go",label = "Go!")
                                                        ),
                                                        column(width = 6,
                                                               
                                                        ),
                                                      )
                                               ),

                                           ),
                                         ),
                                         fluidRow(
                                           box(width = 12,title = "Fitted Models",
                                               
                                               uiOutput("fiveRisk1Out0"),
                                               br(),
                                               uiOutput("fiveRisk1Out1"),
                                               br(),
                                               uiOutput("fiveRisk1Out2"),
                                               fluidRow(
                                                 column(width = 4,
                                                        # checkboxInput("fiveRiskLogScale", "Scale plot by log(1+p)", value = T),
                                                        # sliderInput("fiveRiskSlider", "% of individuals to plot (for increased performance)", min = 5, max = 100, value = 100, step = 5),
                                                 ),
                                                 column(width = 2,
                                                        br(),
                                                        # p(" ", actionLink(inputId = "fiveRisk1Text2",
                                                        #                   icon = icon("info-circle"),
                                                        #                   label = HTML(""))),
                                                        # bsTooltip("fiveRisk1Text2", title = "It can be visually better to log(1+p) transform the data before plotting it. This can help improve plotting performance.")   
                                                 ),
                                                 column(width = 6,
                                                        uiOutput("fiveRisk1DownloadUI")
                                                        ),
                                               ),
                                               
                                              
                                               fluidRow(
                                                 
                                                 column(width = 6,
                                                        column(width = 12,
                                                               # h3("Logistic Regression")
                                                        ),
                                                        plotlyOutput("fiveRiskBox11")
                                                        ),
                                                 column(width = 6,
                                                        plotOutput("fiveRiskBox12")
                                                 )
                                               ),
                                               
                                               uiOutput("fiveRiskBox2"),
                                               uiOutput("fiveRiskBox3"),
                                               uiOutput("fiveRiskBox4")
                                           )
                                         )
                                  ),
                                  column(width = 3,
                                         fluidRow(
                                           # div(
                                             box(width = 12, id = "Risk1Info",style = "overflow-y: scroll;",
                                                 column(width = 12,
                                                        fluidRow(
                                                          p("This page uses regression-based models to predict expected costs and activity in the system."),
                                                          p(" ", actionLink(inputId = "fiveRisk1InfoD1",
                                                                            label = HTML("About this page"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%')),
                                                          p(""),
                                                          p(" ", actionLink(inputId = "fiveRisk1InfoD2",
                                                                            label = HTML("Technical details"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%')),
                                                          p(" ", actionLink(inputId = "fiveRisk1InfoD3",
                                                                            label = HTML("Understanding the outputs"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%')),
                                                          
                                                          p(" ", actionLink(inputId = "viz_helpRisk1",
                                                                            label = HTML("More information: definitions"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%')),
                                                          p(" ", actionLink(inputId = "modalGraphInfoRisk1",
                                                                            label = HTML("General/Graph Tips"),
                                                                            icon = icon("info-circle"),
                                                                            style='font-size:100%'))
                                                        )
                                                 )
                                             )#, style = "position:fixed;  width:inherit;")
                                         ),
                                  ),
                                ),
                        )
                        
                      ) 
)