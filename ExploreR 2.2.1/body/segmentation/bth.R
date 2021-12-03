bth <- tabItem(tabName="twoBTH",
               
               tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 30 - 20 - 80 - 20;

        $("#bthInfo").height(boxHeight);
        // $("#CARTInfo").height(boxHeight);
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
                 box(h4("Bridge to Health Segment Proportions"), width = 4,
                 ),
                 box(background = "green",width=4,
                     uiOutput("oneCoverBoxs7"),
                 ),
                 box(background = "green", width = 4,
                     uiOutput("oneCoverBoxDate9")
                     # h4("Year: 2020-21"),
                 )
               ),
               fluidRow(
                 column(width = 9,
                        fluidRow(
                          box(width = 12,
                              selectizeInput(inputId = "twoBTHFocus",
                                             label = "Split Segments",
                                             choices = c(1,2),
                                             options = list(
                                               placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }'))),
                              selectInput(inputId = "twoBTHsegment_layout",
                                          label = "Compare segments together or individually",
                                          choices = list(Stacked = "stacked",
                                                         Separate = "separate")),
                              
                              selectInput(inputId = "twoBTHsegment_percent",
                                          label = "Percent or count of population",
                                          choices = list(Percent="percent",Count="freq")),
                              
                              plotlyOutput("twoBTHPlot",
                                           height = 800)
                              
                          ),
                        ),
                          fluidRow(
                            box(width = 12,
                                       pickerInput(
                                         inputId = "twoBtHTreeMapOptions",
                                         label = "Plot segment size by",
                                         choices = c("Number of individuals", "Total Cost", "Total Activity"),
                                         selected = "Number of individuals"
                                       ),
                                       plotlyOutput("twoBthTreeMap"),
                                       
                                       plotlyOutput("twoBthPie", height = '800px'),
                            )
                            
                          )
                 ),
                 column(width = 3,
                        fluidRow(
                          div(
                              box(width = 12, id = "bthInfo",style = "overflow-y: scroll;",title = "Bridges to Health Definitions",
                                             p("This page compares the composition of the population by the Bridges to Health segmentation method."),
                                             
                                             p("The segments in this method are defined as follows:"),
                                             
                                             tableOutput("seg_definitions"),

                                             p(" ", actionLink(inputId = "viz_helpseg",
                                                               label = HTML("More information: definitions"),
                                                               icon = icon("info-circle"),
                                                               style='font-size:100%')),
                                             p(" ", actionLink(inputId = "modalGraphInfoSeg",
                                                               label = HTML("General/Graph Tips"),
                                                               icon = icon("info-circle"),
                                                               style='font-size:100%'))
                              ), style = "position:fixed;  width:inherit;")
                        )
                 )
               )
)
