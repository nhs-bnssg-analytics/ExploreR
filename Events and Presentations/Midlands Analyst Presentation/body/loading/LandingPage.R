landingPage <- tabItem(tabName="landingPage",
                       tags$head(tags$script('
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();
        var boxHeight = window_height - header_height - 30 - 20 - 80;

        $("#landingInfo").height(boxHeight);
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
                         box(h4("Welcome to the ExploreR"),
                         ),
                         box(width = 3,
                             fluidRow(
                               column(width = 12,
                                      # div(
                                      img(src='bnssgccg-logo.jpg', height = '70px'),
                               )
                             )
                         ),

                         box(width = 3,
                             img(src='logo--default.png', height = '70px')
                         )

                       ),
                       fluidRow(
                         box(width = 9,
                             titlePanel("Population Dataset"),
                             wellPanel(
                               fluidRow(
                               uiOutput("zeroLoadingPopDataUI")
                               )
                             ),
                             fluidRow(
                             uiOutput("zeroLoadingAnalysisDataUI"),
                             )
                         ),

                         column(width = 3,
                                fluidRow(
                                  div(
                                  box(width = 12, id = "landingInfo", style = "overflow-y: scroll;",
                                      title = "How to use this tool",
                                      p("Please read the boxes on the right - these will explain the intended use for each page."),
                                      p("Clicking on ", actionLink(inputId = "zeroExampleLink",
                                                                label = HTML("links"),
                                                                icon = icon("info-circle"),
                                                                style='font-size:100%'), " provides further information."),
                                      p(actionLink(inputId = "zeroTerminology",
                                                                   label = HTML("Dataset Terminology"),
                                                                   icon = icon("info-circle"),
                                                                   style='font-size:100%')),
                                      p(actionLink(inputId = "zeroLandingSetup",
                                                   label = HTML("ExploreR Setup"),
                                                   icon = icon("info-circle"),
                                                   style='font-size:100%')),
                                      p(actionLink(inputId = "zeroContact",
                                                   label = HTML("About the ExploreR"),
                                                   icon = icon("info-circle"),
                                                   style='font-size:100%')),
                                      p(""),
                                      
                                  ), style = "position:fixed;  width:inherit;")

                                )
                         )
                       )
                       
                       
)