setField <- tabItem(tabName="zeroField",
                    fluidRow(
                      box(h4("Population Dataset Field Definitions"),
                      )
                    ),
                    fluidRow(
                      column(width = 9,
                             fluidRow(
                               box(width = 12, title = "Attributes Mapping",
                                   # This will need to be an lapply mapped over all required fields.
                                   lapply(mandatory_data_keyword, function(x) {
                                     fluidRow(
                                       column(width = 6,
                                              textOutput(paste0("zeroFieldAssignText",x))
                                       ),
                                       column(width = 6,
                                              selectizeInput(paste0("zeroFieldAssignSelect",x), paste0("Please select a field"), choices = c(1,2),
                                                             options = list(
                                                               placeholder = 'Please select an option',
                                                               onInitialize = I('function() { this.setValue(""); }')
                                                             )
                                              )
                                       )
                                     )
                                   }),

                                   selectizeInput("ltcDropdown", paste0("Select all LTC Fields"), choices = c(1,2), multiple = TRUE,
                                                  options = list(
                                                    placeholder = 'Please select at least one option',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                  )
                                   ),
                                   #####################################################
                                   # pickerInput(
                                   #   inputId = "simpleLTCs",
                                   #   label = "Select Simple LTCs",
                                   #   choices = c(1,2),
                                   #   selected = c(1,2),
                                   #   options = list(
                                   #     `actions-box` = TRUE),
                                   #   multiple = TRUE
                                   # ),
                                   # pickerInput(
                                   #   inputId = "complexLTCs",
                                   #   label = "Select Complex LTCs",
                                   #   choices = c(1,2),
                                   #   selected = c(1,2),
                                   #   options = list(
                                   #     `actions-box` = TRUE),
                                   #   multiple = TRUE
                                   # )
                                   selectInput(
                                     inputId = "simpleLTCs",
                                     label = "Select Simple LTCs",
                                     choices = c(1,2),
                                     selected = c(1,2),
                                     # options = list(
                                     #   `actions-box` = TRUE),
                                     multiple = TRUE
                                   ),
                                   selectInput(
                                     inputId = "complexLTCs",
                                     label = "Select Complex LTCs",
                                     choices = c(1,2),
                                     selected = c(1,2),
                                     # options = list(
                                     #   `actions-box` = TRUE),
                                     multiple = TRUE
                                   ),
                                   ## Selecting wider determinants
                                   pickerInput("DeterminantCheckBox", "Select Wider Determinants", choices = c(1,2), multiple = TRUE,
                                               options = list(
                                                 `actions-box` = TRUE))
                               ),
                               
                               box(width = 12, title = "Bridges to Health Segments",
                                   ### BtH segmentation setup
                                   # h3("Bridges to Health Segment Definitions"),
                                   p("Please select the appropriate fields for each segment, corresponding to the BtH segment definitions."),
                                   p("Note, for each segment, the union of eavh selected field is taken."),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHFrailty", "Frailty", choices = c(1,2),
                                                           options = list(
                                                             placeholder = 'Please select frailty',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHFrailtyUI")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHLimitedReserve", "Limited Reserve (organ failure)", choices = c(1,2),
                                                           multiple = TRUE,
                                                           options = list(
                                                             placeholder = 'Please select at least one option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHLimitedReserveUI")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHDecline", "Short Period of Decline", choices = c(1,2),
                                                           multiple = TRUE,
                                                           options = list(
                                                             placeholder = 'Please select at least one option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHDeclineUI")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHDisability", "Stable But Serious Disability", choices = c(1,2),
                                                           multiple = TRUE,
                                                           options = list(
                                                             placeholder = 'Please select at least one option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHDisabilityUI")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHChronic", "Chronic Conditions", choices = c(1,2),
                                                           multiple = TRUE,
                                                           options = list(
                                                             placeholder = 'Please select at least one option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHChronicUI")
                                     )
                                   ),
                                   fluidRow(
                                     column(width = 4,
                                            selectizeInput("twoBTHMaternal", "Maternal", choices = c(1,2),
                                                           options = list(
                                                             placeholder = 'Please select field indicating pregnancy',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                                     ),
                                     column(width = 8,
                                            uiOutput("twoBTHMaternalUI")
                                     )
                                   ),
                                   ## A&E identifier
                                   p("Acutely Ill"),
                                   
                                   tabsetPanel(
                                     id = "ByActivitytabs",
                                     type = "tabs",
                                     tabPanel("By Activity", 
                                                  fluidRow(
                                                    # This needs to become a selectize input if missing data - fix later
                                                    column(width = 6, selectInput("aeidentifierCol", "Please select the appropriate column", choices = c(1,2))),
                                                    column(width = 6, selectInput("aeidentifierVal", "Please select the value corresponding to A&E", choices = c(1,2)))
                                                  )
                                     ),
                                     tabPanel("By Attributes",
                                              fluidRow(
                                                column(width = 4,
                                                       selectizeInput("twoBTHAcute", "Acute", choices = c(1,2),
                                                                      options = list(
                                                                        placeholder = 'Please select field indicating ae attendance',
                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                      )
                                                       )
                                                ),
                                                column(width = 8,
                                                       uiOutput("twoBTHAcuteUI")
                                                )
                                              ),
                                              
                                                  )
                                   ),
                                   hr(),
                                   br(),
                                   br(),
                                   actionButton("zeroThreeProceed", "Go!"),
                                   br(),
                                   br(),
                                   br()
                                   
                               ),
                               # box(width = 12,
                                   
                               # ),
                             )
                      ),
                      column(width = 3,
                             div(style = "position:fixed;  width:10;",
                                 box(width = 12, height = '915px', title = "Field Assignment",
                                     div(style = 'height:895px; overflow-y: scroll',
                                         
                                         # column(width = 12,
                                         p(strong("Attributes Mapping:")),
                                         p("Please select the required column(s) for each of dropdowns present."),
                                         br(),
                                         p(strong("Bridges to Health Segments:")),
                                         p("For each of the segments, select the field(s) that are used to define them."),
                                         p("When a field is selected, use the options to the right of the dropdown to select which values of the field indicate membership in the BtH segment."),
                                         p("When multiple elements are selected in a dropdown, the union of them is used to define the BtH segment."),
                                         
                                         # p("When multiple elements are selected in a dropdown, the union of them is used to define the BtH segment"),
                                         
                                     )
                                     # style = "overflow: hidden; overflow-y: auto; position:fixed;  width:inherit;")
                                 )
                             )
                             
                             
                      )
                    )
)