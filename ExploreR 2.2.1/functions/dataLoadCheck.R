
landingUIPart1 <- function(dat, dat2) {
  # browser()
  if(class(dat2) == "try-error") {
    # If no data is loaded, tell user to load some
    column(width = 12,
           p("No loaded Population Data found."),
           p("Please upload the relevant data files to proceed."),
           actionButton("zeroUploadPopData", "Upload Files")
    )
  } else {
    column(width = 12,
           p("Loaded and processed data found."),
           p(paste0("There are ", format(nrow(dat$attributes), big.mark = ","), " individuals in Population Dataset. There are ", format(sum(dat$attributes$total_act), big.mark = ","), " points of activity across all PODs.")),
           p("If you would like to use a different Population Dataset, please upload the relevant data files."),
           actionButton("zeroUploadPopData", "Upload Files")
    )
  }
}


landingUIPart2 <- function(dat, dat2) {
  if(class(dat2) != "try-error") {
    sets <- colnames(dat2$AnalysisDatasets)
    sets <- setNames(sets, makeNice(sets))
    updatePickerInput(session, "zeroAnalysisDatasetToRemove", choices = sets[-1])
  }
  # browser()
  if(class(dat2) == "try-error") {
    # If no data is loaded, show nothing
    list(p(""))
  } else {
    column(width = 12,
           h3("Analysis Dataset"),
           wellPanel(
             fluidRow(
               column(width = 12,
                      p("Please select an analysis dataset from the list below, or click to add a new set."),
                      fluidRow(
                        column(width = 8,
                               pickerInput("zeroAnalysisDataPicker", "Available Datasets:", choices = sets, selected = "Population Dataset")
                        ),
                        column(width = 4, style = "margin-top: 25px;",
                               actionButton("zeroAnalysisDataPickerAdd", "Add New Analysis Dataset")
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               uiOutput("zeroAnalysisDataText")
                        )
                      )
               ),
             ),
             
           ),
           column(width = 12,
                  actionButton("zeroLoadingMainGo", "Go!")
           )
    )
  }
}


dataLoadCheck <- function(attributes, activity, output) {
  # function to check data is in expected format (does or doesn't have the correct column names)
  # then render the next page if the format seems fine
  
  # so, what needs to be checked?
  # Activity table first 
  # NOTE: NA or NULLs need to be replaced with unknown - with the exception of numerical data. Further thoughts need to be had on this
  
  actCols <- colnames(activity)
  
  validate(
    need("id" %in% actCols, "Error: Activity table missing id column.")
  )
  validate(
    need("pod_l1" %in% actCols, "Error: Activity table missing pod column.")
  )
  # For the following: should we instead assume missing values to take the other column's?
  validate(
    need("arr_date" %in% actCols, "Error: Activity table missing arrival date column.")
  )
  validate(
    need("dep_date" %in% actCols, "Error: Activity table missing departure date column.")
  )
  validate(
    need("spec" %in% actCols, "Error: Activity table missing spec column.")
  )
  validate(
    need("cost" %in% actCols, "Error: Activity table missing cost column.")
  )
  
  # Attributes columns checking
  
  attCols <- colnames(attributes)
  
  validate(
    need("id" %in% attCols, "Error: Attributes table missing id column.")
  )
  # Check for at least one clinical column
  clin <- attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames()
  
  validate(
    need(clin > 0, "Attributes table must have at least one clinical field.")
  )
  # renderThree()

  return(TRUE)
}

renderThree <- function() {

  lapply(1:5, function(x) {
    output[[paste0("zeroFieldAssignText",mandatory_data_keyword[x])]] <- renderText({paste0("Please select the field corresponding to ", mandatory_data_text[x])})
  })
  # paste0("zeroFieldAssignSelect",mandatory_data_keyword[x])
  ## Try to find a default field - if nothing, then leave the field empty
  lapply(mandatory_data_keyword, function(x) {
    i <- colnames(attributes)[which(grepl(x, colnames(attributes), fixed=TRUE))]
    updateSelectizeInput(session, paste0("zeroFieldAssignSelect",x), choices = colnames(attributes) %>% makeNice %>% sw, selected = if (is_empty(i)) {NULL} else {i[1]} ,
                         options = list(
                           placeholder = 'Please select an option',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
  })
  
  updatePickerInput(session, "DeterminantCheckBox", choices = colnames(attributes) %>% makeNice %>% sw, selected = colnames(attributes)[startsWith(colnames(attributes),"socio")])
  updateSelectInput(session, "aeidentifierCol", choices = colnames(activity)[-1])
  
  lapply(c("twoBTHLimitedReserve","twoBTHDecline","twoBTHDisability","twoBTHChronic"), function(x) {
    updateSelectizeInput(session, x, choices = colnames(attributes) %>% makeNice %>% sw,
                           # (attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames() %>% makeNice %>% sw),
                         options = list(
                           placeholder = 'Please select at least one option',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
  })
  updateSelectizeInput(session, "twoBTHMaternal", choices = colnames(attributes) %>% makeNice %>% sw,
                       options = list(
                         placeholder = 'Please select the relevant field',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  updateSelectizeInput(session, "twoBTHFrailty", choices = colnames(attributes) %>% makeNice %>% sw,
                       options = list(
                         placeholder = 'Please select the relevant field',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  updateSelectizeInput(session, "ltcDropdown", choices = (attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames() %>% makeNice %>% sw),
                       options = list(
                         placeholder = 'Please select all LTCs',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  updateSelectizeInput(session, "twoBTHAcute", choices = colnames(attributes) %>% makeNice %>% sw,
                       options = list(
                         placeholder = 'Please select the relevant field(s)',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  
  
  updateSelectInput(session, "simpleLTCs", choices = (attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames() %>% makeNice %>% sw)
  )
  updateSelectInput(session, "complexLTCs", choices = (attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames() %>% makeNice %>% sw)
  )
  
}


updateAnalysisDatasetIDUI <- function(toLoad, input, output, dat2, dat) {
  print("Updating UI")
  
  output$zeroAnalysisDataUI <- renderUI({
    lapply(1:length(dat2$AnalysisDataset$rules), function(x) {
      s2 <- dat2$AnalysisDataset$rules[[x]][1]
      names(s2) <- s2 %>% removePrefix %>% makeNice
      
      t <- dat2$groupByList[!is.na(dat2$groupByList)]
      names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
      
      t2 <- c("total_cost", "total_act")
      names(t2) <- sapply(c("total_cost", "total_act"),makeNicePPY)
      
      cm <- dat$attributes%>%select(starts_with("clinic.misc")) %>% colnames
      names(cm) <- sapply(cm, removePrefix) %>% sapply(makeNice)
      
      segColNames2 <- list(
        dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
        c(cm, dat2$ltcCols),
        dat2$areaCols,
        dat2$socioCols,
        t,
        t2
      )
      names(segColNames2) <- c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "GlobalGroups", "Costs and Activity")
      
      fluidRow(
        if(x > 1) {
          column(width = 12,
                 p(tags$b(dat2$AnalysisDataset$join[[x - 1]]))
          )
        },
        # fluidRow(
        #   column(width = 4,
        #          column(width = 12,
        #                 selectInput(inputId = paste0("zeroAnalysisDatasetRule",x,"FieldType"),
        #                             label = "Select Field Type",
        #                             choices = c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "Costs and Activity", "GlobalGroups"),
        #                             selected = dat2$AnalysisDataset$rules[[x]][1]
        #                 )
        #          )
        #   )
        # ),
        column(width = 1,
               paste0("Rule ", x)
        ),
        column(width = 4,
               pickerInput(inputId = paste0("zeroAnalysisDataset", "Rule", x, "Field"),
                           label = "Select Field",
                           choices = segColNames2,#fieldTypeSelect(dat2$AnalysisDataset$rules[[x]][1], dat, dat2),
                           selected = s2)
        ),
        column(width = 3,
               pickerInput(inputId = paste0("zeroAnalysisDataset", "Select",x),
                           label = "Select operator for highlighting field values",
                           choices = c("=", "in numeric range", ">=", "<=", "in (multiple choices select)"),
                           selected = dat2$AnalysisDataset$rules[[x]][2]
               )
        ),
        column(width = 2,
               uiOutput(paste0("zeroAnalysisDataset", "UI",x))
        ),
        column(width = 2,
               actionButton(paste0("zeroAnalysisDataset", "Rule",x,"Delete"), "Delete Field")
        )
      )
    })
  })
}

updateAnalysisDatasetIDUISelectsFull <- function(toLoad, input, output, dat2) {
  print("Updating every select operator UI, from memory")
  lapply(1:toLoad$analysisDatasetRules, function(x) {
    renderAnalysisDatasetIDUISelectsX(x, dat, dat2, data, input, output)
  })
}

renderAnalysisDatasetIDUISelectsX <- function(x, dat, dat2, data, input, output, new = F) {
  print(paste0("Updating select ", x))
  # browser()
  operator <- dat2$AnalysisDataset$rules[[x]][2]
  output[[paste0("zeroAnalysisDatasetUI",x)]] <- renderUI({
    if(operator == "=") {
      textInput(paste0("zeroAnalysisDataset", x,"SelectI"), label = "Value", value = dat2$AnalysisDataset$rules[[x]][3])
    } else if (operator == "in numeric range") {
      column(width = 12,
             textInput(paste0("zeroAnalysisDataset", x,"SelectR1"), label = "Value", value = dat2$AnalysisDataset$rules[[x]][3]),
             p(paste0("<= ",input[[paste0("zeroAnalysisDatasetRule",x,"Field")]], " <=")),
             textInput(paste0("zeroAnalysisDataset", x,"SelectR2"), label = "Value", value = dat2$AnalysisDataset$rules[[x]][4]),
      )
    } else if (operator == ">=") {
      textInput(paste0("zeroAnalysisDataset", x,"SelectI"), label = "Value", value = dat2$AnalysisDataset$rules[[x]][3])
    } else if (operator == "<=") {
      textInput(paste0("zeroAnalysisDataset", x,"SelectI"), label = "Value", value = dat2$AnalysisDataset$rules[[x]][3])
    } else if (operator == "in (multiple choices select)") {
      print(dat2$AnalysisDataset$rules[[x]][3:length(dat2$AnalysisDataset$rules[[x]])])
      checkboxGroupInput(paste0("zeroAnalysisDataset", x,"SelectC"), label = "Value(s):", choices = 
                           if(length(unique(dat$attributes[,dat2$AnalysisDataset$rules[[x]][1]])) > 200) {
                             "Too many options to display."
                           } else {
                             sort(unique(dat$attributes[,dat2$AnalysisDataset$rules[[x]][1]]))
                           }
                         ,
                         selected = dat2$AnalysisDataset$rules[[x]][3:length(dat2$AnalysisDataset$rules[[x]])])
    }
  })
}