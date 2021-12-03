
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(shinyBS)
library(plotly)
library(treemap)
library(RColorBrewer)
library(shinyTree)
library(ggplot2)


library(shinyWidgets)
# library(sf)
# library(tigris)
# library(leaflet)
# library(tmap)
library(dplyr)
# library(gridExtra)
# library(grid)
library(scales)

library(rpart)
library(rpart.plot)

library(cluster)
library(FactoMineR)

library(data.table)
library(DT)

source("./text/landing_tab_text.R")

dat2 <- try(readRDS("./data/dat2.rds"),silent = TRUE)
dat <- try(readRDS("./data/dat.rds"),silent = TRUE)
attributes <- NULL
activity <- NULL


source("./header.R")
source("./sidebar.R")
source("./body.R")


ui <- dashboardPage(header,
                    dashboardSidebar(sidebarMenuOutput("tabs")),
                    body)


server <- function(input,output,session){
  options(shiny.maxRequestSize=100*1024^3)
  source("./functions/dataLoadCheck.R", local = TRUE)
  lm_mod <- reactiveValues()
  lm_mod$lm_obj <- NULL
  
  toLoad <- reactiveValues(
    zero3 = F,
    section1 = F,
    dep = FALSE,
    ethnicity = FALSE,
    geo = FALSE,
    widerDet = FALSE,
    segBTH = FALSE,
    cohortRules = 0,
    analysisDataset = F,
    analysisDatasetRules = 0,
    analysisDatasetRulesE = 0,
    analysisDatasetUpdate = F,
    analysisDatasetLoadDat = F,
    cohort = FALSE,
    loadTheo = FALSE
  )
  
  updateValues <- function(dat2, toLoad) {
    # Section 1 (graph summaries)
    toLoad$section1 <- TRUE
    # Clinical and activity are always loaded
    toLoad$ethnicity <- !is.na(dat2$ethnicity)
    toLoad$dep <- if(!is.na(dat2$dep)) {TRUE} else {FALSE}
    toLoad$geo <- if(!is.na(dat2$area)) {TRUE} else {FALSE}
    toLoad$widerDet <- if(!is.na(dat2$wider)) {TRUE} else {FALSE}
    # Section 2 (Segmentation)
    toLoad$segBTH <- "util.segmentation.BtHSegment"%in%colnames(dat$attributes)
    # Section 3 (Cohort Identification)
    toLoad$cohort <- TRUE
    toLoad$threeCohortRules <<- 1
    toLoad$threeCohortRulesE <<- 1
    # Section 4 (Theoplots)
    toLoad$loadTheo <- TRUE
    # Section 5 (Risk Stratification)
    toLoad
  }
  
  ##
  ## Sidebar upon loading
  output$tabs <- renderMenu({
    sidebarMenu(id="tabsData",
                menuItem("Landing Page",
                         tabName="landingPage"))
  })
  
  # Loading Page UI part 1
  output$zeroLoadingPopDataUI <- renderUI({
    landingUIPart1(dat, dat2)
  })
  
  observeEvent(input$zeroUploadPopData, {
    # Render sidebar to show file upload options
    output$tabs <- renderMenu({
          sidebarMenu(id="tabsData",
                      menuItem("Landing Page",
                               tabName="landingPage"),
                      menuItem("Initalisation (Population Dataset)",
                               tabName="initPopData",
                               menuSubItem("Data Loading",
                                           tabName="zeroData"))
          )
      })
    # Render file page (if applicable)
    
    # Jump to page
    updateTabItems(session, "tabsData", selected = "zeroData")
    # TODO: rewrite the control for this page -> need to jump back here once data is uploaded and compiled

  })
  
  # Modals for landing page
  ####################
  observeEvent(input$zeroTerminology, {
    showModal(
      modalDialog(
        title = "Dataset Terminology",
        p(strong("Q: What is a 'Population Dataset'?")),
        p("A: 'Population Dataset' refers to the linked data of the entire population of the system (e.g. an entire ICS)."),
        br(),
        p(strong("Q: What is an 'Analysis Dataset'?")),
        p("A: 'Analysis Dataset' refers to a subset of the Population Dataset. For example, if the Population Dataset covers an entire ICS, an Analysis Dataset could be an ICP."),
        p("This structure exists to allow users to focus on particular subsets of their system without having to load in new data."),
        br(),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$zeroLandingSetup, {
    showModal(
      modalDialog(
        title = "ExploreR Setup",
        p("If there is no Population Dataset loaded, click 'Upload Files'. Please refer to the handbook for data preparation and structure, then follow the steps in subsequent tabs."),
        p("If there is a Population Dataset, to load the ExploreR simply select the desired Analysis Dataset from the second box, and click 'Go!'"),
        p("Further Analysis Datasets can be added by any user. Click 'Add New Analysis Dataset' and follow the instructions on the next page."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  observeEvent(input$zeroContact, {
    showModal(
      modalDialog(
        title = "About the ExploreR",
        p("The ExploreR was developed by the BNSSG Modelling and Analytics team, during a project funded by The Health Foundation (Nov 2020-Feb 2022)."),
        p("To get in touch: ",
          a("Email the BNSSG Mdodelling and Analytics Team", href = "mailto:bnssg.analytics@nhs.net"),
          " or ",
          a("Applied Analytics at The Health Foundation", href = "mailto:Applied.Analytics@health.org.uk")
          ),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  ##############################################################
  ## Uploading data files   ####################################
  ##############################################################
  
  observeEvent(input$zeroRad, {
    output$ZeroDataUI <- renderUI({
      if (input$zeroRad == "Local Files") {
        fluidRow(
          column(width = 12,
                 br(),
                 h4("Upload Files"),
                 br()
          ),
          column(width = 6,
                 fileInput("openAttributes", "Select Attributes File", multiple = FALSE, accept = c(".rds", ".csv"))
          ),
          column(width = 6,
                 fileInput("openActivity", "Select Activity File", multiple = FALSE, accept = c(".rds", ".csv"))
          ),
          column(width = 12,
                 br(),
                 br(),
                 actionButton("loadFileData", "Go!"),
                 br()
          ))
      } else if (input$zeroRad == "SQL Query") {
        p("Feature has been removed due to security concerns.")
      }
    })
  })
  
  
  observeEvent(input$loadFileData, {
    # freezeReactiveValue(input,"loadFileData")
    # A case when using file uploads, and a case when SQL-ing the data
    output$zeroLoadMessage <- renderText({
      ext <- tools::file_ext(input$openAttributes$datapath)
      validate(
        need(ext == "rds" | ext == "csv", "Please upload a .rds or .csv Attributes file")
      )
      if (ext == "rds") {
        attributes <<- readRDS(input$openAttributes$datapath)
      } else {
        attributes <<- read.csv(input$openAttributes$datapath, check.names = F, as.is = T)
      }
      ext <- tools::file_ext(input$openActivity$datapath)
      validate(
        need(ext == "rds" | ext == "csv", "Please upload a .rds or .csv Activity file")
      )
      if (ext == "rds") {
        activity <<- readRDS(input$openActivity$datapath)
      } else {
        # browser()
        # activity <<- read.csv(input$openActivity$datapath, check.names = F) # as.POSIXct.Date()
        activity <<- read.csv(input$openActivity$datapath, check.names = F, as.is = T) #parse_datetime 
        # Need to convert dates to POSIXct format.
        print("Formatting arr_date")
        activity$arr_date <<- as.POSIXct(activity$arr_date,  format = "%d/%m/%Y %H:%M") # tryFormats = "%Y-%m-%d %H:%M:%S"
        print("Formatting dep_date")
        activity$dep_date <<- as.POSIXct(activity$dep_date,  format = "%d/%m/%Y %H:%M")
        # str( as.POSIXct(activity$arr_date[1:100], format = "%d/%m/%Y %H:%M"))
        # dmy(activity$arr_date[1:100])
        # activity$dep_date <<- as.POSIXct(activity$dep_date)
      }
      dataLoadCheck(attributes, activity, output)

      output$tabs <- renderMenu({
        sidebarMenu(id="tabsAll",
                    menuItem("Landing Page",
                             tabName="landingPage"),
                    menuItem("Initalisation (Population Dataset)",
                             tabName="initPopData",
                             menuSubItem("Data Loading",
                                         tabName="zeroData"),
                             menuSubItem("Field Assignment",
                                         tabName="zeroField"))
        )
      })
      updateTabsetPanel(session, "tabsAll", selected = "zeroField")
      toLoad$zero3 <<- TRUE
      renderThree()
      
      "Data successfully loaded."
    })
  })
  
  
  ## ######################################################################
  ## Setting field defaults
  
  observeEvent(input$aeidentifierCol, ignoreInit = TRUE, {
    updateSelectInput(session, "aeidentifierVal", choices = unique(activity[,input$aeidentifierCol]))
  })
  
  ## Setting the UI for selecting specific values of BtH segment fields
  lapply(c("twoBTHFrailty", "twoBTHLimitedReserve", "twoBTHDecline","twoBTHDisability","twoBTHChronic", "twoBTHMaternal", "twoBTHAcute"), function(x) {
    observeEvent(input[[x]], ignoreInit = TRUE, {
      output[[paste0(x, "UI")]] <- renderUI({
        lapply(if(!is.null(input[[x]])) {
          if(input[[x]] == "") {NULL} else {input[[x]]}
        } else {
          input[[x]]
        }, function(y) {
          fluidRow(
            column(width = 6,
                   selectInput(inputId = paste0(x,y,"Select"),
                               label = paste0("Select operator for highlighting values for ",makeNice(y)),
                               choices = c("=", "in numeric range", ">=", "<=", "in (multiple choices select)"))
            ),
            column(width = 6,
                   uiOutput(paste0(x,y,"SelectUI"))
            )
          )
        })
      })
    })
    observeEvent(input[[x]], {
      lapply(if(!is.null(input[[x]])) {
        if(input[[x]] == "") {NULL} else {input[[x]]}
      } else {
        input[[x]]
      }, function(y) {
        observeEvent(input[[paste0(x,y,"Select")]], {
          output[[paste0(x,y,"SelectUI")]] <- renderUI({
            ## case when each of the operators
            if(input[[paste0(x,y,"Select")]] == "=") {
              textInput(paste0(x,y,"SelectI"), label = "Value", value = 1)
            } else if (input[[paste0(x,y,"Select")]] == "in numeric range") {
              column(width = 12,
                     textInput(paste0(x,y,"SelectR1"), label = "Value", value = 1),
                     p(paste0("<= ",y, " <=")),
                     textInput(paste0(x,y,"SelectR2"), label = "Value", value = 1),
              )
            } else if (input[[paste0(x,y,"Select")]] == ">=") {
              textInput(paste0(x,y,"SelectI"), label = "Value", value = 1)
            } else if (input[[paste0(x,y,"Select")]] == "<=") {
              textInput(paste0(x,y,"SelectI"), label = "Value", value = 1)
            } else if (input[[paste0(x,y,"Select")]] == "in (multiple choices select)") {
              checkboxGroupInput(paste0(x,y,"SelectC"), label = "Value(s):", choices = unique(attributes[,y]))
            }
          })
        })
      })
    })
  })
  
  ## End of BtH select values

  
  observeEvent(input$zeroThreeProceed,{

    ######################################################################################################################################################
    # # Error testing functions, removed to save RAM
    # replace_na_0 <- function(values) {
    #   print("Replacing 0")
    #   gc()
    #   sapply(values, function(x) {
    #     if (is.na(x)) {0} else {x}
    #   }) %>% unlist()
    # }
    # replace_na_unknown <- function(values) {
    #   print("Replacing na")
    #   gc()
    #   sapply(values, function(x) {
    #     if (is.na(x)) {"Unknown"} else {x}
    #   }) %>% unlist()
    # }
    # replace_na_nl <- function(values) {
    #   print("Replacing nl")
    #   gc()
    #   sapply(values, function(x) {
    #     
    #     if (is.na(x)) {"No Further Levels"} else {x}
    #   }) %>% unlist()
    # }
    # attributes <<- attributes %>% mutate(across(starts_with("clinic.") & !starts_with("clinic.misc"), replace_na_0))
    # 
    # activity <<- activity %>% mutate(cost = replace_na_0(cost))
    # gc()
    # activity <<- activity %>% mutate(spec = replace_na_unknown(spec))
    # gc()
    # # Benchmarking notes: 12GB ram handled ~74 million records (act), x columns by 1,000,000 records
    # # Got this far, then failed when allocating memory
    # activity <<- activity %>% mutate(across(starts_with("pod"), replace_na_nl))
    #######################################################################################################################################################

    
    gc()
    # Save down the theodata - any activity beyond 1 year of historic
    # Do this to save RAM in subsequent operations, as the size of activity can be reduced
    # First, test for old theo data. If exists, remove
    old_dat2 <- try(readRDS("./data/dat2.rds"),silent = TRUE)
    if(class(old_dat2) != "try-error") {
      nums <- unique(old_dat2$actLookUp[,2])
      for(i in nums) {
        if(i > 0) {
          print(i)
          file.remove(paste0("./data/theoAct/", i, "act_part.rds"))
        }
      }
    }

    # Cut activity and save down in chunks
    # want no more than 10000 in each group
    mintime <- min(activity$arr_date, na.rm = T)
    maxtime <- max(activity$dep_date, na.rm = T)
    
    maxDate <- max(activity$dep_date, na.rm = T)
    maxDate2 <- ymd(maxDate) - years(1)
    if(is.na(maxDate2)) {
      maxDate2 <- as.POSIXlt(as.Date(maxDate))
      maxDate2$year <- maxDate2$year-1
      maxDate2 <- as.Date(maxDate2)
    }
    # browser()
    saveRDS(activity %>% filter(.data[["arr_date"]] >= maxDate2), paste0("./data/act_.rds")) # this will be the activity file needed for dep()

    ids <- unique(activity$id)
    gn <- ceiling(length(ids) / 10000) # is how many groups we want
    df <- lapply(1:gn, function(x) {
      c <- ids[((x-1)*10000 + 1):(x*10000)]
      c <- which(activity$id %in% c)
      c <- activity[c,]
      gc()
      saveRDS(c,paste0("./data/theoAct/", x, "act_part.rds"))
      # return the part of the table for lookup
      data.frame(id = ids[((x-1)*10000 + 1):(x*10000)], lookUp = x) # need to cut NAs off
    })
    gc()
    # browser()
    df <- do.call(rbind, df)
    df <- df[!is.na(df$id),]
    print("Saved files")
    #  dat2$actLookUp <- rbind(df,data.frame(id = which(!dat$attributes$id%in%ids), lookUp = -1))
    actLookUp <- rbind(df,data.frame(id = which(!attributes$id%in%ids), lookUp = rep(-1, length(which(!attributes$id%in%ids)))))
    # browser()
    ####
    rm(activity, envir = as.environment(".GlobalEnv"))
    rm(activity)
    gc()
    activity <<- readRDS("./data/act_.rds")
    gc()
    source("./functions/act_format.R")
    source("./functions/att_format.R")
    # browser()
    # TODO crash if either simple of complex LTCs are empty?
    attributes <- att(attributes, activity, input, output)
    dat <<- left_join(attributes,
                      activity %>% filter(.data[["arr_date"]] >= maxDate2) %>% group_by(id) %>%
                        summarise(total_cost = sum(cost),
                                  total_act = n()) %>%
                        ungroup()
                      # process_act(activity)
    ) %>% list(attributes = ., activity = activity)
    rm(attributes)
    rm(activity)
    # rm(list=ls())
    # globalEnv()
    # ".GlobalEnv"
    gc()
    # browser()
    # reminder that if the tables are joined, there will be NAs where some patients don't have activity - this needs to be changed to 0
    dat$attributes$total_cost <- lapply(dat$attributes$total_cost%>% unlist(), function(x) {if (is.na(x)) {0} else {x}}) %>% unlist()
    dat$attributes$total_act <- lapply(dat$attributes$total_act%>% unlist(), function(x) {if (is.na(x)) {0} else {x}}) %>% unlist()
    dat2 <<- dep(input, dat)
    dat2$mintime <<- mintime
    dat2$maxtime <<- maxtime
    dat2$actLookUp <<- actLookUp
    # browser()
    dat <<- dat[-2]
    saveRDS(dat ,"./data/dat.rds")
    saveRDS(dat2 ,"./data/dat2.rds")
    

    output$tabs <- renderMenu({
      sidebarMenu(id="tabsData",
                  menuItem("Landing Page",
                           tabName="landingPage"))
    })
    output$zeroLoadingPopDataUI <- renderUI({
      landingUIPart1(dat, dat2)
    })
    output$zeroLoadingAnalysisDataUI <- renderUI({
      landingUIPart2(dat, dat2)
    })
    updateTabItems(session, "tabsData", selected = "landingPage")

  })
  ############################################################
  ############################################################
  
  # Loading Page UI part 2
  output$zeroLoadingAnalysisDataUI <- renderUI({
    landingUIPart2(dat, dat2)
  })
  
  observeEvent(input$zeroAnalysisDataPicker, {
    # browser()
    print("Analysis dataset text")
    # TODO need to cache activity count in a persistent way - problem when subset is selected
    output$zeroAnalysisDataText <- renderText(
      paste0(
        "The Analysis Dataset '", makeNice(input$zeroAnalysisDataPicker),  
        "' covers ", format(length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]])), big.mark = ","), " individuals. ",
        # format(sum(dat$attributes$total_act[dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]]]), big.mark = ",")
        format(dat2$AnalysisDatasetsStats[,input$zeroAnalysisDataPicker], big.mark = ",")
        , " points of activity are associated with this dataset."
      )
    )
  })
  observeEvent(input$zeroAnalysisDataPicker, once = T, {
    showModal(
      modalDialog(
        title = "User Agreement",
        p("By using the PHM ExploreR you agree to use the tool responsibly, as per your duty of care. You also agree to use the ExploreR to answer considered questions, as opposed to ",
        "'fishing'. In addition, you have a duty to maintain confidentiality, as patient-level data is present in the tool."),
        p("Subsequently, as we cannot physically stop you from taking screenshots or images from the tool, in the event you do take some to share with collegues, it is your duty to ",
          "ensure confidentiality is maintained."),
        "If you have any concerns about being able to responsibly use this tool, close the window.",
        footer = list(modalButton("I accept")),
        easyClose = F
      )
    )
  })
  
  ## Add analysis dataset
  observeEvent(input$zeroAnalysisDataPickerAdd, {
    showModal(
       modalDialog(
        title = "Please wait while loading.",
        footer = list(),
        easyClose = F
      )
    )
    # Render sidebar to show file upload options
    output$tabs <- renderMenu({
      sidebarMenu(id="tabsData",
                  menuItem("Landing Page",
                           tabName="landingPage"),
                  menuItem("Analysis Dataset Identification",
                           tabName="analysisData")
      )
    })
    dat <<- readRDS("./data/dat.rds")
    # Render file page (if applicable)
    toLoad$analysisDatasetRules <<- 1
    toLoad$analysisDatasetRulesE <<- 1
    toLoad$analysisDataset <<- T

    dat2$AnalysisDataset <<- list()
    dat2$AnalysisDataset$rules <<- list()
    # TODO fix to make sure options are NOT infinitely many
    dat2$AnalysisDataset$rules[[1]] <<- c(dat2$cohortDefaultCols[1], "in (multiple choices select)",  dat2$cohortDefaultVals[!is.na(dat2$cohortDefaultVals)])
    dat2$AnalysisDataset$join <<- list()
    updateAnalysisDatasetIDUI(toLoad, input, output, dat2, dat)
    # Jump to page
    updateTabItems(session, "tabsData", selected = "analysisData")
    removeModal()
  })
  
  ###########################################
  ### Analysis Dataset    ###################
  ###########################################
  # Analysis dataset modals
  
  observeEvent(input$zeroAnalysis1, {
    showModal( modalDialog(
      title = "What is an Analysis Dataset?",
      p("Analysis Dataset is a subset of the Population Dataset. For example, if the Population Dataset covers an entire ICS, an Analysis Dataset could be an ICP."),
      p("This structure exists to allow users to focus on particular subsets of their system without having to load in new data - e.g. only looking at a particular ICS."),
      p("Analysis Datasets created on this page are saved for future use (they will be present the next time the ExploreR is started)."),

      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
  
  observeEvent(input$zeroAnalysis2, {
    showModal( modalDialog(
      title = "Creating an Analysis Dataset",
      
      p("To create an Analysis Dataset, follow these steps:"),
      
      p("1. Determine what criteria defines the Dataset."),
      p("2. Split this criteria into simple statements, such as 'age > 18', and 'has hypertension'"),
      
      p("3. For each of these statements, create a new line (using button 'Add new field'). Note these clauses are joined either by OR/AND. 
        This can be changed by the dropdown to the left of the button. Clauses are evaluated top to bottom. Both operators have the same priority."),
      p("4. For each clause, select the appropriate field name, then the values to include in the Dataset. There are multiple ways of selecting these values. ",
        "These options can be selected from the dropdown to the right of the field. Details of the operators are at the bottom of this popup."),
      p("5. Click 'Get Analysis Dataset' to select individuals defined by the criteria. Then, return to Landing Page, select the newly created Analysis Dataset, and click 'Go!'"),
      br(),
      hr(),
      p(strong("Operators")),
      p("'=' - equals to a specific value"),
      p("'in numeric range' - values of selected field lie between 2 input values. Suitable for numeric data. Uses <= and >="),
      p("'>=' and '<=' - lower/upper limit, respectively"),
      p("'in (multiple choice select)' - multiple values to include. Recommended for non-numeric options. Useful for seeing distinct values of the selected field. Cannot be used if more than 200 unique values are present."),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
  observeEvent(input$zeroAnalysis3, {
    showModal( modalDialog(
      title = "Uploading IDs",
      p("If there is a pre-identified group of interest, IDs can be uploaded via a .csv file."),
      p("This file should contain only a single column of IDs, with no row or column names."),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
  
  
  # Add new line option
  observeEvent({input$zeroAnalysisDataNewUI}, {
    print("Adding new line")
    
    l <- length(dat2$AnalysisDataset$rules)
    freezeReactiveValue(input,"zeroAnalysisDataNewUI")
    freezeReactiveValue(input, paste0("zeroAnalysisDatasetRule",l+1,"Delete"))

    dat2$AnalysisDataset$rules[[l+1]] <<- c(dat2$cohortDefaultCols[1], 
      "in (multiple choices select)",
      dat2$cohortDefaultVals)
    
    dat2$AnalysisDataset$join[[l]] <<- isolate(input$zeroAnalysisDataUIandor)
    
    toLoad$analysisDatasetRules <<- toLoad$analysisDatasetRules + 1
    if(toLoad$analysisDatasetRules > toLoad$analysisDatasetRulesE) {
      toLoad$analysisDatasetRulesE <<- toLoad$analysisDatasetRules
    }
    updateAnalysisDatasetIDUI(toLoad, input, output, dat2, dat)
    
    updateAnalysisDatasetIDUISelectsFull(toLoad, input, output, dat2)

    print("Added new line")

  })

  # need to move all data assignments to c()
  observeEvent(toLoad$analysisDatasetRulesE, {
    lapply(toLoad$analysisDatasetRulesE, function(x) {
      observeEvent(input[[paste0("zeroAnalysisDatasetSelect",x)]], ignoreInit = T, {
        print(paste0("Update of select uis (operator) + memory ", x))
        dat2$AnalysisDataset$rules[[x]][2] <<- isolate(input[[paste0("zeroAnalysisDatasetSelect",x)]])
        renderAnalysisDatasetIDUISelectsX(x, dat, dat2, data, input, output)
      })
      # need error checking to make sure we can't enter a null value
      observeEvent(input[[paste0("zeroAnalysisDatasetRule",x,"Field")]], ignoreInit = T, {
        print(paste0("Adding field to memory ", x))
        # browser()
        if(dat2$AnalysisDataset$rules[[x]][1] != input[[paste0("zeroAnalysisDatasetRule",x,"Field")]]) {
          dat2$AnalysisDataset$rules[[x]][1] <<- input[[paste0("zeroAnalysisDatasetRule",x,"Field")]]
          # reset accepted values
          dat2$AnalysisDataset$rules[[x]] <<- c(dat2$AnalysisDataset$rules[[x]][1:2], NULL)
        }
        # Update the available options for this
        renderAnalysisDatasetIDUISelectsX(x, dat, dat2, data, input, output, new = T)
      })
      
      # Wipe memory of selected options - if many from multiple select
      observeEvent(input[[paste0("zeroAnalysisDataset", x,"SelectI")]], ignoreInit = T, {
        print("Adding selected value to memory1")
        dat2$AnalysisDataset$rules[[x]] <<- isolate(c(dat2$AnalysisDataset$rules[[x]][1:2], input[[paste0("zeroAnalysisDataset", x,"SelectI")]]))
      })
      
      observeEvent(input[[paste0("zeroAnalysisDataset", x,"SelectR1")]], ignoreInit = T, {
        print("Adding selected value to memory2")
        dat2$AnalysisDataset$rules[[x]] <<- isolate(c(dat2$AnalysisDataset$rules[[x]][1:2], input[[paste0("zeroAnalysisDataset", x,"SelectR1")]]))
      })
      
      observeEvent(input[[paste0("zeroAnalysisDataset", x,"SelectR2")]], ignoreInit = T, {
        print("Adding selected value to memory3")
        dat2$AnalysisDataset$rules[[x]] <<- isolate(c(dat2$AnalysisDataset$rules[[x]][1:3], input[[paste0("zeroAnalysisDataset", x,"SelectR2")]]))
      })
      
      observeEvent(input[[paste0("zeroAnalysisDataset", x,"SelectC")]], ignoreInit = T, {
        print(paste0("Adding selected value to memory4 ", x))
        dat2$AnalysisDataset$rules[[x]] <<- c(dat2$AnalysisDataset$rules[[x]][1:2],
                                              isolate(input[[paste0("zeroAnalysisDataset", x,"SelectC")]])
                                              )
      })
      
      observeEvent(input[[paste0("zeroAnalysisDatasetRule",x,"Delete")]], ignoreInit = T, {
        if(length(dat2$AnalysisDataset$rules) > 1) {
          print("Deleting a line")
          dat2$AnalysisDataset$rules <<- dat2$AnalysisDataset$rules[-x]
          if(length(dat2$AnalysisDataset$join) < 2) {
            dat2$AnalysisDataset$join <<- list()
          } else {
            dat2$AnalysisDataset$join <<- dat2$AnalysisDataset$join[-min(x, length(dat2$AnalysisDataset$join))]
          }
          toLoad$analysisDatasetRules <<- toLoad$analysisDatasetRules - 1
          updateAnalysisDatasetIDUI(toLoad, input, output, dat2, dat)
          freezeReactiveValue(input, paste0("zeroAnalysisDatasetSelect",x))
          freezeReactiveValue(input, paste0("zeroAnalysisDatasetRule",x,"Delete"))
          # freezeReactiveValue(input, paste0("zeroAnalysisDatasetRule",x,"FieldType"))
        } else {
          output$zeroAnalysisDatasetGetIDsMessage <- renderText("Unable to delete, only 1 line remaining")
          freezeReactiveValue(input, paste0("zeroAnalysisDatasetRule",x,"Delete"))
        }
      })
    })
  })
  
  observeEvent(input$zeroAnalysisDatasetGetIDs,{
    freezeReactiveValue(input, "zeroAnalysisDatasetGetIDs")
    showModal( modalDialog(
      title = "Please wait while processing.",
      footer = tagList(),
      easyClose = F
    ))
    print("Attempting to get analysis dataset")
    print(dat2$AnalysisDataset)
    if(length(which(sapply(dat2$AnalysisDataset$rules, function(x) {
      !""%in%x[3:length(x)]
    }))) < length(dat2$AnalysisDataset$rules)) {
      output$zeroAnalysisDatasetGetIDsMessage <- renderText(paste0("Missing values found."))
    } else if(length(which(sapply(dat2$AnalysisDataset$rules, length) > 2)) == length(dat2$AnalysisDataset$rules)) {
      # Only get cohort if we can feasibly do so
      analysisSet <- getAnalysisDataset(input,dat,dat2$AnalysisDataset)
      if(length(which(analysisSet)) > 20) {

        dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]] <<- analysisSet
        print("Got data")

        output$zeroAnalysisDatasetGetIDsMessage <- renderText(paste0("Successfully identified ", 
                                                                     isolate(length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]]))),
                                                                     " individuals in dataset"))
        # Send user back to starting page, set Analysis box to option just created
        
        dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetName]] <<- sum(dat$attributes$total_act[dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]]])
        
        updatePickerInput(session, "zeroAnalysisDataPicker", choices = colnames(dat2$AnalysisDatasets), selected = input$zeroAnalysisDatasetName)
        updatePickerInput(session, "zeroAnalysisDatasetToRemove", choices = colnames(dat2$AnalysisDatasets)[-1], selected = input$zeroAnalysisDatasetToRemove)
        # browser()
        saveRDS(dat2, "./data/dat2.rds")
        output$zeroAnalysisDataText <- renderText(
          paste0(
            "The dataset '", makeNice(input$zeroAnalysisDataPicker),  
            "' covers ", length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]])), " individuals. ",
            format(dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetName]],big.mark = ","),
            " points of activity are associated with this dataset."
          )
        )
        removeModal()
      } else {
        output$zeroAnalysisDatasetGetIDsMessage <- renderText("Fewer than 20 individuals present, terminating assignment. Please create a dataset with more individuals.")
      }
    } else {
      output$zeroAnalysisDatasetGetIDsMessage <- renderText(paste0("Missing values found."))
    }
  })
  
  observeEvent(input$zeroAnalysisDatasetBack, {
    updateTabItems(session, "tabsData", selected = "landingPage")
  })
  
  getAnalysisDataset <- function(input,dat,fullRuleset) {
    # browser()
    data <- lapply(1:length(fullRuleset$rules), function(y) {
      x <- fullRuleset$rules[[y]]
      data <- dat$attributes[,x[1]] %>% as.vector()
      if(x[2] == "=") {
        data <- case_when(data == x[3] ~ 1,
                          TRUE ~ 0)
      } else if (x[2] == "in numeric range") {
        data <- case_when(data >= as.numeric(x[3]) & data <= as.numeric(x[4]) ~ 1,
                          TRUE ~ 0)
      } else if (x[2] == ">=") {
        data <- case_when(data >= as.numeric(x[3]) ~ 1,
                          TRUE ~ 0)
      } else if (x[2] == "<=") {
        data <- case_when(data <= as.numeric(x[3]) ~ 1,
                          TRUE ~ 0)
      } else if (x[2] == "in (multiple choices select)") {
        data <- case_when(data %in% x[3:length(x)] ~ 1,
                          TRUE ~ 0)
      }
    })
    # Order of OR/AND is linear: starting from the front, take each pair and use result in next calculation
    if(!is_empty(fullRuleset$join)) {
      for(i in 1:length(fullRuleset$join)){
        if(fullRuleset$join[[i]] == "AND") {
          # only make changes if AND - i == AND is between i and i+1th rule
          data[[i+1]] <- sapply(data[[i]]+data[[i+1]], function(z) {
            if(z == 2) {1} else {0}
          })
          data[[i]] <- rep(0, length(data[[i+1]]))
        } else {
          # OR line
          data[[i+1]] <- sapply(data[[i]]+data[[i+1]], function(z) {
            if(z >= 1) {1} else {0}
          })
          data[[i]] <- rep(0, length(data[[i+1]]))
        }
      }
    }
    data <- data.frame(data) %>% rowSums() %>% sapply(function(x) {
      if(x > 0) {T} else {F} %>% unlist()
    })
    # browser()
    return(data)
  }
  
  observeEvent(input$zeroAnalysisDataUploadIdsButton, {
    # browser()
    ext <- tools::file_ext(input$zeroAnalysisDataUploadIds$datapath)
    if (ext == "csv") {
      dat2$cohortIDs <<- read.csv(input$zeroAnalysisDataUploadIds$datapath, stringsAsFactors = F, header = F)
      if(ncol(dat2$cohortIDs) == 1 && nrow(dat2$cohortIDs) > 0) {
        showModal( modalDialog(
          title = "Please wait while processing.",
          footer = tagList(),
          easyClose = F
        ))
        # Correct format
        dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]] <<- dat$attributes$id %in% dat2$cohortIDs[[1]]
        output$zeroAnalysisDatasetGetIDsMessage <- renderText(paste0("Successfully identified ", 
                                                                  isolate(length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]]))),
                                                          " individuals in dataset."))
        dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetName]] <<- sum(dat$attributes$total_act[dat2$AnalysisDatasets[[input$zeroAnalysisDatasetName]]])
        
        updatePickerInput(session, "zeroAnalysisDataPicker", choices = colnames(dat2$AnalysisDatasets), selected = input$zeroAnalysisDatasetName)
        updatePickerInput(session, "zeroAnalysisDatasetToRemove", choices = colnames(dat2$AnalysisDatasets)[-1], selected = input$zeroAnalysisDatasetToRemove)
        saveRDS(dat2, "./data/dat2.rds")
        output$zeroAnalysisDataText <- renderText(
          paste0(
            "The dataset '", makeNice(input$zeroAnalysisDataPicker),  
            "' covers ", length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]])), " individuals. ",
            format(dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetName]],big.mark = ","),
            " points of activity are associated with this dataset."
          )
        )
        removeModal()
      }
    }
  })
  
  observeEvent(input$zeroAnalysisDatasetRemove, {
    
    name = input$zeroAnalysisDatasetToRemove
    
    showModal( modalDialog(
      title = paste0("Removing dataset", name),
      p("Please wait while processing."),
      footer = tagList(),
      easyClose = F
    ))
    
    output$zeroAnalysisDatasetRemoveMessage <- renderText({
      paste("Removed Analysis Dataset ", name)
    })

    dat2$AnalysisDatasets[[input$zeroAnalysisDatasetToRemove]] <<- NULL
    dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetToRemove]] <<- NULL

    updatePickerInput(session, "zeroAnalysisDataPicker", choices = colnames(dat2$AnalysisDatasets))
    updatePickerInput(session, "zeroAnalysisDatasetToRemove", choices = colnames(dat2$AnalysisDatasets)[-1])

    saveRDS(dat2, "./data/dat2.rds")
    
    output$zeroAnalysisDataText <- renderText(
      paste0(
        "The dataset '", makeNice(input$zeroAnalysisDataPicker),  
        "' covers ", length(which(dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]])), " individuals. ",
        format(dat2$AnalysisDatasetsStats[[input$zeroAnalysisDatasetName]],big.mark = ","),
        " points of activity are associated with this dataset."
      )
    )
    removeModal()
  })
  
  ###########################################
  ###########################################
  ###########################################
  
  ## Load main part
  observeEvent(input$zeroLoadingMainGo, {
    # sidebar
    showModal(
      modalDialog(
        title = "Please wait while loading.",
        footer = list(),
        easyClose = F
      )
    )
    # Render sidebar
    output$tabs <- renderMenu({
      sidebar
    })
    # browser()
    if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
      output$twoSegOutput <- renderMenu({
        menuItem("Segmentation",
                 tabName="twoTab",
                 menuSubItem("LTC by Age",
                             tabName="twoLTCAge"),
                 menuSubItem("Bridges to Health",
                             tabName="twoBTH"),
                 menuSubItem("Decision Trees",
                             tabName="twoCART"),
                 menuSubItem("K-Means Clustering",
                             tabName="twoClusters")
        )
      })
    } else {
      output$twoSegOutput <- renderMenu({
        menuItem("Segmentation",
                 tabName="twoTab",
                 menuSubItem("LTC by Age",
                             tabName="twoLTCAge"),
                 menuSubItem("Decision Trees",
                             tabName="twoCART"),
                 menuSubItem("K-Means Clustering",
                             tabName="twoClusters")
        )
      })
    }
    
    if(nrow(dat$attributes) < nrow(dat2$AnalysisDatasets)) {
      print("Re-reading attributes data")
      dat <<- readRDS("./data/dat.rds")
    }
    gc()
    dat$attributes <<- dat$attributes[dat2$AnalysisDatasets[[input$zeroAnalysisDataPicker]],]
    gc()
    # Render file page (if applicable)

    # Jump to page - for now, to cohort page
    updateTabItems(session, "tabsAll", selected = "navTab")
    # NOTE: loading of the rest of the tool needs to be moved here
    toLoad$section1 <<- F
    toLoad$loadTheo <<- F
    toLoad$segBTH <<- F
    toLoad$loadTheo <<- F
    toLoad$threeCohort <<- F
    
    toLoad <- updateValues(dat2, toLoad)

    
  })
  
  
##############################################
  ##
  ## Tools Main Section
  ##
  ############################################
  ## Section 1: General Summary
  #
  # Begin by loading each page according to toLoad's TRUE/FALSE values
  #
  
  source("./functions/loadExploreR.R", local = TRUE, encoding = "utf-8")
  source("./functions/section1Tools.R", local = TRUE, encoding = "utf-8")
  
  observeEvent(toLoad$section1, {
    # browser()
    if(toLoad$section1) {
      print("Loading")
      # toLoad <- updateValues(dat2, toLoad)
      # browser()
      try(
        loadExploreR(data, dat, input, output)
      )
      removeModal()
    }
    # browser()
  })
  
  observeEvent(input$threeCohort, {
    updatethreeCohortIDUI(toLoad, input, output, dat2, dat)
  })
  
  
  observeEvent(input$navTabJCohort, {
    updateTabItems(session, "tabsAll", selected = "threeID")
  })
  observeEvent(input$navTabJSG, {
    updateTabItems(session, "tabsAll", selected = "oneTab")
  })
  observeEvent(input$navTabJSeg, {
    updateTabItems(session, "tabsAll", selected = "twoLTCAge")
  })
  observeEvent(input$navTabJTheo, {
    updateTabItems(session, "tabsAll", selected = "fourTab")
  })
  observeEvent(input$navTabJRiskStrat, {
    updateTabItems(session, "tabsAll", selected = "fiveTab")
  })
  observeEvent(input$navTabGlobalVarAdd, {
    if(input$navTabGlobalVar %in% dat2$groupByList) {
      output$navTabMessage <- renderText(paste0("Already in GlobalGroups."))
    } else {
      if(length(unique(dat$attributes[[input$navTabGlobalVar]])) < 20) {
        i <- length(dat2$groupByList) + 1
        dat2$groupByListText[[i]] <<- unname(makeNice(input$navTabGlobalVar))
        dat2$groupByList[[i]] <<- unname(input$navTabGlobalVar)
        updateAllWithNewImportantField(dat)
        output$navTabMessage <- renderText(paste0("Added ", makeNice(isolate(input$navTabGlobalVar)), " to GlobalGroups."))
      } else {
        output$navTabMessage <- renderText(paste0("Could not add ", makeNice(isolate(input$navTabGlobalVar)), " as variable since it has too many unique values."))
      }
      
    }
  })
  observeEvent(input$navTabGlobalVarRemove, {
    # browser()
    if(input$navTabCurrentGlobalVar %in% dat2$groupByList) {
      i <- which(dat2$groupByList == input$navTabCurrentGlobalVar)
      dat2$groupByListText <<- dat2$groupByListText[-i]
      dat2$groupByList <<- dat2$groupByList[-i]
      updateAllWithNewImportantField(dat)
      output$navTabMessage <- renderText(paste0("Removed ", makeNice(isolate(input$navTabCurrentGlobalVar))))
    } else {
      output$navTabMessage <- renderText(paste0("Could not remove ", makeNice(isolate(input$navTabCurrentGlobalVar)), " as variable not found in GlobalGroups."))
    }
  })
  observeEvent(input$navTabGlobal,{
    showModal( modalDialog(
      title = "GlobalGroups Info",
      p("A selection of fields are available on every page in the ExploreR to allow data interrogation. 'GlobalGroups' is the term used to refer them (on account of being 'globally' available. )"),
      p("The option on this page exists to allow users to customise the fields available.",
        "To add a field, select it from the dropdown, then click 'Add'. Similarly, fields can also be removed."),
      p("A field may only be added if it is not already in GlobalGroups. In addition, ",
      "please note only fields with fewer than 11 unique elements may be added."),
      p("WARNING: please minimise removal of default options to minimise risk of crashing."),
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
 
  
  ############################################################################################################################################
  # 1.1 Demog Tab
  
  observeEvent({input$oneDemogCountPercentage
    input$oneDemogStackedSeparate
    input$oneDemogGroupBy
    input$oneDemogClinNeed}, {
      output$oneDemogPlot1 <- renderPlotly({

        group <- input$oneDemogGroupBy %>% groupLookUp(dat2, .)
        print("Demog, main")
        data <- if(is.na(group)) {
          dat$attributes %>% select_at(c(dat2$age,
                                         dat2$sex))
        } else {
          dat$attributes %>% select_at(c(dat2$age,
                                         dat2$sex,group))
        }
        if(is.na(group)) {
          colnames(data) <- c("demog.age", "demog.sex")
        } else {
          colnames(data) <- c("demog.age", "demog.sex", group)
        }
        if (input$oneDemogClinNeed != "") {
          data <- data %>% cbind(dat$attributes %>% select_at(input$oneDemogClinNeed))
        }
        data$demog.age <- age_groups(data$demog.age, (0:floor(max(data$demog.age)/5))*5)
        source("./functions/poppyramid.R")
        poppyramid(data, group, input$oneDemogClinNeed, input$oneDemogCountPercentage, input$oneDemogStackedSeparate, input)
      })
    })
  
  
  ############################################################################################################################################
  # 1.2 Multimorbidity and population by conditions

  observeEvent({input$oneClinY
    input$oneClinGroupBy}, {
      if(toLoad$section1) {
        data <- dat$attributes$util.ltc_sum %>% as.data.frame()

        p <- try({clinicPlotSolve(dat, data, input$oneClinY, groupLookUp(dat2, input$oneClinGroupBy),
                                  guideText = input$oneClinGroupBy)}, silent = TRUE)
        if(class(p) == "try-error") {print(p)} else {
          output$oneClinPlot1 <- renderPlotly({p})
        }
      }
  })

  output$graph1 <- renderPlotly({
    values <- dat$attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% select(!starts_with("clinic.any")) %>% 
      colSums()
    values <- values / nrow(dat$attributes) * 100
    data <- data.frame(ltc = colnames(dat$attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% select(!starts_with("clinic.any"))),# %>% makeNice(),
               percentage = values)
    p <- data[order(data$percentage, decreasing = FALSE),] %>%
      # mutate(ltc = makeNice(ltc)) %>%
      mutate(ltc = factor(ltc, levels = ltc)) %>%
    ggplot(aes(x=ltc, y=percentage)) + 
      geom_bar(stat = "identity") + ggtitle("  ") +
      xlab("LTC") + ylab("% of population")  + 
      coord_flip()
    ggplotly(p)
  })
  
  
  
  #################################################################################################################################################
  # 1.3 Spending by POD type
  
  observeEvent({input$oneActCoA
    input$oneActVertical
    input$oneActX}, {
      if(toLoad$section1) {
        selectedGroup <- groupLookUp(dat2, input$oneActVertical)
        try({
          if(!is.na(selectedGroup)) {
            d <- getActGraphData(dat, dat2, selectedGroup)
            d[[selectedGroup]] <- factor(d[[selectedGroup]] )
            try({ p <- #
              ggplotly(ggplot(
              d,
              aes_string(x="pod_l1", y=selectedGroup, size= "cost", fill = "cost", color = "cost", text = "text")) +
                geom_point(shape=21) + 
                ggtitle("Bubbleplot of Points of Delivery") +
                xlab("Point of Delivery (POD)") + 
                ylab(input$oneActVertical)  + labs(color=input$oneActCoA) + # %in%input$oneActX
                guides(fill = FALSE) +
                scale_colour_continuous(labels=scales::label_number_si()) +
                scale_fill_continuous(labels=scales::label_number_si()) +
                theme(axis.text.x = element_text(angle = 45, hjust=1)),
              tooltip = "text")
              })
            output$oneActivityPlot2 <- renderPlotly({
              p
            })
            # output$oneActTable1 <- renderUI({})
          }
        })
      }
    })
  
  
  # 1.4 Deprivation
  
  observeEvent({input$oneDeprivationY
    input$oneDeprivationGroupBy}, {
      if(toLoad$section1 & toLoad$dep) {
        data <- dat$attributes %>% select_at(dat2$dep) %>% as.data.frame()
        output$oneDeprivationPlot1 <- renderPlotly({clinicPlotSolve(dat, data, input$oneDeprivationY, groupLookUp(dat2, input$oneDeprivationGroupBy), 
                                                                    xtitle = makeNice(paste0(dat2$dep," (1 most deprived, 10 least)")),
                                                                    title = "Deprivation"
                                                                    ,
                                                                    guideText = input$oneDeprivationGroupBy)%>%
            layout(barmode="overlay",
                   xaxis = list(tickangle = -45))
          })
        }
    })
#   observeEvent({input$oneDeprivationY
#     input$oneDeprivationGroupBy}, {
#       if(toLoad$section1 & toLoad$dep) {
#         try({
#         if(.Platform$OS.type == "windows"){
#           sf <- st_read("S:/Finance/Shared Area/BNSSG - BI/6 References/14 GIS/Layer Files/BNSSG_LSOA_Boundary_simple_10m.shp") 
#           Local_Authority_Boundary <- st_read("S:/Finance/Shared Area/BNSSG - BI/6 References/14 GIS/rshiny_simplified/la_boundary_100.shp") 
#           PCN_Boundary <- st_read("S:/Finance/Shared Area/BNSSG - BI/6 References/14 GIS/rshiny_simplified/pcn_boundary_100.shp") 
#           ##
#           # grab my data and format to the lsoas here
#           data <- dat$attributes %>% select_at(c(dat2$dep, "area.Lower Super Output Area (LSOA)")) %>% as.data.frame() %>%
#           mutate(lsoa = toupper(`area.Lower Super Output Area (LSOA)`),
#                  patients = socio.wd_imd_decile_19)
#           
#           
#           output$mapPlot <- renderLeaflet({
#             
#             
#             LSOA <- geo_join(sf, data,'lsoa11cd',"lsoa" , by = NULL, how = "left")
#             LSOA_Layer = st_as_sf(LSOA)
#             
#             tm <- tm_shape(LSOA_Layer) +
#               tm_polygons(style="pretty",
#                           col= "patients", 
#                           id= "lsoa11nmw",#"msoa11hclnm",
#                           border.col = "white",
#                           border.alpha = .4,
#                           popup.vars=c("lsoa11cd","patients"),
#                           alpha = 0.6,
#                           colorNA = NULL,
#                           legend.hist = TRUE) +
#               tm_shape(PCN_Boundary,
#                        name = "PCN Boundaries") +
#               tm_fill("layer",
#                       alpha = 0,
#                       legend.show = FALSE,
#                       id = "layer") +
#               tm_borders(col=NA,lwd = 2) +
#               
#               tm_shape(Local_Authority_Boundary,
#                        name = "Local Authority")+
#               tm_fill("layer",
#                       alpha = 0,
#                       legend.show = FALSE,
#                       id = "LAD15NM")+
#               tm_borders(col=NA,lwd = 2)
#             
#             tm %>%   
#               tmap_leaflet()  %>% 
#               
#               leaflet::hideGroup("PCN Boundaries")  %>% 
#               leaflet::hideGroup("Local Authority")   
#             
#           })
#         }
# })
#       }
#     })
  
  # 1.5 Geography
  
  observeEvent({input$oneGeoX
    input$oneGeoY
    input$oneGeoGroupBy},ignoreInit = TRUE, {
      if(toLoad$section1 & toLoad$geo) {
        data <- dat$attributes %>% select_at(input$oneGeoX) %>% as.data.frame()
        output$oneGeoPlot1 <- renderPlotly({clinicPlotSolve(dat, data, input$oneGeoY, groupLookUp(dat2, input$oneGeoGroupBy),
                                                            xtitle = makeNice(removePrefix2(input$oneGeoX)),
                                                            title = "Geography",
                                                            guideText = input$oneGeoGroupBy)%>%
            layout(barmode="overlay",
                   xaxis = list(tickangle = -45))%>% layout(
                     margin = list(b = 5*max(nchar(as.character(data[,1])))) # to fully display the x and y axis labels
                   )
          })
      }
    })
  
  # 1.6 Wider Determinants
  observeEvent({input$oneWidDetY
    input$oneWidDetX
    input$oneWidDetGroupBy},ignoreInit = TRUE, {
      if(toLoad$section1 & toLoad$widerDet) {

        data <- dat$attributes %>% select_at(input$oneWidDetX) %>% as.data.frame() ## needs to be reconfigured to use bands for continous data
        output$oneWidDetPlot1 <- renderPlotly({clinicPlotSolve(dat, data, input$oneWidDetY, groupLookUp(dat2, input$oneWidDetGroupBy), 
                                                               xtitle = makeNice(paste0("Determinant: ", makeNice(input$oneWidDetX %>% removePrefix))),
                                                               title = "Wider Determinants",
                                                               guideText = input$oneGeoGroupBy) %>%
            layout(barmode="overlay",
                   xaxis = list(tickangle = -45))
          })
      }
    })
  
  
  dataModal <- function() {
    modalDialog(
      HTML("<h2><strong>Definitions and FAQ</strong></h2>"),
      p(strong("Q: What is BtH Segmentation?")),
      p("A: BtH segmentation refers to ", a(href = "https://outcomesbasedhealthcare.com/bridges-to-health-segmentation-model/", target = "_blank", "Bridges to Health (BtH)"),
        ", a segmentation model for whole population outcomes based approaches."
      ),
      br(),
      p(strong("Q: What is meant by Deprivation?")),
      p("A: Deprivation refers to the ",
        a(href = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/853811/IoD2019_FAQ_v4.pdf", target = "_blank", "Index of Multiple Deprivation (IMD)")
      ),
      br(),
      p(strong("Q: What are 'GlobalGroups'?")),
      p("A: A selectin of variables that are user input derived."),
      br(),
      p(strong("Q: What can be a Wider Determinant?")),
      p("A: Wider Determinants data comes from The Index of Multiple Deprivation, and includes measures such as air pollution and crime."),
      br(),
      p(strong("Q: What does count or percentage refer to?")),
      p("A: Either raw count of the population or percentage of the population. "),
      br(),
      p(strong("Q: What are NAs/NULL/Missing labels?")),
      p("A: 'NA', 'NULL' and in some instances missing labels, refer to missing data - values in this category should be treated as 'missing' or 'unkown'."),
      br(),
      p(strong("Q: What does Â£s mean?")),
      p("A: Treat 'Â£s' is the same as '£s': the software packages used to create this tool cannot handle the symbol '£' by itself in some cases."),
      br(),
      p(strong("Acronyms")),
      p("ICP: Integrated Care Partnership. Corresponds to Localities"),
      p("LTC: Long Term Conditions"),
      p("POD: Point of Delivery"),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    )
  }

  lapply(c(
    "modalDefinitionCover",
    paste0("viz_help", 1:6),
    "viz_helpseg",
    "viz_helpcart",
    "viz_helpM",
    "viz_helpKMeans",
    "viz_helpRisk1",
    "viz_helpthreeCohort"
  ), function(x) {
    observeEvent(input[[x]], {
      showModal( dataModal() )
    })
  })

  observeEvent({input$graphDataModalOk
    input$viz_helpClinDataOk
    input$modalDropdownOneDemogOk
    input$section1DefinitionsOk}, {
      removeModal()
    })
  
  
  #### Modals for graph helptext
  graphModal <- function() {
    modalDialog(
      HTML("<h2><strong>Graph Tips</strong></h2>"), # Graphs Tips 
      p(strong("Most graphs are interactive."), " This means they provide a number of useful functions:"),
      p("1. You can hover over points or bars to see exact values."),
      p("2. You can select an area to zoom in (useful when graphs are small). To zoom out, double-click on the graph."),
      p("3. If a graph has a legend, clicking on items in the legend will toggle their visibility in the graph itself."),
      p("Hover over the top right of a graph to see all available options."),
      p("If the graphs on a page load in a small (squished) size, re-size the entire window to solve this."),
      p("Some methods of plotting(e.g. plotting by LSOA) can cause the x-axis to have so many ticks, the labels become illegible. Zooming in can help solve this."),
      br(),
      p("When handling large numbers, some graphs use 'K' to denote thousands, and 'M' for millions."),
      br(),
      HTML("<h2><strong>Dropdown Tips</strong></h2>"),
      p("Some dropdowns allow exactly one options, while others allow the user to leave it blank or select multiple options."),
      p("Backspace can be used to remove selected options. In the case of multiple selected options, you can use arrow keys to navigate between selected choices."),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose = TRUE
    )
  }
  
  lapply(paste0("modalGraphInfo", 
                c("",
                  "Cover",
                  "Zero",
                  "OneDemog",
                  "Clin",
                  "Act",
                  "Dep",
                  "Area",
                  "Wider",
                  "Seg",
                  "CART",
                  "M",
                  "KMeans",
                  "Risk1",
                  "ThreeCohort"
                )), function(x) {
                  observeEvent(input[[x]], {
                    showModal( graphModal() )
                  })
                })
  
  observeEvent(input$modalDropdownOneDemog, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Dropdowns</strong></h2>"),
        p(strong("1st dropdown (Split data by): "), "This option can be used to split each horizontal bar by a variety of options. See the dropdown for these."),
        p(strong("2nd dropdown (Stacked or Separate Graph): "), "If an option is selected from the dropdown above, changing this selection draws a population pyramid "),
        p("for each unique value of the now split bars."),
        p(strong("3rd dropdown (Counts or % of population): "), "Changes the axis resolution between % and raw count of population."),
        p(strong("4th dropdown (Split bands by condition): "), "Splits each bar by selected condition (those that have the condition, against those who don't). Click on the legend to toggle the visibility of these categories.", 
        "It is recommended the 1st and 4th dropdowns are not used simulatiously (for clarity of plot)."),

        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$viz_helpActDataCalc, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Data Calculation for this graph</strong></h2>"),
        p("All activity is first split of Point of Delivery (POD) before being joined with attributes data."),
        p("The data is then split by distinct values of the y-axis field."),
        # p("Please note that per head calculations are per active user: specifically, per individuals in the population with at least 1 recorded activity in the relevant POD."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  observeEvent(input$zeroFilterData, {
    showModal(
      modalDialog(# Data filter
        HTML("<h2><strong>Data Resolution</strong></h2>"),
        p("This option can be used to change the data to a specific ICP."),
        p("Please note this will take a few seconds for the ExploreR to do."),
        p("Comparison of ICPs is only possible when viewing the entire dataset.",
          "If an ICP is selected, PCNs are used as the defaul geographical unit."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  
  ##############################################
  ##
  ## Tab 2 Segmentation
  ## 
  ############################################
  observeEvent(input$oneClinMulti,{
    if(input$oneClinMulti!= "") {
      output$graph1 <- renderPlotly({isolate({
        values <- dat$attributes %>% select_at(input$oneClinMulti) %>% 
          colSums()
        values <- values / nrow(dat$attributes) * 100
        data <- data.frame(ltc = removePrefix2(input$oneClinMulti),
                           percentage = round(values, digits = 3))
        p <- data[order(data$percentage, decreasing = FALSE),] %>%
          mutate(ltc = factor(ltc, levels = ltc)) %>%
          ggplot(aes(x=ltc, y=percentage)) + 
          geom_bar(stat = "identity") + ggtitle("  ") +
          xlab("LTC") + ylab("% of population")  + 
          coord_flip()
        ggplotly(p)
      })
      })
    }
  })
  
  observeEvent(input$clin3x3Plot, {
    
    source("./functions/3x3plots.R", local = TRUE)
    if(!is.null(input$twoLTCAgeLTCSelection)) {
      # browser()
      data <- getThreeByThreePlotsData(dat$attributes, input)
      output$clin3x3graph <- renderPlot(isolate(getThreeByThreePlots(data, input)))
      
      d <- isolate(reformatTreeSegData(data, dat))
      
      output$two3x3TreeMap <- renderPlotly({
        renderTreeMapSegPlot(d, input$two3x3gTreeMapOptions)
      })
      output$two3x3Pie <- renderPlotly({
        renderPieSegPlot(d, input$two3x3gTreeMapOptions)
      })
      dat$attributes[[paste0("util.segmentation.", input$twoClinAgeName)]] <<- paste0(data$g.age, ", ", data$g.complexity)
      
      listText = input$twoClinAgeName
      listCol = paste0("util.segmentation.", input$twoClinAgeName)
      
      if(listText%in%dat2$groupByListText) {
        i <- which(dat2$groupByListText == listText)
      } else {
        i <- length(dat2$groupByListText) + 1
      }
      dat2$groupByListText[[i]] <<- listText
      dat2$groupByList[[i]] <<- listCol

      updateAllWithNewImportantField(dat)
    }
  })
  
  observeEvent({input$ltcSelection
    toLoad$section1}, ignoreInit = T, {
      if(toLoad$section1) {
        all = dat2$ltcCols
        names(all) = sapply(all, removePrefix) %>% sapply(makeNice)
        if(input$ltcSelection == "All Conditions"){
          options = dat2$ltcCols
        } else if (input$ltcSelection == "Major Conditions") {
          options = dat2$complexLTCCols
        } else if (input$ltcSelection ==  "Minor Conditions") {
          options = dat2$simpleLTCCols
        }
        # options <- clinOptions(options)
        names(options) = sapply(options, removePrefix) %>% sapply(makeNice)
        updatePickerInput(session, "twoLTCAgeLTCSelection", choices = all, selected = options)
      }
  })
  
  
  
  ############################################
  ## Bridges to Health

  source("./functions/segmentationBTHTools.R", local = TRUE)
  observeEvent({
    input$twoBTHsegment_percent
    input$twoBTHsegment_layout
    input$twoBTHFocus
    toLoad$segBTH
  }, {
    if(toLoad$segBTH) {
      if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
        output$twoBTHPlot <- renderPlotly({
          segment_proportion_bars(dat2 = getBtHGraph(dat, dat2, input$twoBTHFocus),
                                  stacked = input$twoBTHsegment_layout,
                                  percent_or_freq = input$twoBTHsegment_percent,
                                  focus = input$twoBTHFocus)
        })
        ## Treemap
        table <- cbind(bridges_to_health(dat, groupLookUp(dat2, "")), dat$attributes %>%
                         group_by(util.segmentation.BtHSegment) %>% 
                         summarise(cost = sum(total_cost),
                                   act = sum(total_act)) %>% ungroup() %>% select(cost, act))

        output$twoBthTreeMap <- renderPlotly({
          plot_ly(
            data = table%>% mutate(segment = util.segmentation.BtHSegment),
            type = "treemap",
            labels = paste0(table$util.segmentation.BtHSegment, "\n ", "Number of individuals: ", table$pcn_freq, "\n ", "Total Cost (£s): ", format(round(table$cost), big.mark = ","), "\n ", " Total Activity: ", format(round(table$act), big.mark = ","), " "),

            parents = NA,
            values = if(input$twoBtHTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoBtHTreeMapOptions == "Total Cost") {~cost} else if(input$twoBtHTreeMapOptions == "Total Activity") {~act},
            hovertemplate = paste0(table$util.segmentation.BtHSegment, "\n ", "Number of individuals: ", table$pcn_freq, "\n ", "Total Cost (£s): ", format(round(table$cost), big.mark = ","), "\n ", " Total Activity: ", format(round(table$act), big.mark = ","), " ", "<extra></extra>")
          )
        })

        output$twoBthPie <- renderPlotly({
            plot_ly(table, 
                    labels=~util.segmentation.BtHSegment,
                    values=if(input$twoBtHTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoBtHTreeMapOptions == "Total Cost") {~cost} else if(input$twoBtHTreeMapOptions == "Total Activity") {~act},#~pcn_freq,
                    marker = list(line = list(color = '#FFFFFF', width = 1)), 
                    type="pie",
                    textposition = ifelse(table$pcn_freq<50,"outside","inside"),textinfo = 'text',
                    hoverinfo = 'text',source = "subset",
                    text=paste0(table$util.segmentation.BtHSegment, "\n ","Number of individuals: ", table$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(table$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(table$act), big.mark = ","), " "),
                    insidetextfont = list(color = '#FFFFFF')) %>%
              layout(showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
        })

      }
    }
  })
  

  ############################################
  ## CART
  observeEvent(input$twoCARTgo, {
    # browser()
    if(!is.null(input$twoCARTVar1)) {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while the data is segmented.</strong></h2>"),
          p("This should finish in no more than a minute or two when using the default settings. Growing larger trees may take more time."),
          footer = tagList()
        )
      )
       try({
         add_sco <- function(x){
           gsub(" ", "_", x, fixed = T)
         }
         # browser()
        library(rpart)
        library(rpart.plot)
        print("Libraries loaded")
        data <- as.data.frame(dat$attributes %>% select_at(c(unique(input$twoCARTVar1), input$twoCARTVar2)))
        # colnames(data) <- c(sapply(c(unique(input$twoCARTVar1)),add_sco) , input$twoCARTVar2)
        # browser()
        colnames(data) <- c(sapply(c(unique(input$twoCARTVar1)),makeNice) , input$twoCARTVar2)
        # colnames(data) <- c(sapply(c(unique(input$twoCARTVar1)),removePrefix) %>% sapply(makeNice), input$twoCARTVar2)

        # # Error removal disabled; now assume no NAs in code
        # print("CART model NA removal")
        # for(i in colnames(data)) {
        #   if(NA %in% data[[i]]) {
        #     if(is.numeric(data[[i]])) {
        #       data[[i]][is.na(data[[i]])] <- sapply(data[[i]][is.na(data[[i]])], function(x) {if(is.na(x)) {0} else {x}})
        #     } else {
        #       data[[i]][is.na(data[[i]])] <- sapply(data[[i]][is.na(data[[i]])], function(x) {if(is.na(x)) {"Missing Data"} else {x}})
        #     }
        #   }
        # }

        print("CART model")
        model<-rpart(as.formula(paste(input$twoCARTVar2, ".", sep=" ~ ")),data=data,
                     control=rpart.control(minbucket=input$twoCARTminbucket,
                                           maxdepth=input$twoCARTmaxdepth,
                                           cp=input$twoCARTcp))
        print("CART model end")
        output$twoCARTTree <- renderPlot({
          rpart.plot(model, type = 3, clip.facs = TRUE, branch = .3, under = TRUE, cex = 1.2,
                     cex.main = 1.4,
                     main = paste0("Decision Tree \n text in leaves is average ", makeNicePPY(input$twoCARTVar2), " for the leaf; number below is % of population in node\n")
          )
        })
        dat2$cartModel <<- model
        ids <- sort(unique(dat2$cartModel$where))
        rules <- rpart.rules(dat2$cartModel)#cover = TRUE,
        newIDs <- 1:length(ids)
        names(newIDs) <- ids
        newSeg <- newIDs[as.character(dat2$cartModel$where)]
        # dat$attributes$util.segmentation.CARTCluster <<- newSeg
        dat$attributes[[paste0("util.segmentation.", input$twoCARTName)]] <<- newSeg
        d <- data.frame(Segment = paste0("Segment ",newSeg), Definition = paste0("Segment ",newSeg), focus = "",
                        total_cost = dat$attributes$total_cost,
                        total_act = dat$attributes$total_act) %>%
          group_by(Segment,
                   Definition) %>%
          summarise(pcn_freq = n(),
                    cost = sum(total_cost),
                    act = sum(total_act)) %>%
          ungroup() %>%
          mutate(pcn_percent = 100*pcn_freq/sum(pcn_freq)) %>%
          group_by(Segment,
                   Definition) %>%
          mutate(bnssg_freq = sum(pcn_freq)) %>%
          ungroup() %>%
          mutate(bnssg_percent = 100*bnssg_freq/sum(bnssg_freq),
                 segment = Segment)
        output$twoCARTTreeMap <- renderPlotly({

          plot_ly(
            data = d,
            type = "treemap",
            labels = paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " "),
            parents = NA,#~bnssg_freq,
            values = if(input$twoCARTTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoCARTTreeMapOptions == "Total Cost") {~cost} else if(input$twoCARTTreeMapOptions == "Total Activity") {~act},
            hovertemplate = paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " ", "<extra></extra>")
          )
        })

        output$twoCARTPie <- renderPlotly({
          plot_ly(d, labels=~Segment,values=if(input$twoCARTTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoCARTTreeMapOptions == "Total Cost") {~cost} else if(input$twoCARTTreeMapOptions == "Total Activity") {~act},#~pcn_freq,
                  marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                  textposition = ifelse(d$pcn_freq<50,"outside","inside"),textinfo = 'text',
                  hoverinfo = 'text',source = "subset",
                  text=paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " "),
                  insidetextfont = list(color = '#FFFFFF')) %>%
            layout(showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
        })

        listText = input$twoCARTName
        listCol = paste0("util.segmentation.", input$twoCARTName)
        
        if(listText%in%dat2$groupByListText) {
          i <- which(dat2$groupByListText == listText)
        } else {
          i <- length(dat2$groupByListText) + 1
        }

        dat2$groupByListText[[i]] <<- listText
        dat2$groupByList[[i]] <<- listCol

        updateAllWithNewImportantField(dat)
        
        updateCheckboxInput(session, "twoCARTTreeRules", value = FALSE)
      })
      removeModal()
    }
  })

  output$twoCARTTreeRulesTableUI <- renderUI({
    if(input$twoCARTTreeRules) {
      column(width = 12,
             tableOutput("twoCARTTreeRulesTable"),style = "overflow-y: scroll;overflow-x: scroll;",
      )
    } else {
      uiOutput("twoCARTTreeRulesTableUIBlank")
    }
  })
  observeEvent(input$twoCARTTreeRules, ignoreInit = T, {
    if(input$twoCARTTreeRules & !is.na(dat2$cartModel)) {
      output$twoCARTTreeRulesTable <- renderTable({
        rules <- rpart.rules(dat2$cartModel, nn = TRUE)
        newIDs <- 1:nrow(rules)
        names(newIDs) <- sort(as.integer(rules$nn))
        newSeg <- newIDs[as.character(rules$nn)]
        rules <- rules[,-1]
        colnames(rules)[1:2] <- c(makeNice(colnames(rules)[1]), "Rule")
        colnames(rules)[3:ncol(rules)] <- " "
        cbind("Segment" = paste0("Segment ", newSeg), rules)
      })
    }
  })

  observeEvent(input$twoCARTInfoD1, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Why should I use CART?</strong></h2>"),
        p("Decision Trees are a well-established statistical learning approach to identify cohorts by objectively determining the
                      person-related attributes to *branch* upon. They are conceptually appropriate to this problem and can yield high levels of discrimination through
                      offering a locally-calibrated solution. This is in contrast to the other methods where there is no guarantee that segment membership rules can be
                      transferred between different demographics, regions and time periods without losing power. That is, the Bridges to Health segment criteria may provide
                      good discrimination on the population where it has been developed, but not necessarily on others."),
        p("The Decision Tree propagates in a binary fashion where two-way splits are made on the statistically-selected explanatory variables at each
                      level. The statistical rigour of the method ensures an optimal number of segments are returned, i.e. splits are only made which yield
                      meaningful improvements in discrimination (in difference to the other considered methods)."),

        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  observeEvent(input$twoCARTInfoD2, {
    showModal(
      modalDialog(
        HTML("<h2><strong>How to read the decision tree</strong></h2>"),
        p("The population being segmented is grouped into the leaf nodes (nodes at the bottom of the tree), according to the rules on the branches."),
        br(),
        p("Text inside the leaf nodes is the average (mean) target variable (with respect to the individuals in the node). Text below is the % of population in that node."),
        br(),
        p("Please note in some cases labels on the graph may overlap. For this reason, you can toggle a tabel below the tree to show the rules leading to each segment."),

        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })

  ############################################
  ## Clusters
  
  observeEvent(input$twoClusterGo, {
    if(!is.null(input$twoClusterVars) & length(input$twoClusterVars) >= 1) {
      try({
      data <- as.data.frame(dat$attributes %>% select_at(c(unique(input$twoClusterVars))))
        if(nrow(data) * ncol(data) > 4*(10^6)) {
          gc()
          showModal(
            modalDialog(
              HTML("<h2><strong>Warning!</strong></h2>"),
              p("A high number of data points are present (", nrow(data), " individuals * ",ncol(data), "columns). This can cause cause exceptions if the programs runs out of memory."),
              p("Please either proceed, or reduce the data being presented."),
              footer = tagList(actionButton("twoKMeansTooManyItems", "Proceed"), modalButton("Cancel"))
            )
          )
        } else {
          runC()
        }
        })
    }
  })

  observeEvent(input$twoKMeansTooManyItems, runC())
  
  runC <- function() {
    {
      showModal(
        modalDialog(
          HTML("<h2><strong>Please wait while processing.</strong></h2>"),
          p("When using multiple fields, this may take a while - assume 10 second per field."),
          footer = tagList()
        )
      )
      result <- try({
        data <- as.data.frame(dat$attributes %>% select_at(c(unique(input$twoClusterVars))))
        # colnames(data) <- c(sapply(c(unique(input$twoClusterVars)),removePrefix) %>% sapply(makeNice))
        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        colnames(data) <- sapply(c(unique(input$twoClusterVars)),add_sco)

        
        
        print("Cluster NA removal")
        for(i in colnames(data)) {
          if(NA %in% data[[i]]) {
            if(is.numeric(data[[i]])) {
              data[[i]][is.na(data[[i]])] <- sapply(data[[i]][is.na(data[[i]])], function(x) {if(is.na(x)) {0} else {x}})
            } else {
              data[[i]][is.na(data[[i]])] <- sapply(data[[i]][is.na(data[[i]])], function(x) {if(is.na(x)) {"Missing Data"} else {x}})
            }
          }
        }
        gc()
        print("Cluster FAMD")
        library(FactoMineR)
        
        # Note here: if every variable is numeric, use normal principal component analysis
        if(length(which(sapply(data, is.numeric))) < ncol(data)) {
          res.famd <- FAMD(data, graph = FALSE, ncp = ncol(data))$ind$coord
        } else {
          print("Resorting to PRINCOMP - all numeric")
          res.famd <- princomp(data)$scores
        }
        gc()
        print("KMeans")
        library(cluster)
        means <- kmeans(res.famd, max(input$twoClusterCenter,1))
        dat$attributes[[paste0("util.segmentation.",input$twoClusterName)]] <<- means$cluster
        d <- data.frame(Segment = paste0("Segment ",means$cluster), Definition = paste0("Segment ",means$cluster), focus = "",
                        total_cost = dat$attributes$total_cost,
                        total_act = dat$attributes$total_act) %>%
          group_by(Segment,
                   Definition) %>%
          summarise(pcn_freq = n(),
                    cost = sum(total_cost),
                    act = sum(total_act)) %>% 
          ungroup() %>% 
          mutate(pcn_percent = 100*pcn_freq/sum(pcn_freq)) %>% 
          group_by(Segment,
                   Definition) %>% 
          mutate(bnssg_freq = sum(pcn_freq)) %>% 
          ungroup() %>% 
          mutate(bnssg_percent = 100*bnssg_freq/sum(bnssg_freq),
                 segment = Segment)
        output$twoClusterTreeMap <- renderPlotly({
          plot_ly(
            data = d,
            type = "treemap",
            labels = paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " "),
            parents = NA,#~bnssg_freq,
            values = if(input$twoClusterTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoClusterTreeMapOptions == "Total Cost") {~cost} else if(input$twoClusterTreeMapOptions == "Total Activity") {~act},
            hovertemplate = paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " ", "<extra></extra>")
          )
        })
        
        output$twoClusterPie <- renderPlotly({
          plot_ly(d, labels=~Segment,values=if(input$twoClusterTreeMapOptions == "Number of individuals") {~bnssg_freq} else if(input$twoClusterTreeMapOptions == "Total Cost") {~cost} else if(input$twoClusterTreeMapOptions == "Total Activity") {~act},#~pcn_freq, 
                  marker = list(line = list(color = '#FFFFFF', width = 1)), type="pie",
                  textposition = ifelse(d$pcn_freq<50,"outside","inside"),textinfo = 'text',
                  hoverinfo = 'text',source = "subset",
                  text=paste0(d$Segment, "\n ","Number of individuals: ", d$pcn_freq, "\n ",  "Total Cost (£s): ", format(round(d$cost), big.mark = ","), "\n ", "Total Activity: ", format(round(d$act), big.mark = ","), " "),
                  insidetextfont = list(color = '#FFFFFF')) %>%
            layout(showlegend = FALSE,separators = ',.') %>% config(displayModeBar = F)
        })
        ## Add to activity plots
        listText = input$twoClusterName#"KMeans Clusters"
        listCol = paste0("util.segmentation.",input$twoClusterName)#"util.segmentation.KMeansCluster"
        
        if(listText%in%dat2$groupByListText) {
          i <- which(dat2$groupByListText == listText)
        } else {
          i <- length(dat2$groupByListText) + 1
        }
        dat2$groupByListText[[i]] <<- listText
        dat2$groupByList[[i]] <<- listCol
        
        updateAllWithNewImportantField(dat)
      })
      if(class(result) == "try-error") {
        output$twoEE <- renderText(result)
      } else {
        output$twoEE <- renderText("")
      }
      removeModal()
    }
  }
 
  
  observeEvent({
    input$twoClusterVar1
    input$twoClusterVar2
  }, {
    if(toLoad$section1) {
      if(isolate(input$twoClusterVar1) != input$twoClusterVar2) {
        if(length(unique(dat$attributes[[input$twoClusterVar1]])) < 20 && length(unique(dat$attributes[[input$twoClusterVar2]])) < 20 ) {
          print(length(unique(dat$attributes[[input$twoClusterVar1]])))
          print(length(unique(dat$attributes[[input$twoClusterVar2]])))
          output$twoClusterTestPlot <- renderPlotly({
            segment_proportion_bars_general(dat2 = isolate(
                                              dat$attributes %>% 
                                              select_at(c(input$twoClusterVar1,input$twoClusterVar2)) %>%
                                              group_by_at(c(input$twoClusterVar1,input$twoClusterVar2)) %>%
                                              summarise(pcn_freq = n()) %>% 
                                              ungroup() %>% 
                                              group_by_at(input$twoClusterVar2) %>% 
                                              mutate(pcn_percent = 100*pcn_freq/sum(pcn_freq)) %>% 
                                              ungroup() %>% 
                                              group_by_at(input$twoClusterVar1) %>% 
                                              mutate(bnssg_freq = sum(pcn_freq)) %>% 
                                              ungroup() %>% 
                                              group_by_at(input$twoClusterVar2) %>% 
                                              mutate(bnssg_percent = 100*bnssg_freq/sum(bnssg_freq)) %>% 
                                              ungroup()),
                                            stacked = input$twoClustersegment_layout,
                                            percent_or_freq = input$twoClustersegment_percent)
          })
        }
      }
    }
  })
  
  
  segment_proportion_bars_general <- function(
    dat2,
    stacked,
    percent_or_freq,
    fill_colours = "default",
    number_of_columns=2){
    
    number_of_segments <- length(unique(dat2[[isolate(input$twoClusterVar1)]])) 
    
    segment_colours <- hue_pal()(number_of_segments)
    
    plot_dat <- dat2 %>% 
      mutate(text = if(percent_or_freq=="percent"){
        paste0(round(pcn_percent,2),
               "% of ",
               makeNice(isolate(input$twoClusterVar2)), 
               " ",
               !!sym(isolate(input$twoClusterVar2)),
               " patients",
               "\nAre in the ",
               !!sym(isolate(input$twoClusterVar1)),
               " segment")
      } else {
        paste0(case_when(pcn_freq > 5 ~ format(pcn_freq,big.mark=","), T ~"*supressed"),
               " ",
               makeNice(isolate(input$twoClusterVar2)),
               " ",
               !!sym(isolate(input$twoClusterVar2)),
               " patients",
               "\nAre in the ",
               !!sym(isolate(input$twoClusterVar1)),
               " segment")
      })
    # browser()
    plot <- plot_dat %>% 
      ggplot(aes(x=factor(!!sym(isolate(input$twoClusterVar2))),
                 y=!!rlang::sym(paste0("pcn_",percent_or_freq)),
                 fill=factor(!!sym(isolate(input$twoClusterVar1))),
                 text=text
      )
      ) +
      geom_col() +
      theme_minimal() +
      labs(x="",
           y="",
           fill = "Segments") +
      coord_flip() +
      scale_y_continuous(labels=ifelse(percent_or_freq=="percent",
                                       function(x) paste0(x,"%"),
                                       scales::comma))+
      scale_fill_viridis_d()
    # f2 <- isolate(input$twoClusterVar1)
    if(stacked == "separate"){
      plot <- plot +
        facet_wrap(sym(isolate(input$twoClusterVar1)),
                   ncol = number_of_columns
        ) +
        theme(legend.position = "none",
              panel.spacing.y=unit(-0.5,"lines")
        )
    }
    return(ggplotly(plot,tooltip="text"))
  }
  
  
  ##############################################
  ##
  ## Tab 3 Cohort Identification
  ## 
  ############################################


  observeEvent(toLoad$threeCohort, {
    # browser()
    print("dat2$threeCohort$rules")
    if(toLoad$threeCohort) {
      print(dat2$threeCohort$rules)
      updatethreeCohortIDUI(toLoad, input, output, dat2, dat)
    }
  })
  # Add new line option
  observeEvent({input$threeCohortNewUI}, {
    print("Adding new line")
    
    l <- length(dat2$threeCohort$rules)
    freezeReactiveValue(input,"threeCohortNewUI")
    freezeReactiveValue(input, paste0("threeCohortRule",l+1,"Delete"))

    dat2$threeCohort$rules[[l+1]] <<- c(dat2$cohortDefaultCols[1], 
        "in (multiple choices select)",
        dat2$cohortDefaultVals)
    
    dat2$threeCohort$join[[l]] <<- isolate(input$threeCohortUIandor)
    print(dat2$threeCohort)
    toLoad$threeCohortRules <<- toLoad$threeCohortRules + 1
    if(toLoad$threeCohortRules > toLoad$threeCohortRulesE) {
      toLoad$threeCohortRulesE <<- toLoad$threeCohortRules
    }
    
    updatethreeCohortIDUI(toLoad, input, output, dat2, dat)
    
    updatethreeCohortIDUISelectsFull(toLoad, input, output, dat2)
    
    print("Added new line")
  })

  
  updatethreeCohortIDUI <- function(toLoad, input, output, dat2, dat) {
    print("Updating UI")
    
    output$threeCohortUI <- renderUI({
      lapply(1:length(dat2$threeCohort$rules), function(x) {
        s2 <- dat2$threeCohort$rules[[x]][1]
        names(s2) <- s2 %>% removePrefix %>% makeNice
        t <- dat2$groupByList[!is.na(dat2$groupByList)]
        names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
        
        t2 <- c("total_cost", "total_act")
        names(t2) <- sapply(c("total_cost", "total_act"),makeNicePPY)
        
        cm <- dat$attributes%>%select(starts_with("clinic.misc")) %>% colnames
        names(cm) <- sapply(cm, removePrefix) %>% sapply(makeNice)
        
        segColNames2 <- list(
          dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
          c(cm, dat2$clinicCols),
          dat2$areaCols,
          dat2$socioCols,
          t,
          t2
        )
        names(segColNames2) <- c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "GlobalGroups", "Costs and Activity")
        
        fluidRow(
          if(x > 1) {
            column(width = 12,
                   p(tags$b(dat2$threeCohort$join[[x - 1]]))
            )
          },
          column(width = 4,
                 pickerInput(inputId = paste0("threeCohort", "Rule", x, "Field"),
                             label = "Select Field",
                             choices = segColNames2,
                             selected = s2)
          ),
          column(width = 3,
                 pickerInput(inputId = paste0("threeCohort", "Select",x),
                             label = "Select operator for highlighting field values",
                             choices = c("=", "in numeric range", ">=", "<=", "in (multiple choices select)"),
                             selected = dat2$threeCohort$rules[[x]][2]
                 )
          ),
          column(width = 3,
                 uiOutput(paste0("threeCohort", "UI",x))
          ),
          column(width = 2,
                 actionButton(paste0("threeCohort", "Rule",x,"Delete"), "Delete Field")
          )
        )
      })
    })
  }
  
  updatethreeCohortIDUISelectsFull <- function(toLoad, input, output, dat2) {
    print("Updating every select operator UI, from memory")
    lapply(1:toLoad$threeCohortRules, function(x) {
      renderthreeCohortIDUISelectsX(x, dat, dat2, data, input, output)
    })
  }
  
  renderthreeCohortIDUISelectsX <- function(x, dat, dat2, data, input, output, new = F) {
    print(paste0("Updating select ", x))
    operator <- dat2$threeCohort$rules[[x]][2]
    output[[paste0("threeCohortUI",x)]] <- renderUI({
      if(operator == "=") {
        textInput(paste0("threeCohort", x,"SelectI"), label = "Value", value = dat2$threeCohort$rules[[x]][3])
      } else if (operator == "in numeric range") {
        column(width = 12,
               textInput(paste0("threeCohort", x,"SelectR1"), label = "Value", value = dat2$threeCohort$rules[[x]][3]),
               p(paste0("<= ",makeNice(input[[paste0("threeCohortRule",x,"Field")]]), " <=")),
               textInput(paste0("threeCohort", x,"SelectR2"), label = "Value", value = dat2$threeCohort$rules[[x]][4]),
        )
      } else if (operator == ">=") {
        textInput(paste0("threeCohort", x,"SelectI"), label = "Value", value = dat2$threeCohort$rules[[x]][3])
      } else if (operator == "<=") {
        textInput(paste0("threeCohort", x,"SelectI"), label = "Value", value = dat2$threeCohort$rules[[x]][3])
      } else if (operator == "in (multiple choices select)") {
        checkboxGroupInput(paste0("threeCohort", x,"SelectC"), label = "Value(s):", choices = 
                             if(length(unique(dat$attributes[,dat2$threeCohort$rules[[x]][1]])) > 200) {
                               "Too many options to display."
                             } else {
                               sort(unique(dat$attributes[,dat2$threeCohort$rules[[x]][1]]))
                             },
                           selected = dat2$threeCohort$rules[[x]][3:length(dat2$threeCohort$rules[[x]])])
      }
    })
  }

  observeEvent(toLoad$threeCohortRulesE, {
    lapply(toLoad$threeCohortRulesE, function(x) {
      observeEvent(input[[paste0("threeCohortSelect",x)]], ignoreInit = T, {
        print(paste0("Update of select uis (operator) + memory ", x))
        dat2$threeCohort$rules[[x]][2] <<- isolate(input[[paste0("threeCohortSelect",x)]])
        renderthreeCohortIDUISelectsX(x, dat, dat2, data, input, output)
      })
      # need error checking to make sure we can't enter a null value
      observeEvent(input[[paste0("threeCohortRule",x,"Field")]], ignoreInit = T, {
        print(paste0("Adding field to memory ", x))
        if(dat2$threeCohort$rules[[x]][1] != input[[paste0("threeCohortRule",x,"Field")]]) {
          dat2$threeCohort$rules[[x]][1] <<- input[[paste0("threeCohortRule",x,"Field")]]
          # reset accepted values
          dat2$threeCohort$rules[[x]] <<- c(dat2$threeCohort$rules[[x]][1:2], NULL)
        }
        # Update the available options for this
        renderthreeCohortIDUISelectsX(x, dat, dat2, data, input, output, new = T)
      })
      
      # Wipe memory of selected options - if many from multiple select
      observeEvent(input[[paste0("threeCohort", x,"SelectI")]], ignoreInit = T, {
        print("Adding selected value to memory1")
        dat2$threeCohort$rules[[x]] <<- isolate(c(dat2$threeCohort$rules[[x]][1:2], input[[paste0("threeCohort", x,"SelectI")]]))
      })
      
      observeEvent(input[[paste0("threeCohort", x,"SelectR1")]], ignoreInit = T, {
        print("Adding selected value to memory2")
        dat2$threeCohort$rules[[x]] <<- isolate(c(dat2$threeCohort$rules[[x]][1:2], input[[paste0("threeCohort", x,"SelectR1")]]))
      })
      
      observeEvent(input[[paste0("threeCohort", x,"SelectR2")]], ignoreInit = T, {
        print("Adding selected value to memory3")
        dat2$threeCohort$rules[[x]] <<- isolate(c(dat2$threeCohort$rules[[x]][1:3], input[[paste0("threeCohort", x,"SelectR2")]]))
      })
      
      observeEvent(input[[paste0("threeCohort", x,"SelectC")]], ignoreInit = T, {
        print(paste0("Adding selected value to memory4 ", x))
        dat2$threeCohort$rules[[x]] <<- c(dat2$threeCohort$rules[[x]][1:2],
                                          isolate(input[[paste0("threeCohort", x,"SelectC")]])
        )
      })
      
      observeEvent(input[[paste0("threeCohortRule",x,"Delete")]], ignoreInit = T, {
        if(length(dat2$threeCohort$rules) > 1) {
          print("Deleting a line")
          dat2$threeCohort$rules <<- dat2$threeCohort$rules[-x]
          if(length(dat2$threeCohort$join) < 2) {
            dat2$threeCohort$join <<- list()
          } else {
            dat2$threeCohort$join <<- dat2$threeCohort$join[-min(x, length(dat2$threeCohort$join))]
          }
          toLoad$threeCohortRules <<- toLoad$threeCohortRules - 1
          updatethreeCohortIDUI(toLoad, input, output, dat2, dat)
          freezeReactiveValue(input, paste0("threeCohortSelect",x))
          freezeReactiveValue(input, paste0("threeCohortRule",x,"Delete"))
          # freezeReactiveValue(input, paste0("threeCohortRule",x,"FieldType"))
        } else {
          output$threeCohortGetIDsMessage <- renderText("Unable to delete, only 1 line remaining")
          freezeReactiveValue(input, paste0("threeCohortRule",x,"Delete"))
        }
      })
    })
  })
  
  
  observeEvent(input$threeCohortGetIDs,{
    print("Attempting to get cohort")
    if(length(which(sapply(dat2$threeCohort$rules, length) > 2)) == length(dat2$threeCohort$rules)) {
      listText = input$threeCohortIDName
      listCol = paste0("util.cohort.", input$threeCohortIDName)
      if(!(listText%in%dat2$groupByListText)) {
        i <- length(dat2$groupByListText) + 1
        dat2$groupByListText[[i]] <<- listText
        dat2$groupByList[[i]] <<- listCol
      }
      dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] <<- getAnalysisDataset(input,dat,dat2$threeCohort)
      dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] <<- case_when(dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] ~ "Yes",
                                                                                        T ~ "No")
      output$threeCohortGetIDsMessage <- renderText(paste0("Successfully identified ",
                                                           isolate(length(which(dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] == "Yes"))),
                                                           " individuals in dataset.", Sys.time()))
      print("Got data")
      
      updateAllWithNewImportantField(dat)
      addCohortDownload()

    } else {
      output$threeCohortGetIDsMessage <- renderText(paste0("Missing values in rules."))
    }
    
  })
  addCohortDownload <- function() {
      output$threeCohortDownload <- renderUI({
        list(
          column(width = 3,
                 downloadButton("threeCohortDownloadButton", paste0("Download Cohort"))
          ),
          column(width = 2,
                 p(" ", actionLink(inputId = "threeIDText1",
                                   icon = icon("info-circle"),
                                   label = HTML(""))),
                 bsTooltip("threeIDText1", title = "Cohort downloaded is determined by Cohort Name above, not latest cohort added.")
                 )
        )
      })
      
  }

  output$threeCohortDownloadButton <- downloadHandler(
    filename = function() {
      paste(input$threeCohortIDName, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat$attributes$id[dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] == "Yes"], file, col.names = F,row.names = F)
    }
  )
  
  observeEvent(input$threeCohortUploadIdsButton, {
    ext <- tools::file_ext(input$threeCohortUploadIds$datapath)
    if (ext == "csv") {
      dat2$cohortIDs <<- read.csv(input$threeCohortUploadIds$datapath, stringsAsFactors = F, header = F)
      if(ncol(dat2$cohortIDs) == 1 && nrow(dat2$cohortIDs) > 0) {
        # Correct format
        listText = input$threeCohortIDName
        listCol = paste0("util.cohort.", input$threeCohortIDName)
        if(!(listText%in%dat2$groupByListText)) {
          i <- length(dat2$groupByListText) + 1
          dat2$groupByListText[[i]] <<- listText
          dat2$groupByList[[i]] <<- listCol
        }
        dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] <<- dat$attributes$id %in% dat2$cohortIDs[[1]]
        dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] <<- case_when(dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] ~ "Yes",
                                                                                        T ~ "No")
        output$threeCohortGetIDsMessage <- renderText(paste0("Successfully identified ",
                                                             length(which(dat$attributes[[paste0("util.cohort.", input$threeCohortIDName)]] == "Yes")),
                                                             " individuals in dataset."))
        
        updateAllWithNewImportantField(dat)
        addCohortDownload()

      }
    }
  })
  
  observeEvent(input$threeCohort1, {
    showModal( modalDialog(
      title = "What is a cohort?",
      p("A cohort is a subset of the dataset, designed to help compare a selected group against the rest of the population. For example, this could be all individuals with hypertension."),
      p("Defining a cohort adds a new variable to the data, that when selected splits the population into those in the cohort ('Yes') and those not ('No')."),
      br(),
      p("Please note adding a cohort is not a requirement for using the ExploreR. Select a tab on the left to move on."),
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
  
  observeEvent(input$threeCohort2, {
    showModal( modalDialog(
      title = "Creating a cohort",
      
      p("To create a cohort, follow these steps:"),
      
      p("1. Determine what criteria defines the cohort"),
      p("2. Split this criteria into simple statements, such as 'age > 18', and 'has hypertension'"),
      
      p("3. For each of these statements, create a new line (using button 'Add new field'). Note these clauses are joined either by OR/AND. 
        This can be changed by the dropdown to the left of the button. Clauses are evaluated top to bottom. Both operators have the same priority."),
      p("4. For each clause, select the appropriate field name, then the values to include in the cohort. There are multiple ways of selecting these values. ",
        "These options can be selected from the dropdown to the right of the field. Details of the operators are at the bottom of this popup."),
      p("5. Click 'Get Cohort' to select individuals defined by the criteria, and add the field to the data. A message will appear to indicate success."),
      br(),
      hr(),
      p(strong("Operators")),
      p("'=' - equals to a specific value"),
      p("'in numeric range' - values of selected field lie between 2 input values. Suitable for numeric data. Uses <= and >="),
      p("'>=' and '<=' - lower/upper limit, respectively"),
      p("'in (multiple choice select)' - multiple values to include. Recommended for non-numeric options. Useful for seeing distinct values of the selected field. Cannot be used if more than 200 unique values are present."),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
  
  observeEvent(input$threeCohort3, {
    showModal( modalDialog(
      title = "Uploading IDs",
      p("If there is a pre-identified group of interest, IDs can be uploaded via a .csv file."),
      p("This file should contain only a single column of IDs, with no row or column names."),
      
      footer = tagList(
        modalButton("OK", icon = NULL)
      ),
      easyClose =TRUE
    ))
  })
 
 ##############################################
 ##
 ## Tab 4 Theoplot
 #
 ## 
 ############################################
 
 source("./functions/theoplots.R", local = TRUE)
 # Theoplot Load
 observeEvent(toLoad$loadTheo, {
   # On load
   if(toLoad$loadTheo) {
     print("Theoplot Load")
    # browser() 
     updateDateRangeInput(session, "fourTheoDate", start = dat2$mintime, end = dat2$maxtime)
     updateDateRangeInput(session, "dateidMulti", start = dat2$mintime, end = dat2$maxtime)
     # updateSelectInput(session, "fourTheoSearchID", choices = if(length(dat$attributes$id) > 500) {dat$attributes$id[1:500]} else {dat$attributes$id})
     # updateTextInput(session, "fourTheoID", value = dat$attributes$id[1])
     
     t <- dat2$groupByList[!is.na(dat2$groupByList)]
     names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
     updatePickerInput(session, "fourTheoSingleGlobalGroup", choices = t)
     updatePickerInput(session, "fourTheoMultiGlobalGroup", choices = t)
     
     tree <- getTree(dat2$theorows%>%as.data.frame())
     output$fourTheoMultiActivityToPlot <- renderTree({
       tree
     })
     output$fourTheotree <- renderTree({
       for(i in 1:length(tree)) {
         attr(tree[[i]],"stselected")=TRUE
       }
       tree
     })

     updateSliderInput(session, "fourTheoFilterLTCSlider", min = 0, max = max(dat$attributes[[dat2$ltc]]), value = c(0, max(dat$attributes[[dat2$ltc]])))
     updateSliderInput(session, "fourTheoFilterLTCSliderMulti", min = 0, max = max(dat$attributes[[dat2$ltc]]), value = c(0, max(dat$attributes[[dat2$ltc]])))

     sample_nhs_num <- dat$attributes %>%
       filter(.data[[dat2$age]] <= 100 & .data[[dat2$age]] >= 10,
              .data[[dat2$ltc]] >= 0 & .data[[dat2$ltc]] <= max(dat$attributes[[dat2$ltc]])) %>%
       dplyr::sample_n(size = 1) %>%  dplyr::pull(id)

     if(length(sample_nhs_num) == 0){
       print("Error: no patients in this category")
     } else {
       # updateTextInput(inputId = "fourTheoID", session = session, value = sample_nhs_num)
       output$figureplotly <- renderPlotly(plotPatientActivityPlotly2(theoData(sample_nhs_num, dat, dat2), dat2, input$fourTheoDate))
     }
     output$fourTheoMultiPlotText <- renderText({"No activity type selected to be plotted (Options tab)."})

   }
 })

 ###############################
 # Single Theoplot
 
 observeEvent(input$fourTheoSingleGlobalGroup, ignoreInit = T, {
   print("Valeus update")
   updatePickerInput(session, "fourTheoSingleGlobalGroupValues", choices = sort(unique(as.character(dat$attributes[[input$fourTheoSingleGlobalGroup]]))))
 })
 
 observeEvent(input$fourTheoSingleGlobalGroupGo, {
   sample_nhs_num <- dat$attributes %>%
     filter(.data[[input$fourTheoSingleGlobalGroup]] %in% input$fourTheoSingleGlobalGroupValues) %>%
     dplyr::sample_n(size = min(1, nrow(.))) %>%  
     dplyr::pull(id)
   if(length(sample_nhs_num) == 0){
     # Error handle
     output$fourTheoSingleErr <- renderText({paste0("Could not find anyone in data with selected attributes, request at ", Sys.time())})
     output$figureplotly <- renderPlotly({
       plotly_empty()
     })
   } else {
     output$fourTheoSingleErr <- renderText(" ")
     output$figureplotly <- renderPlotly(plotPatientActivityPlotly2(theoData(sample_nhs_num, dat, dat2),dat2,input$fourTheoDate))
   }
 })
 
 # Options tab 2 (By filter)
 observeEvent(input$fourTheoFilterGo, {
     sample_nhs_num <- dat$attributes %>%
       filter(.data[[dat2$age]] <= input$fourTheoFilterAgeSlider[2] & .data[[dat2$age]] >= input$fourTheoFilterAgeSlider[1],
              .data[[dat2$ltc]] >= input$fourTheoFilterLTCSlider[1] & .data[[dat2$ltc]] <= input$fourTheoFilterLTCSlider[2]) %>%
       dplyr::sample_n(size = min(nrow(.),1)) %>%  
       dplyr::pull(id)
     if(length(sample_nhs_num) == 0){
       # Error handle
       output$fourTheoSingleErr <- renderText({paste0("Could not find anyone in data with selected attributes, request at ", Sys.time())})
       output$figureplotly <- renderPlotly({
         plotly_empty()
       })
     } else {
       output$fourTheoSingleErr <- renderText(" ")
       output$figureplotly <- renderPlotly(plotPatientActivityPlotly2(theoData(sample_nhs_num, dat, dat2),dat2,input$fourTheoDate))
     }
 })

 
 
 ###############################
 # Options tab 3 (File Upload)
 observeEvent(input$theoFileInput, {
   ext <- tools::file_ext(input$theoFileInput$datapath)
   # browser()
   if (ext == "csv") {
     dat2$theoFile <<- read.csv(input$theoFileInput$datapath,stringsAsFactors = F, header = F)
     # Need: 1st column to be integer, 2nd and 3rd date, 4th date?
     n <- ncol(dat2$theoFile)
     if(n > 4 | n < 1) {
       output$theoFileUI <- renderUI({p("Something went wrong while loading the file.")})
       # throw exception - unrecognised format
     } else {
       # Handle file
       # Assume columns contain correct data types
       dat2$theoFile <<- as.data.frame(dat2$theoFile[dat2$theoFile[,1]%in%dat2$actLookUp$id,])
       
       if(n > 1) {
         for(i in 2:n) {
           dat2$theoFile[,i] <<- ymd(dat2$theoFile[,i])
           if(!is.Date(dat2$theoFile[,i])) {
             print("Not Date")
             # browser()
           }
         }
       }
       if(nrow(dat2$theoFile) > 0) {
         output$theoFileUI <- renderUI({
           fluidRow(
             column(width = 12,
                    br(),
                    selectInput("fourTheoFileSearchID", "Select ID:", choices = dat2$theoFile[,1]),
                    textInput("fourTheoFileID", "Type to Select:", value = dat2$theoFile[,1][1]),
                    column(width = 4,
                           actionButton("fourTheoFileGo", HTML("Plot")),
                    )
             )
           )
         })
       } else {
         output$theoFileUI <- renderUI({p("Could not find valid individuals.")})
       }
       print("File Uploaded")
     }
   }
 })
 
 observeEvent(input$fourTheoFileGo, {
   # Need to add a vertical line at dat2$theoFile[,4] (if exists)
   # And scale graph to (dat2$theoFile[,2:3])
   # times <- dat2$theoFile[dat2$theoFile[,1] == input$fourTheoFileID,2:3]
   if(input$fourTheoFileID %in% dat$attributes$id) {
     output$figureplotly <- renderPlotly(
       plotPatientActivityPlotly2(theoData(isolate(input$fourTheoFileID), dat, dat2)
                                  ,dat2,input$fourTheoDate))
   } else {
     output$figureplotly <- renderPlotly(plotly_empty())
     output$fourTheoSingleTheoAttributes <- renderText({"Selected individual not found in data."})
   }
   freezeReactiveValue(input, "fourTheoFileGo")
 })
 observeEvent(input$fourTheoFileSearchID, {
   updateTextInput(session, "fourTheoFileID", value = input$fourTheoFileSearchID)
   freezeReactiveValue(input, "fourTheoFileID")
 })
 
 # Messages for single theoplots
 observeEvent(input$fourTheoSingleInfo1, {
   showModal(
     modalDialog(
       title = "Using Theoplots",
       
       p("This page provides theoplots for individuals. For group/aggregated theoplots, click the 'Group Theoplot' tab above."),
       
       p("To get started, select the data range of interest (or leave as default). Historic data, beyond the initial date may be present. Then, use one of the tabs below to select an individual, and click plot."),
       p("The expandable tree to the right of this box shows the activity types available. Untick boxes to remove the corresponding activitise from the graph."),
       p("The Attributes section in the bottom right shows some additional information about the selected individual."),
       p("The Activity section in the bottom center shows the raw data being plotted. This feature may be removed in the final release."),
       
       p("Plots can be saved by clicking the camera icon when hovering over the graph. By using the tool, you agree to keep any and all screenshots and images confidential in",
         " line with IG rules."),
       br(),
       p("Please note pseudo-IDs have been removed from the graphs to ensure an anomymous output. Individuals fitting the criteria are randomly sampled from the population."),
       br(),
       hr(),
       p(strong("Plot tips:"), "Theoplots are interactive - select an area to zoom in,
                   or hover over activity to see if there is more information available.
                   Double click on plot to return to default plot. Explore the navigation
                   tools that appear at the top right of the plot."),
       p("Please note lines (activity with duration) does not have hovertext, and can be coloured wrong. To see details, find the end points, which will have the relevant information in the hovertext."),
       # p("To upload IDs."),
       footer = tagList(
         modalButton("OK", icon = NULL)
       ),
       easyClose =TRUE
     )
   )
 })
 
 
 #############################
 # Multiline Theoplots
 observeEvent(input$fourTheoMultiGlobalGroupGo, {
   plotMultiLineTheo(dat, dat2, input, 
                     dat$attributes %>%
                       filter(.data[[input$fourTheoMultiGlobalGroup]] %in% input$fourTheoMultiGlobalGroupValues) %>%
                       dplyr::sample_n(size = min(input$fourTheoMultiPatentNum, nrow(.))) %>%
                       dplyr::pull(id)
   )
 })
 observeEvent(input$fourTheoMultiGlobalGroup, ignoreInit = T, {
  # I mis-spelled values
   print("Valeus update")
   updatePickerInput(session, "fourTheoMultiGlobalGroupValues", choices = sort(unique(as.character(dat$attributes[[input$fourTheoMultiGlobalGroup]]))))
 })

 observeEvent(input$fourTheoFilterGoMulti, {
   plotMultiLineTheo(dat, dat2, input, 
                     dat$attributes %>%
                       filter(.data[[dat2$age]] <= input$fourTheoFilterAgeSliderMulti[2] & .data[[dat2$age]] >= input$fourTheoFilterAgeSliderMulti[1],
                              .data[[dat2$ltc]] >= input$fourTheoFilterLTCSliderMulti[1] & .data[[dat2$ltc]] <= input$fourTheoFilterLTCSliderMulti[2]) %>%
                       dplyr::sample_n(size = min(input$fourTheoMultiPatentNum, nrow(.))) %>%
                       dplyr::pull(id)
                     )
 })

 # Upload a file
 observeEvent(input$theoFileInputMulti, {
   ext <- tools::file_ext(input$theoFileInputMulti$datapath)
   # browser()
   if (ext == "csv") {
     dat2$theoFileMulti <<- read.csv(input$theoFileInputMulti$datapath,stringsAsFactors = F)
     # Need: 1st column to be integer, 2nd and 3rd date, 4th date?
     n <- ncol(dat2$theoFileMulti)
     if(n > 4 | n < 1) {
       output$theoFileUIMulti <- renderUI({p("Something went wrong while loading the file.")})
       # throw exception - unrecognised format
     } else {
       # Handle file
       # Assume columns contain correct data types
       dat2$theoFileMulti <<- as.data.frame(dat2$theoFileMulti[dat2$theoFileMulti[,1]%in%dat2$actLookUp$id,])
       if(n > 1) {
         for(i in 2:n) {
           dat2$theoFileMulti[,i] <<- ymd(dat2$theoFileMulti[,i])
           if(!is.Date(dat2$theoFileMulti[,i])) {
             print("Not Date")
           }
         }
       }
       if(nrow(dat2$theoFileMulti) > 0) {
         output$theoFileUIMulti <- renderUI({
           fluidRow(
             column(width = 12,
                    br(),
                    selectInput("fourTheoFileSearchIDMulti", "IDs uploaded:", choices = dat2$theoFileMulti[,1]),
                    column(width = 4,
                           actionButton("fourTheoFileGoMulti", HTML("Sample & Plot!")),
                    ),
             )
           )
         })
       } else {
         output$theoFileUIMulti <- renderUI({p("Could not find valid individuals.")})
       }
       print("File Uploaded")
     }
   }
 })
 
 observeEvent(input$fourTheoFileGoMulti, {
   plotMultiLineTheo(dat, dat2, input, 
                     sample(dat2$theoFileMulti[,1], min(length(dat2$theoFileMulti[,1]), input$fourTheoMultiPatentNum))
   )

   freezeReactiveValue(input, "fourTheoFileGo")
 })
 
 observeEvent(input$fourTheoMultiInfo1, {
   showModal(
     modalDialog(
       title = "Using Theoplots",
       
       p("This page provides group/aggregated theoplots. For individual theoplots, click the 'Single Theoplot' tab above."),
       
       p("To get started, select the data range of interest (or leave as default). Historic data, beyond the initial date may be present."),
       p("To select the activity type(s) to plot, please go to the 'Options' tab. Expand the tree as required, and select activity type(s) of interest.",
         " This will create a table, in which colours can be specified for each activity type. Please note the behaviour of this table can be odd at times."),
       p("Once activity is selected, use the tabs on the left to specifying how many individuals you would like sampled, and by what criteria."),
       p("The plotting itself may take a while if a large number of individuals are selected."),
       p("Select the 'Theoplot' tab to view the plot once it has rendered."),
       br(),
       p("Plots can be saved by clicking the camera icon when hovering over the graph. By using the tool, you agree to keep any and all screenshots and images confidential in",
         " line with IG rules."),
       br(),
       p("Group theoplots are designed to allow comparisons of patient journeys."),
       br(),
       p("Please note pseudo-IDs have been removed from the graphs to ensure an anomymous output. Individuals fitting the criteria are randomly sampled from the population."),
       br(),
       hr(),
       p(strong("Plot tips:"), "Theoplots are interactive - select an area to zoom in,
                   or hover over activity to see if there is more information available.
                   Double click on plot to return to default plot. Explore the navigation
                   tools that appear at the top right of the plot."),
       footer = tagList(
         modalButton("OK", icon = NULL)
       ),
       easyClose =TRUE
     )
   )
 })

 ###############################################################
 
 # Colour mapping for activity
 observeEvent(input$fourTheoMultiActivityToPlot, {
   selected <- getSelectedShinyTreeValues(dat2, input$fourTheoMultiActivityToPlot)
   ntree <- selected[[2]]
   treeCols <- selected[[3]]
   selected <- selected[[1]]
   
   if(length(treeCols) > 0) {
     # In short, grabs the list of entries to assign colours to
     # all the ancestor values are for tracking which are expanded in the tree, and which aren't
     t <- sapply(1:nrow(ntree), function(i) {
       removeScores(ntree[i,]) %>% paste0(collapse = ".")
     })
     
     allCols <- get_selected(input$fourTheoMultiActivityToPlot, format = "names") %>%
       lapply(function(x){
         paste0(c(attr(x, "ancestry"), x[1]), collapse = ".")
       })%>%
       unlist()
     isOpen <- lapply(get_selected(input$fourTheoMultiActivityToPlot, format = "names"), function(x){
       if(is.null(attr(x, "stopened"))) {
         F
       } else {
         T
       }
     })%>%unlist()
     
     hasNoAncestor <- lapply(get_selected(input$fourTheoMultiActivityToPlot, format = "names"), function(x) {
       if(length(attr(x, "ancestry")) > 0) {
         F
       } else {
         T
       }
     })%>%unlist()
     
     hasOpenAncestor <- lapply(get_selected(input$fourTheoMultiActivityToPlot, format = "names"), function(x) {
       y <- paste0(c(attr(x, "ancestry")), collapse = ".")
       if(y == "") {
         F
       } else {
         isOpen[which(allCols == y)]
       }
     }) %>% unlist()
     #      is closed AND (no ancestor OR has open ancestor) # closed = NOT open
     actualCols <- allCols[(!isOpen & hasNoAncestor) | (!isOpen & hasOpenAncestor)]
     
   }

   if(length(treeCols) > 0) {
     dat2$multiTheoColSNum <<- dat2$multiTheoColSNum+1
     dat2$multiTheoColS <<- actualCols
     
     selected <- gsub(" ", "_", actualCols, fixed = T) %>% gsub("_", ".",.) # selected
     cols <- c("green", "blue", "yellow", "black", "red", "grey")
     if(length(selected) == 1) {
       x <- data.frame(t(c(selected, 1:6))) %>% as.data.table()
     } else {
       x <- cbind(selected, sapply(1:length(cols), function(x) {rep(x,length(selected))})) %>% as.data.table()
     }
     
     colnames(x) <- c("Activity", cols)

     x <- x %>% mutate(check = sapply(x$Activity%>%as.character, function(y) {
       y <- paste0(y, paste0(rep("a",dat2$multiTheoColSNum-1), collapse = ""))
       if(!is.null(input[[y]])) {input[[y]]} else {4}
     })
     )
     # Format names of table entries, and map in radio buttons
     x <- cbind(data.frame(Activity_Name = x$Activity),x) %>% mutate(Activity_Name = gsub(".", "<br>", Activity_Name, fixed = T),
                                                                     Activity = paste0(Activity, paste0(rep("a",dat2$multiTheoColSNum), collapse = ""))
     ) %>%
       mutate(across(cols, function(x) {sprintf('<input type="radio" name="%s" value="%s" %s/>', Activity, `x`, ifelse(`x`==`check`,"checked" ,""))}) )
     output$fourTheoMultiActivityToPlotColours <- renderUI({
       column(width = 12, fluidRow(style = "overflow-x: scroll;",
                                   DT::dataTableOutput('multiTheoColours'),
                                   verbatimTextOutput('sel')
       )
       )
     })
     
     x <- as.data.table(x)
     # Remove field used to select which entry is selected i neach row, then render the table
     x[ ,`:=`(check = NULL)]
     output$multiTheoColours = DT::renderDataTable(
       x, escape = FALSE, selection = 'none', server = FALSE, rownames=FALSE,
       options = list(dom = 't', paging = FALSE, ordering = FALSE, columnDefs = list(list(visible=FALSE, targets=c(1)))), # options=list()
       callback = JS("table.rows().every(function(i, tab, row) {
                    var $this = $(this.node());
                    $this.attr('id', this.data()[1]);
                    $this.addClass('shiny-input-radiogroup');
  });
                    Shiny.unbindAll(table.table().node());
                    Shiny.bindAll(table.table().node());")
     )
     # browser()
   }
 })

 
  ###############################
  #### Risk Strat / Regression(s)
  
  # 5.1
  observeEvent(input$fiveRisk1go, {
    source("./functions/section5Tools.R")
    # Obtain selected activity from shinyTree

    selected <- getSelectedShinyTreeValues(dat2, input$fiveRiskTargetTree)
    ntree <- selected[[2]]
    treeCols <- selected[[3]]
    selected <- selected[[1]]

    if(!is.null(input$twoRisk1Var1) & nrow(ntree[treeCols,]) > 0) {
      throwModal("Loading activity data")
        # Train logit on entire pop -> predict activity chance on them -> select those who are "yes" to then train linear regression
        
        ## For now, hurdle model but use both steps: find individuals with activity, then predict their predicted cost
        # Logistic regression
        # Either on activity or cost
        ################
      # browser()
        if(input$twoRisk1Var2 == "Expected cost (Â£s)") {
          print("Cost")
          act_data <- readRDS("./data/act_.rds") %>% group_by_at(c("id", dat2$actPodCols)) %>%
            summarise(act = sum(cost)) %>%
            pivot_wider(names_from = dat2$actPodCols, values_from = c("act"), values_fill = 0, names_sep = ".") %>%
            select_at(c("id", selected[selected%in%colnames(.)]))
        } else {
          act_data <- readRDS("./data/act_.rds") %>% group_by_at(c("id", dat2$actPodCols)) %>%
            summarise(act = n()) %>%
            pivot_wider(names_from = dat2$actPodCols, values_from =c("act"), values_fill = 0, names_sep = ".") %>%
            select_at(c("id", selected[selected%in%colnames(.)]))
        }
        showModal(
          modalDialog(
            h2("Formatting activity data"),
            footer = list(),
            easyClose = F
          )
        )
        act_data <- data.frame(id = act_data$id, act = rowSums(act_data[,-1]))
        # Merge to attributes
        act_data <- merge(dat$attributes[,c("id",input$twoRisk1Var1)], act_data, all.x = T)
        act_data$act <- act_data$act %>% sapply(function(x) { if(is.na(x)) {0} else {x}})

        add_sco <- function(x){
          gsub(" ", "_", x, fixed = T)
        }
        colnames(act_data) <- sapply(colnames(act_data),add_sco)
        
        NAcount <- lapply(act_data, function(x) {
          length(which(is.na(x)))/length(x) > 0.1
        })%>%unlist
        act_data<-act_data[,!NAcount]
        excluded_for_NAs <- act_data$id[!act_data$id%in%(na.omit(act_data)$id)]
        act_data<-act_data[!act_data$id%in%excluded_for_NAs,]
        act_data_backup <- act_data

        output$fiveRisk1Out0 <- renderUI({
          list("NAs in data are excluded.",
            br(),
            if(length((colnames(act_data)[NAcount] > 0))) {
              paste0("Columns ", paste0(colnames(act_data)[NAcount]%>%sapply(removePrefix) %>% sapply(makeNice), collapse = ", "), " have been excluded for containing too many NAs.")
            } else {
              paste0("No columns have been excluded")
            },
            br(),
            paste0(length(excluded_for_NAs)," individuals cannot be predicted for as profile contains NAs."),
            br(),
            br()#,
            # h3("Logistic Regression")
          )
        })
        
        # Modify primary data to be binary - for logistic regression
        act_data <- act_data %>% mutate(act = case_when(act >= 1~1, T ~ 0))
        gc()
        # Lumping of factor or chr data - do if >10 unique values
        for(i in input$twoRisk1Var1) {
          if(!is.numeric(act_data[[i]])) {
            if(length(unique(act_data[[i]])) > 10) {
              act_data[[i]] <- fct_lump_lowfreq(act_data[[i]])
            }
          }
        }
        showModal(
          modalDialog(
            h2("Obtaining training data"),
            footer = list(),
            easyClose = F
          )
        )
        gc()
        # Training and testing data [70% train]
        get_training_data <- function(data) {
          data$id%in% (data %>% group_by(across(!is.numeric)) %>% slice(sample(1:n(), ceiling(0.7*n())) ))$id
        }
        train <- get_training_data(act_data)
        gc()
        
        # Trim out cols with only 1 value ; further, note this assumes no NAs will trim data to small enough that unique values are lost
        colU <- lapply(na.omit(act_data[train,]), function(x) {length(unique(x))}) %>% unlist
        
        # Run logistic regression
        showModal(
          modalDialog(
            h2("Running logistic regression"),
            footer = list(),
            easyClose = F
          )
        )
        print(Sys.time())
        glmModel <- glm(as.formula(paste0("`","act","`", "~", "`", ".", "`"))
                        , family = "binomial", data = act_data[train,colU > 1],
                        na.action = na.omit)
        print(Sys.time())
        
        showModal(
          modalDialog(
            h2("Testing logistic regression"),
            footer = list(),
            easyClose = F
          )
        )
        # Test model - once I have a way of printing important numbers, use same to print
        predicted <- predict(glmModel, act_data[!train,], type="response")
        acc <- length(which(act_data[!train,"act"] == 1*(predicted>input$fiveRiskMinProb)))/length(which(!train))
        
        
        showModal(
          modalDialog(
            h2("Obtaining logistic regression predicted values"),
            footer = list(),
            easyClose = F
          )
        )
        predicted <- predict(glmModel, act_data, type="response")
        
        output$fiveRisk1Out1 <- renderUI({
          list(
            HTML("<b>Logistic regression executed. Summary:</b>"),
            br(),
            paste0("Testing Accuracy: ", paste0(round(acc*100), "%")),
            br(),
            column(width = 6,fluidRow(
                   paste0("Identified ", length(which(predicted >= input$fiveRiskMinProb)), " individuals with a minimum probability of ", input$fiveRiskMinProb)
            )),
            column(width = 6,
                   downloadButton("fiveRiskDownload1", "Download Logistic Regression Model"),
            )
          )
        })
        
        output$fiveRisk1DownloadUI <- renderUI({
          list(
            # downloadButton("fiveRiskDownload1", "Download Logistic Regression Model"),
            uiOutput("fiveRisk1DownloadUI2")
          )
        })
        glmModel$model <- NULL
        glmModel$data <- NULL
        glmModel$qr$qr <- NULL
        glmModel$effects <- NULL
        glmModel$residuals <- NULL
        glmModel$fitted.values <- NULL
        glmModel$linear.predictors <- NULL
        glmModel$weights <- NULL
        glmModel$prior.weights <- NULL
        glmModel$y <- NULL
        
        dat2$riskLogModel <<- glmModel
        
        ######
        # ggplotly(ggplot(dat2$act_data, aes(x = demog.Age, group = demog.Age))+ geom_bar()+facet_wrap("~act"))
        # ggplotly(
        #   ggplot(dat2$act_data_backup, aes(x = demog.Age, y = sum(act), group = demog.Age))+ geom_col()+facet_wrap("~act")
        #   )
        #####
        # browser()
        # roc_table$false <- predict(glmModel, act_data[!train,], type="response")
        # roc_table$predicted <- predict(glmModel, act_data[!train,], type="response")
        # roc_table$predicted <- predict(glmModel, act_data[!train,], type="response")
        
        actuals <- act_data[!train,"act"] == case_when(predict(glmModel, act_data[!train,], type="response") > input$fiveRiskMinProb ~ 1,
                                                       T ~ 0)
        scores <- predict(glmModel, act_data[!train,], type="response")
        actuals <- actuals[order(scores)]
        
        # Want when scores == 0.5 which(scores[order(scores)] > input$fiveRiskMinProb)[1]
        # Point is at (specificity, Sensitivity) = TN/N, TP/P
        p <- which(scores[order(scores)] > input$fiveRiskMinProb)[1]
        
        sens <- (sum(actuals) - cumsum(actuals))/sum(actuals)
        spec <- cumsum(!actuals)/sum(!actuals)
        (auc <- sum(spec*diff(c(0, 1 - sens))))
        #  Sensitivity is TP/P and specificity is TN/N
        dPlot <- data.frame(Specificity = 1 - spec, Sensitivity = sens)
        ### Note: I can probably remove 90% of points to increase running speed for this graph
        if(nrow(dPlot) > 100000) {
          dPlot <- dPlot[c(1,(1:floor(nrow(dPlot)/10))*10,nrow(dPlot)),]
        }
        ############
        bp <- ggplot(dPlot, aes(Specificity, Sensitivity)) + geom_line() + 
          geom_abline(intercept = 0, slope = 1, color="green" ) + 
          ggtitle(paste0("ROC curve | AUC = ", round(auc, 2))) +
          geom_point(aes(x = 1-spec[p], y = sens[p], color = "Selected threshold")) +
          # labs(color="Selected threshold") +
          guides(color=guide_legend(title=NULL)) +
          scale_color_discrete(breaks=c("Selected threshold"))
        
        # ggplotly(bp) %>% layout()
        
        output$fiveRiskBox11 <- renderPlotly({
          ggplotly(bp)
        })
        
        output$fiveRiskBox12 <- renderPlot({
          dPlot <- data.frame(Predicted = predict(glmModel, act_data[!train,], type="response"), Testing = log(1+act_data_backup$act[!train]))
          bp <- ggplot(dPlot, aes(Testing, Predicted)) + geom_point() + xlab("Testing Data (scaled by log(1+p))") + ylab("Predicted Data")+
            geom_hline(yintercept = input$fiveRiskMinProb, color = "red") +
            ggtitle(paste0("Logistic regression test results"))
          bp
        })
        
        #####
        if(input$twoRisk1Var2 == "Likelihood of activity [0,1]") {
          showModal(
            modalDialog(
              h2("Graphing Results"),
              footer = list(),
              easyClose = F
            )
          )
          dat2$riskLinearModel <<- NULL
          dat2$riskTable <<- data.frame(id = act_data$id, predicted_act = predicted, predicted_act_group = ntile(predicted,10))
          dat$attributes$util.risk_strat <<- util_risk_strat(dat, data.frame(id = act_data$id, predicted_act = predicted))

        } else {
          print("Moving on to linear regression")
          output$fiveRiskBox2 <-renderUI({
            fluidRow(
              column(width = 12,
                     h3("Linear Regression")
              ),
              column(width = 6,
                     plotOutput("fiveRiskBox21")
              ),
              column(width = 6,
                     plotOutput("fiveRiskBox22")
              )
            )
          })
          
          # Having obtained predictions, find people with activity
          train <- get_training_data(((act_data[((predicted>=input$fiveRiskMinProb)),])))
          
          colU <- na.omit((act_data[((predicted>=input$fiveRiskMinProb)),])[train,]) %>% lapply( function(x) {length(unique(x))}) %>% unlist
          showModal(
            modalDialog(
              h2("Performing linear regression"),
              footer = list(),
              easyClose = F
            )
          )
          # Pulling in the backup
          act_data$act <- act_data_backup$act
          print("Starting regression")
          try({
            # Linear Regression Model
            ### Missing factor levels - ensure predicted>input$fiveRiskMinProb STILL contains all factor levels - manually add in any missing?
            # if a level is missing -> not expected to produce non-zero results - add in with 0 target?
            for(i in colnames((act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[train,])) {
              if(!is.numeric(act_data[[i]])) {
                # Check for missing levels
                if(length(which(!(unique((act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[train,i])%in%act_data[[i]]))) > 0) {
                  # Add in missing levels
                }
              }
            }
            lmModel <- lm(as.formula(paste0("`","act","`", "~", "`", ".", "`")),
                          data = (act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[train,]
            )
          })
          showModal(
            modalDialog(
              h2("Linear regression predictions"),
              footer = list(),
              easyClose = F
            )
          )
          print("Starting prediction")
          out <- tryCatch(
            {
              lmPred <- predict.lm(lmModel, (act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[!train,])
            },
            error=function(cond) {
              print("Crash")
              # browser()
            })
          t <- data.frame(y = lmPred, x = (act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[!train,"act"])
          
          plot <- ggplot(t[sample(1:nrow(t), ceiling(nrow(t)/100)),],aes(x,y))+ geom_point(color='blue')+
            ylab("Predicted (fitted) values")+
            xlab("Training values") +
            geom_smooth(method = "lm", se = FALSE, color = 'red') +
            ggtitle("Model Output")+
            ylim(0, quantile(lmPred, 0.99)
            )+
            xlim(0,quantile((act_data[((predicted>=input$fiveRiskMinProb)),colU > 1])[!train,"act"], 0.99))
          # geom_smooth(color='red',data = t[sample(1:nrow(t), ceiling(nrow(t)/100)),], aes(x=x, y=y))
          output$fiveRiskBox21 <- renderPlot({
            plot
          })

          me <- round(sum(abs((act_data$act[((predicted>=input$fiveRiskMinProb))])[!train]-lmPred))/length(act_data$act))
          ars <- round(summary(lmModel)$adj.r.squared, 2)
          print("Starting linear regression prediction 2")
          try({
            lmPred <- predict.lm(lmModel, act_data)
          })
          lmPred <- tryCatch(
            {
              predict.lm(lmModel, act_data)
            },
            error=function(cond) {
              print("Failed to lm, mssing levels - defaulting to 0")
              lmPred <- rep(0, nrow(act_data))
              lmPred[predicted>=input$fiveRiskMinProb] <- predict.lm(lmModel, act_data[predicted>=input$fiveRiskMinProb,])
              return(lmPred)
            })
          
          act_data$predicted_act <- predicted * lmPred
          act_data$predicted_act[predicted<input$fiveRiskMinProb] <- 0
          
          # predicted>input$fiveRiskMinProb -> set to 0
          # Expected costs
          act_data$predicted_act <- lapply(act_data$predicted_act, function(x) { if(!is.na(x)){if (x < 0) {0} else {x}} })%>%unlist
          
          output$fiveRisk1Out2 <- renderUI({
            list(
              HTML("<b>Linear regression executed. Summary:</b>"),
              br(),
              paste0("Mean absolute error: ",me),
              br(),
              paste0("Adjusted R Square: ", ars ),
              br(),
              br()
            )
          })
          
          output$fiveRisk1DownloadUI2 <- renderUI({
            list(
              downloadButton("fiveRiskDownload2", "Download Linear Regression Model")
            )
          })
          lmModel$model <- NULL
          lmModel$data <- NULL
          lmModel$qr$qr <- NULL
          lmModel$effects <- NULL
          lmModel$residuals <- NULL
          lmModel$fitted.values <- NULL
          # Notes on this: can't run summary or plot -> basically not an lm object -> only export the coeffients?

          dat2$riskLinearModel <<- lmModel
          
          lm_mod$lm_obj <- lmModel
          
          plotData <- data.frame(y = c(act_data$predicted_act, act_data$act), x = c(rep("Predicted",nrow(act_data)),rep("Original",nrow(act_data))))
          plotData <- plotData[sample(1:nrow(plotData), ceiling(nrow(plotData)*(100/100))),]
          dat2$riskTable <<- act_data%>% select(c(id, predicted_act)) %>% mutate(predicted_act_group = ntile(predicted_act,10))
          dat$attributes$util.risk_strat <<- util_risk_strat(dat, act_data)
          
          showModal(
            modalDialog(
              h2("Graphing Results"),
              footer = list(),
              easyClose = F
            )
          )
        }

        listText = "Risk Strat"
        listCol = "util.risk_strat"
        
        if(listText%in%dat2$groupByListText) {
          i <- which(dat2$groupByListText == listText)
        } else {
          i <- length(dat2$groupByListText) + 1
        }
        dat2$groupByListText[[i]] <<- listText
        dat2$groupByList[[i]] <<- listCol
        
        updateAllWithNewImportantField(dat)
        
        output$fiveRiskBox4 <- renderUI({
          fluidRow(
            column(width = 6,
                   br(),
                   br(),
                   h3(paste0("Summary statistics of ", input$twoRisk1Var2)),
                   tableOutput("fiveRiskplot22Table")
            ),
            column(width = 6,
                   br(),
                   br(),
                   plotlyOutput("fiveRiskplot22")   
            )
          )
        })

        output$fiveRiskplot22 <- renderPlotly({
          riskStratGroupSummaryBoxPlots(dat2$riskTable, input)
        })
        output$fiveRiskplot22Table <- renderTable({
          df <- as.data.frame(dat2$riskTable%>%group_by(predicted_act_group)%>%summarise_at("predicted_act", .funs = c(min, max, mean, median, IQR)))
          colnames(df) <- c("Group", c("Min", "Max", "Mean", "Median", "IQR"))
          df
        })
        print("Rendered plot")
        
        removeModal()
    }
  })
  
  observeEvent(input$fiveRiskLogScale,{
    print(str(dat2$riskTable))
    if(!is.null(dat2$riskTable)) {
      if(is.null(dat2$riskLinearModel)) {
        # Do nothing
        box_plot <- ggplot(dat2$riskTable, aes(x = x, y = y),stat = "identity") +
          geom_boxplot() + ylab(input$twoRisk1Var2) +xlab("") +# using log(1+p)
          ggtitle("Comparison of predicted vs training data distribution") + scale_y_continuous(labels=scales::label_number_si())
      } else {
        box_plot <- ggplot(dat2$riskTable, aes(x = x, y = y),stat = "identity") +
          geom_boxplot() + ylab(input$twoRisk1Var2) +xlab("") +# using log(1+p)
          ggtitle("Comparison of predicted vs training data distribution") + scale_y_continuous(labels=scales::label_number_si())
        if(input$fiveRiskLogScale) {
          box_plot <- box_plot + scale_y_continuous(trans = "log1p",
                                                    labels=scales::label_number_si()) + ylab(paste0(input$twoRisk1Var2))
        }
      }
      output$fiveRiskBox22 <- renderPlotly({        
        ggplotly(box_plot)
      })
    }
  })
  
  observeEvent(input$fiveRisk1InfoD1, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Risk Stratification</strong></h2>"),
        p("This page stratifies the population by the selected measure into 10 distinct groups."),
        
        HTML("<strong>To get started:</strong>"),
        p("Use the first dropdown to select attribute(s) to use during the stratification."),
        p("Then, select the prediction target. There are 3 options: the probability of having activity of the selected type, 
          the expect number of activity instances, or the expected costs. The activity types can be selected from the tree at the right (the tree can be expanded to show further POD levels)."),
        
        p("The final step is to decide on a cut-off value in range [0, 1], then click 'Go!'"),
        p("For more details, see Technical details."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  observeEvent(input$fiveRisk1InfoD2, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Technical Details</strong></h2>"),
        
        p("First, the selected activity is summarised (either as activity instance count, or cost, depending on the option selected). Then, a logistic regression model is trained",
          " on a subset, before being used to predict, for the entire population, the probability that an individual will have activity based on their attributes data.",
          "This returns a probability in the range [0 ,1]. All patients with a probability greater than or equal the cut-off value are given a positive prediction; all others a negative.",
          ),
        
        p("If the 'Likelyhood of activity' option is selected, the ExploreR creates the stratified groups based on the returned probabilities, and terminates.",
          "If one of the other options is selected, then a linear regression model is trained on the subset predicted to have some activity/cost (based on the output of the first step). ",
          "This is done to minimise the number of negative values predicted by the linear model.",),
        
        p("Finally, the linear model is used to predict the expected activity/costs, which are then multiplied by the probability from the logistic model to obtain the expected",
          "activity/cost for each individual. Please note linear regression can give rise to negative predictions, which are replaced with 0s for data consistency."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  observeEvent(input$fiveRisk1InfoD3, {
    showModal(
      modalDialog(
        HTML("<h2><strong>Understanding the Outputs</strong></h2>"),
        
        p("A number of summary measures and graphs are produced as outputs (as well as the stratified groups):"),
        p("1. A summary of how many NAs were found, and subsequently excluded."),
        p("2. Summary statistics of the logistic (and if run) the linear regression performance.",
          "For the logistic regression this is the testing accuracy, and for the linear regression it's the mean absolute error, and the (adjusted) R square"),
        p("3. Graphs for the logistic regression are an ", a(href = "https://acutecaretesting.org/en/articles/roc-curves-what-are-they-and-how-are-they-used", target = "_blank", "ROC curve"),
          ", and a plot of testing data (scaled by log(1+p)) against predicted probability."
        ),
        p("4. If run, the linear regression will also have a similar graph, of fitted values against original data. Only a subset of the data is plotted (in blue), with the red line",
          " of best fit."),
        p("5. Summaries of the stratified groups. Includes a table of a key measures, and a boxplot for easier comparison."),
        
        footer = tagList(
          modalButton("OK", icon = NULL)
        ),
        easyClose = TRUE
      )
    )
  })
  
  output$fiveRiskDownload1 <- downloadHandler(
    filename = function() {
      paste("LogisticModel", ".rds", sep = "")
    },
    content = function(file) {
      print(str(dat2$riskLogModel))
      print(object.size(dat2$riskLogModel))
      print(file)
      saveRDS(dat2$riskLogModel, file)
    }
  )
  output$fiveRiskDownload2 <- downloadHandler(
    filename = function() {
      paste("LinearModel", ".rds", sep = "")
    },
    content = function(file) {
      print(str(dat2$riskLinearModel))
      print(object.size(dat2$riskLinearModel))
      print(file)
      saveRDS(dat2$riskLinearModel, file)
    }
  )
  
  ## Need to:
  # Update all group_bys
  # Some seg/risk strat select options
  updateAllWithNewImportantField <- function(dat) {
    # Assume for cohort user has to go back and re-select "highlighted variables" option
    # Update section 1 options

    updateSelectInput(session, "oneActVertical", choices = dat2$groupByListText[!is.na(dat2$groupByList)], selected = input$oneActVertical)
    updateSectionOneGroupBy(session, dat2)

    # Update segmentation: CART, Clustering, Risk options
    t <- dat2$groupByList[!is.na(dat2$groupByList)]
    names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
    
    t2 <- c("total_cost", "total_act")
    names(t2) <- sapply(c("total_cost", "total_act"),makeNicePPY)
    
    cm <- dat$attributes%>%select(starts_with("clinic.misc")) %>% colnames
    names(cm) <- sapply(cm, removePrefix) %>% sapply(makeNice)
    
    segColNames2 <- list(
      dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
      c(cm, dat2$clinicCols),
      dat2$areaCols,
      dat2$socioCols,
      t
    )
    segColNames <- c(
      dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
      c(cm, dat2$clinicCols),
      dat2$areaCols,
      dat2$socioCols,
      t
    )
    names(segColNames2) <- c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "GlobalGroups")
    
    updatePickerInput(session, "twoCARTVar1", choices = segColNames2,
                      selected = input$twoCARTVar1,
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable"))

    updatePickerInput(session, "navTabGlobalVar", choices = segColNames2, selected = input$navTabGlobalVar)
    updatePickerInput(session, "navTabCurrentGlobalVar", choices = t)
    
    updatePickerInput(session, "twoClusterVar1", choices = segColNames2, selected = input$twoClusterVar1)
    updatePickerInput(session, "twoClusterVar2", choices = segColNames2, selected = input$twoClusterVar2)
    
    
    segColNames3 <- segColNames2
    
    segColNames3[[length(segColNames3)+1]] <- t2
    names(segColNames3)[length(segColNames3)] <- "Total Activity/Cost"
    updatePickerInput(session, "twoClusterVars", choices = segColNames3, 
                      selected = input$twoClusterVars,#c(segColNames,t2),
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable"))
    
    updatePickerInput(session, "twoRisk1Var1", choices = segColNames2,
                      selected = input$twoRisk1Var1,#segColNames,
                      options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable"))
    
    updatePickerInput(session, "fourTheoSingleGlobalGroup", choices = t, selected = input$fourTheoSingleGlobalGroup)
    updatePickerInput(session, "fourTheoMultiGlobalGroup", choices = t, selected = input$fourTheoMultiGlobalGroup)
    
  }
  
}

shinyApp(ui = ui, server = server, options = list(shiny.maxRequestSize = 500*1024^10))


