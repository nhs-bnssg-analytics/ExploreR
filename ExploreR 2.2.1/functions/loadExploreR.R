loadExploreR <- function(data, dat, input, output) {
  dat2$yChoices <<- c("Population (Percentage)",
          "Population (Count)",
          "Spend (Total, £s)",
          "Spend (Per Capita, £s)",
          "Activity (Total)",
          "Activity (Per Capita)")

  maxDate2 <- ymd(dat2$maxtime) - years(1)
  minDate2 <- ymd(dat2$mintime) - years(1)
  if(is.na(maxDate2)) {
    maxDate2 <- as.POSIXlt(as.Date(dat2$maxtime)) # "2021/02/29"
    maxDate2$year <- maxDate2$year-1
    maxDate2 <- as.Date(maxDate2)
  }
  if(is.na(minDate2)) {
    minDate2 <- as.POSIXlt(as.Date(dat2$maxtime)) # "2021/02/29"
    minDate2$year <- minDate2$year-1
    minDate2 <- as.Date(minDate2)
  }
  
  for(i in 0:17) {
    output[[paste0("oneCoverBoxs",i)]] <- renderUI({h4(tags$b("Analysis Dataset: "), makeNice(input$zeroAnalysisDataPicker))})
    output[[paste0("oneCoverBoxDate",i)]] <- renderUI({h4(paste0("Activity from ", minDate2, " to ", maxDate2))})
  }

  # Load page 1.0
  #########
  # 1.0.1 Population pyramid
  
  demChoices <- c(dat2$clinicCols)
  names(demChoices) <- makeNice(demChoices %>% sapply(function(x) {removePrefix(x) %>% removePrefix()}) %>% unlist())
  updateSelectizeInput(session, "oneDemogClinNeed", choices = demChoices,# (dat2$ltcCols, dat2$demogCols)
                       options = list(
                         placeholder = 'Please select a condition (optional)',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  
  population_pyramid <- function(input_dat){
    colnames(input_dat)[1:2] <- c("demog.age", "demog.sex")
    # input_dat <- input_dat[order(demog.sex, decreasing = T),]
    measure_axis <- sort(seq(from=-ceiling(max(abs(input_dat$freq))),
                             to=ceiling(max(abs(input_dat$freq))),
                             length.out=11))
    measure_axis_labels <- ceiling(abs(measure_axis))
    # input_dat <- input_dat[order(input_dat$demog.sex, decreasing = T),]
    p <- plot_ly(input_dat,
                 y = ~ demog.age,
                 x = ~ freq,    
                 color = ~ demog.sex,
                 legendgroup= ~ demog.sex,
                 orientation = "h",
                 # sort = F,
                 text =  ~paste0("There are ", text_freq, " ", demog.age , "-year-olds ", demog.sex, "s <br>"),
                 hoverinfo="text") %>% 
      add_bars(opacity=0.5,
               width=1) %>% 
      layout(
        xaxis=list(tickvals=measure_axis,
                   ticktext=measure_axis_labels,
                   title="Raw Count of Population"),
        yaxis=list(title="Age"),
        legend=list(orientation="h",
                    x = 0.0, y = 1))
    return(p)
  }
  # browser()
  output$pyramid <- renderPlotly({
    population_pyramid(
      dat$attributes %>% 
        select_at(c(dat2$age, dat2$sex)) %>% 
        subset(.[[dat2$sex]] %in% c("male", "female")) %>% 
        group_by_at(c(dat2$age, dat2$sex)) %>% 
        count(name = "freq") %>% 
        mutate(freq=ifelse(!!sym(dat2$sex) == "male", yes = freq, no = -freq)) %>% 
        ungroup() %>% 
        mutate(text_freq=ifelse(abs(freq)<=4,"*suppressed",trimws(format(abs(freq),big.mark = ","),"both"))
        )
    )
  })
  
  # 1.0.2 Clinical Graph
  
  output$pyramidClinical <- renderPlotly({clinicPlotSolve(dat, dat$attributes[,"util.ltc_sum"] %>% as.data.frame(),# %>% as.character()
                                                             "Population (Percentage)",
                                                             NA,
                                                          "Number of LTCs",
                                                             title = NA)})
  # browser()
  # 1.0.3 Activity Graph
  # 1.0.4 Deprivation Graph
  # 1.0.5 Area Graph
  # 1.0.6 Determinants Graph
  # Load page 1.1 (if applicable?)
  #########
  
  # Load page 1.2 (if applicable?)
  #########
  
  updateSelectInput(session, "oneClinY", choices = dat2$yChoices)
  updateSelectizeInput(session, "oneClinMulti", choices = clinOptions(dat2$ltcCols),# dat2$ltcCols, 
                       selected = dat2$ltcCols[1:2],
                       options = list(
                         placeholder = 'Please select at least one option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))
  
  # Load page 1.3 (activity) (if applicable? - ahould always be)
  #########
  updateSelectInput(session, "oneActVertical", choices = dat2$groupByListText[!is.na(dat2$groupByList)])
  
  optList <- makeNice(c(unique(dat2$theorows$pod_l1), "Total"))
  names(optList) <- makeNice(optList)
  updateCheckboxGroupInput(session, "oneActX", choices = optList, selected = optList, inline = TRUE)


  

  
  output$oneActGraph2 <- renderPlotly({
    #####
    actGraph2 <- dat$attributes %>%
      select_at("total_cost") %>% 
      arrange_at("total_cost") %>%
      mutate(.,cost_cumsum_prop = cumsum(total_cost) / sum(total_cost),
             pop_prop = row_number() / nrow(.))
    # Need a smart way of dropping a couple hundred thousand rows
    while(nrow(actGraph2) > 2000) {
      toKeep <- c()
      i = 1
      while(i < nrow(actGraph2)) {
        q <- which(actGraph2$cost_cumsum_prop < actGraph2$cost_cumsum_prop[i] + 0.01 & actGraph2$cost_cumsum_prop > actGraph2$cost_cumsum_prop[i])
        if(length(q) < 1) {
          toKeep <- c(toKeep, i:nrow(actGraph2))
          break()
        }
        s <- sample((1:length(q))+i, ceiling(length(q)/10))
        toKeep <- c(toKeep, s)
        i <- i + length(q)
      }
      actGraph2 <- actGraph2[unique(toKeep),]
    }
    actGraph2$text <- paste(round(actGraph2$pop_prop * 100, digits = 1),"% of the population uses ", "\n",
                                 round(actGraph2$cost_cumsum_prop * 100, digits = 1), "% of spending",
                                 sep = "")
    actGraph2 %>% arrange(cost_cumsum_prop, pop_prop) %>% 
      plot_ly(., x = ~(pop_prop*100), y = ~(cost_cumsum_prop*100), name = 'Lorenz Curve', type = 'scatter', mode = 'lines', text = .[,"text"], hoverinfo = "text") %>% 
      layout(
             xaxis = list(title = 'Proportion of population (%)'),
             yaxis = list(title = 'Prop. of spending (%)')) %>%
    add_segments(x = 0, xend = 100, y = 0, yend = 100, name = 'Reference Line')
  
  })

  
  output$pyramidActivity <- renderPlotly({
    ggplotly(ggplot(as.data.frame(
      # dat2$summaryPageGraphs[[3]]
      {print(Sys.time())
        selectedGroup <- if(!is.na(dat2$area)) {dat2$area} else {"util.ltc_sum"}
      acts <- unique(dat2$theorows$pod_l1)
      
      sumn <- function(x) {
        n <- length(x)
        if(n == 0) {return(0)} else {return(sum(x)/n)} # total/ number of people in part
      }
      groupAndSum <- function(data, selectedGroup, actOrCost = ".cost", f) {
        # This needs a new function, passing cost+activity in pairs for scaling.
        data <- data %>% group_by_at(selectedGroup) %>% summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), f) %>%
          pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), names_to = "pod_l1") %>%
          mutate(pod_l1 = 
                   sapply(pod_l1, function(y) {
                     s <- strsplit(y, ".", fixed = TRUE)[[1]]
                     s <- s[c(-1,-2,-length(s))]
                     s
                   })
          )
        data<-data[,c("pod_l1",selectedGroup,colnames(data)[3])]
        colnames(data)[3]<-"cost"
        data
      }
      
      data <- groupAndSum(
        dat$attributes %>% select_at(c("id", lapply(acts, function(x) {c(paste0("util.pod_l1.", x, ".cost"), paste0("util.pod_l1.", x, ".act"))}) %>% unlist(), selectedGroup)) %>% as.data.frame()
        , selectedGroup, actOrCost = ".cost", sum)
      
      # Insert Total
      newdat <- data %>% group_by_at(selectedGroup) %>% summarise(total = sum(cost)) %>% ungroup() %>% as.data.frame()
      newdat <- cbind(rep("Total", nrow(newdat)), newdat)
      colnames(newdat) <- colnames(data)
      data <- rbind(newdat, data %>% as.data.frame())
      data[,"pod_l1"] <- (data[,"pod_l1"] %>% as.data.frame())[,1] %>% as.character() %>% makeNice()
      data$text <-
        paste("Cost is ", format(round(data[,"cost"]), big.mark = ","), "\n",
              "At ", data[,selectedGroup], "\n",
              "And POD ", data[,"pod_l1"], "\n",
              sep = ""
        )
      data}
      ), aes_string(x="pod_l1", 
                                                                           y=if(!is.na(dat2$area)) {dat2$area} else {"util.ltc_sum"}, 
                                                                           size= "cost", 
                                                                           fill = "cost",
                                                                           color = "cost",
                                                                           text = "text")) +
               geom_point(shape=21) + 
               ylab("") + xlab("Point of Delivery (POD)") +
               guides(fill = FALSE) +
               scale_color_continuous(labels = scales::label_number_si())+
               theme(axis.text.x = element_text(size = 8, angle = 45),
                     legend.text = element_text(size = 6),
                     axis.text.y = element_text(size = 8, angle = 45, hjust = -1)
                     )
    ,tooltip = "text")
  })
  
  
  # print("Dep")
  
  # Load page 1.4 (deprivation) (if applicable?)
  #########
  if(toLoad$dep) {
    output$uipyramidDeprivation <- renderUI({
      plotlyOutput("pyramidDeprivation")
    })
    output$uioneDeprivationPlot1 <- renderUI({
      plotlyOutput("oneDeprivationPlot1")
    })

    print("Loading base deprivation")

    output$pyramidDeprivation <- renderPlotly({clinicPlotSolve(dat, dat$attributes[,dat2$dep] %>% as.data.frame(),# %>% as.character()
                                                               "Population (Percentage)",
                                                               NA,
                                                               makeNice(paste0(dat2$dep," (1 most deprived, 10 least)")),
                                                               title = NA)})
    updateSelectInput(session, "oneDeprivationY", choices = dat2$yChoices)
  } else {
    output$uipyramidDeprivation <- renderUI({
      "Missing Deprivation data field. Unable to load this plot."
    })
    output$uioneDeprivationPlot1 <- renderUI({
      "Missing Deprivation data field. Unable to load this page."
    })
  }
  # Load page 1.5 (geography) (if applicable?)
  #########
  if(toLoad$geo) {
    updateSelectInput(session, "oneGeoY", choices = dat2$yChoices)
    
    updateSelectInput(session, "oneGeoX", choices = dat2$areaCols, selected = dat2$area[1]) #geoXvalues

    output$uipyramidGeography <- renderUI({
      plotlyOutput("pyramidGeography")
    })
    output$uioneGeoPlot1 <- renderUI({
      plotlyOutput("oneGeoPlot1")
    })
    # loadArea()
    print("Loading base geo")
    
    output$pyramidGeography <- renderPlotly({clinicPlotSolve(dat, dat$attributes[,dat2$area] %>% as.data.frame(),
                                                             dat2$yChoices[1],
                                                             NA,
                                                             makeNice(dat2$area),
                                                             title = NA)%>%
        layout(barmode="overlay",
               xaxis = list(tickangle = -45),
               showlegend = FALSE)
    })
    
    updateSelectInput(session, "oneGeoY", choices = dat2$yChoices)
  } else {
    output$uipyramidGeography <- renderUI({
      "Missing Geographical data. Unable to load this plot."
    })
    output$uioneGeoPlot1 <- renderUI({
      "Missing Area data field. Unable to load this page."
    })
  }
  # Load page 1.6 (determinants) (if applicable?)
  ######### 
  if(toLoad$widerDet) {
    output$uipyramidDeterminants <- renderUI({
      plotlyOutput("pyramidDeterminants")
    })
    output$uioneWidDetPlot1 <- renderUI({
      plotlyOutput("oneWidDetPlot1")
    })
    # loadWiderDeterminants()
    print("Loading base determinants")
    output$pyramidDeterminants <- renderPlotly({clinicPlotSolve(dat, dat$attributes[,dat2$wider[1]] %>% as.data.frame(),
                                                                "Population (Percentage)",
                                                                NA,
                                                                makeNice(dat2$wider[1]),
                                                                title = NA)})
    # Determinants page
    updateSelectInput(session, "oneWidDetY", choices = dat2$yChoices)
    detXvalues <- dat2$wider
    names(detXvalues) <- makeNice(detXvalues %>% sapply(removePrefix) %>% unlist())
    updateSelectInput(session, "oneWidDetX", choices = detXvalues)
  } else {
    output$uipyramidDeterminants <- renderUI({
      "Missing Wider Determinants data fields. Unable to load this plot"
    })
    output$uioneWidDetPlot1 <- renderUI({
      "Missing Wider Determinants data fields. Unable to load this page."
    })
  }
  updateSectionOneGroupBy(session, dat2)

  updateSelectInput(session, "oneWidDetMapRegion", choices = dat2$areaCols)

  print("Summary page loading done")
  
  options = dat2$ltcCols
  source("./functions/3x3plots.R", local = TRUE)
  data <- isolate(getThreeByThreePlotsData(dat$attributes, input, fields = options))
  ######
  output$clin3x3graph <- renderPlot(isolate(getThreeByThreePlots(data, input)))
  d <- reformatTreeSegData(data, dat)
  output$two3x3TreeMap <- renderPlotly({
    renderTreeMapSegPlot(d, input$two3x3gTreeMapOptions)
  })
  output$two3x3Pie <- renderPlotly({
    renderPieSegPlot(d, input$two3x3gTreeMapOptions)
  })
  # Load the segmentation page
  if(toLoad$segBTH) {
    output$seg_definitions <- renderTable({
      df <- data.frame(Segment = c("Frailty", "Limited Reserve", "Short Period of Decline",
                                   "Stable But Serious Disability", "Chronic Conditions", 
                                   "Maternal", "Acutely Ill", "Healthy")
      )
      df <- df %>% mutate(Definition = case_when(Segment == "Frailty" ~ "Is in Electronic Frailty Index (EFI) category 'Frail' or 'Moderate'",
                                                 Segment == "Limited Reserve" ~ "Has one or more of: chronic kidney disease, heart failure, or ever had a myocardial infarction",
                                                 Segment == "Short Period of Decline" ~ "Has had any form of cancer at some time since 2003",
                                                 Segment == "Stable But Serious Disability" ~ "Has one or more of: hearing impairment, visual impairment, ataxia, amnesia, aphasia, cerebral palsy, or a brain injury",
                                                 Segment == "Chronic Conditions" ~ " Has one or more of: ischaematic heart disease (excluding myocardial infarction), arrhythmia (excluding atrial fibrillation), hypertension, diabetes (type 1 or 2), non-alcoholic fatty liver disease, depression, serious mental illness, personality disorder, inflammatory arthritis, asthma, chronic obstuctive pulmonary disorder, or an 'other' significant cardiovascular condition (not explicitly described)",
                                                 Segment == "Maternal" ~ "Is female and pregnant",
                                                 Segment == "Acutely Ill" ~ "Has attended A&E (for any reason) within the previous 12 months",
                                                 Segment == "Healthy" ~ "Is not covered by one of the other segments (n.b. may contain patients with long term conditions or health needs not covered by other segment definitions - e.g. organ transplant, iron-deficiency anaemia, eczema, psoriasis, thyroid disease, coeliac disease, hepatitis B or C, endometriosis, substance dependence, eating disorders, etc.)",
                                                 TRUE ~ "other"))
    })
    
  }

  cm <- dat$attributes%>%select(starts_with("clinic.misc")) %>% colnames
  names(cm) <- sapply(cm, removePrefix) %>% sapply(makeNice)

  t <- dat2$groupByList[!is.na(dat2$groupByList)]
  names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
  
  t2 <- c("total_cost", "total_act")
  names(t2) <- sapply(c("total_cost", "total_act"),makeNicePPY)

  segColNames <- c(
    dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
    c(cm, dat2$clinicCols),
    dat2$areaCols,
    dat2$socioCols,
    t
  )

  segColNames2 <- list(
    dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
    c(cm, dat2$clinicCols),
    dat2$areaCols,
    dat2$socioCols,
    t
  )
  names(segColNames2) <- c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "GlobalGroups")

  updatePickerInput(session, "twoCARTVar1", choices = segColNames2,
                    selected = segColNames)#,
                    # options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable"))
  updatePickerInput(session, "twoRisk1Var1", choices = segColNames2,
                    selected = segColNames)
  
  # 
  updatePickerInput(session, "navTabGlobalVar", choices = segColNames2)
  updatePickerInput(session, "navTabCurrentGlobalVar", choices = t)
  
  updatePickerInput(session, "twoClusterVar1", choices = segColNames2)
  updatePickerInput(session, "twoClusterVar2", choices = segColNames2)
  
  # browser()
  updatePickerInput(session, "twoCARTVar2", choices = t2)
  t2Risk <- colnames(dat$attributes)[startsWith(colnames(dat$attributes), "util.pod")]
  names(t2Risk) <- sapply(t2Risk, removePrefix) %>% sapply(makeNice)
  
  segColNames3 <- segColNames2
  segColNames3[[length(segColNames3)+1]] <- t2
  names(segColNames3)[length(segColNames3)] <- "Total Activity/Cost"
  updatePickerInput(session, "twoClusterVars", choices = segColNames3, 
                    selected = c(segColNames,t2),
                    options = list(`actions-box` = TRUE, size = 12, noneSelectedText = "Please select at least 1 variable"))
  ####

  tree <- getTree(dat2$theorows%>%as.data.frame())
  
  for(i in 1:length(tree)) {
    attr(tree[[i]],"stselected")=TRUE
  }
  output$fiveRiskTargetTree <- renderTree({
    tree
  })
  # print(dat2$threeCohort$rules)
  #################
  dat2$threeCohort <<- list()
  dat2$threeCohort$rules <<- list()
  dat2$threeCohort$rules[[1]] <<- c(dat2$cohortDefaultCols[1], "in (multiple choices select)",  dat2$cohortDefaultVals)
  dat2$threeCohort$join <<- list()
  toLoad$threeCohort <<- T
}