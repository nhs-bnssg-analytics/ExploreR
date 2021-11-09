


# structure for loading:
# create a reacting value "load or not"
# if TRUE, load tabs (being TRUE should be conditional on having a dat file)
# make each plot a ui so it's possible to print an error - may need to ask Josh about this (i.e. if I can user renderplot and validate instead of something else)
# if TRUE. load most tabs (i.e. have an observeEvent waiting on it)
# is there a way to disable all other observeEvents so they don't work until this has happened? or maybe manually freeze them during loading
# have separate loading reactive values for deprivation, area and ethnicity? Maybe tie some of this to a loading variable for section one as most y-axis select inputs will need to be reset

att <- function(attributes, activity, input, output) {
# browser()
  # 1048575 
  a <- input$zeroFieldAssignSelectage
  gc()
  attributes$demog.sex = attributes[,input$zeroFieldAssignSelectsex]
  attributes$demog.age = attributes[,input$zeroFieldAssignSelectage]

  attributes <- attributes %>% mutate(util.age_band_10 = case_when(
    demog.age < 10 ~ "[0,10)",
    demog.age >= 10 & demog.age <= 20 ~ "[10,20)",
    demog.age >= 20 & demog.age <= 30 ~ "[20,30)",
    demog.age >= 30 & demog.age <= 40 ~ "[30,40)",
    demog.age >= 40 & demog.age <= 50 ~ "[40,50)",
    demog.age >= 50 & demog.age <= 60 ~ "[50,60)",
    demog.age >= 60 & demog.age <= 70 ~ "[60,70)",
    demog.age >= 70 & demog.age <= 80 ~ "[70,80)",
    demog.age >= 80 & demog.age <= 90 ~ "[80,90)",
    demog.age >= 90             ~ "90 or over"
  ),

  util.age_band_10 = factor(
    util.age_band_10,
    levels = c(
      "[0,10)",
      "[10,20)",
      "[20,30)",
      "[30,40)",
      "[40,50)",
      "[50,60)",
      "[60,70)",
      "[70,80)",
      "[80,90)",
      "90 or over"
    ),
    ordered = TRUE
  ))
  
  attributes$util.ltc_sum <- attributes %>% select_at(input$ltcDropdown) %>% rowSums()
  
  attributes$util.simple_ltc_sum <- attributes %>% select_at(input$simpleLTCs) %>% rowSums()
  attributes$util.complex_ltc_sum <- attributes %>% select_at(input$complexLTCs) %>% rowSums()

  # Assume date exists, and is in ymd format
  maxDate <- max(activity$dep_date, na.rm = T)
  maxDate2 <- ymd(maxDate) - years(1)
  if(is.na(maxDate2)) {
    maxDate2 <- as.POSIXlt(as.Date(maxDate)) # "2021/02/29"
    maxDate2$year <- maxDate2$year-1
    maxDate2 <- as.Date(maxDate2)
  }
  
  # need to make this a vector of nrow length
  sumCols <- function(df, cols, segment, input) {
    gc()
    if(segment == "twoBTHChronic") {
      print("On Chronic")
      input <- as.list(c(rep(1, times = length(cols)),rep("=", times = length(cols))))
      names(input) <- c(sapply(cols, function(i) {paste0(segment,i,"SelectI")}),sapply(cols, function(i) {paste0(segment,i,"Select")}))
    }
    df <- df %>% select_at(cols)
    # print(str(cols))
    gc()
    c <- colnames(df)
    for (i in c) {
      # For each column, find what the mapping is for valid elements, and apply it
      if(input[[paste0(segment,i,"Select")]] == "=") {
        df[,i] <- case_when(df[,i] == input[[paste0(segment,i,"SelectI")]] ~ 1,
                            TRUE ~ 0)
      } else if (input[[paste0(segment,i,"Select")]] == "in numeric range") {
        df[,i] <- case_when(input[[paste0(x,i,"SelectR1")]] <= df[,i] & df[,i] <= input[[paste0(x,i,"SelectR2")]] ~ 1,
                            TRUE ~ 0)
      } else if (input[[paste0(segment,i,"Select")]] == ">=") {
        df[,i] <- case_when(df[,i] >= input[[paste0(segment,i,"SelectI")]] ~ 1,
                            TRUE ~ 0)
        
      } else if (input[[paste0(segment,i,"Select")]] == "<=") {
        df[,i] <- case_when(df[,i] <= input[[paste0(segment,i,"SelectI")]] ~ 1,
                            TRUE ~ 0)
        
      } else if (input[[paste0(segment,i,"Select")]] == "in (multiple choices select)") {
        df[,i] <- case_when(df[,i] %in% input[[paste0(segment,i,"SelectC")]] ~ 1,
                            TRUE ~ 0)
      }
    }
    df <- df %>% rowSums(na.rm = TRUE)
    # print(str(df))
    df
  }
  # TODO: test code below
  if(input$ByActivitytabs == "By Activity") {
    util.act.secondary_ae_attended <- activity %>% filter(.data[["dep_date"]] >= maxDate2) %>%
      filter(!!sym(input$aeidentifierCol) == input$aeidentifierVal) %>% group_by_at(c("id", input$aeidentifierCol)) %>% summarise(n = n()) %>% as.data.frame()

    attributes$util.act.secondary_ae_attended <- left_join(attributes %>% select(id) %>% as.data.frame(), util.act.secondary_ae_attended) %>%
      select(n) %>%
      as.list() %>% unlist() %>% lapply(function(x) {if (is.na(x)) {0} else {x}}) %>% unlist()
  } else {
    attributes$util.act.secondary_ae_attended <- sumCols(attributes, input$twoBTHAcute, "twoBTHAcute", input)
    
  }
  
  # util.act.secondary_ae_attended <- activity  %>% filter(.data[["dep_date"]] >= maxDate2) %>%
  #   filter(!!sym(input$aeidentifierCol) == input$aeidentifierVal) %>% group_by_at(c("id", input$aeidentifierCol)) %>% summarise(n = n()) %>% as.data.frame()
  # 
  # attributes$util.act.secondary_ae_attended <- left_join(attributes %>% select(id) %>% as.data.frame(), util.act.secondary_ae_attended) %>% 
  #   select(n) %>% 
  #   as.list() %>% unlist() %>% lapply(function(x) {if (is.na(x)) {0} else {x}}) %>% unlist()

  seg <- c("twoBTHFrailty", "twoBTHLimitedReserve", "twoBTHDecline","twoBTHDisability","twoBTHChronic", "twoBTHMaternal")
  
  gc()
  
  # Check if fields have been filled in - if not, then skip BtH

  if (input$twoBTHFrailty == "") { # Frailty
    # output$twoBTHError <- renderText({"Missing Frailty Data"})
  } else if (is.null(input$twoBTHLimitedReserve)) { # Limited Reserve
    # output$twoBTHError <- renderText({"Missing Limited Reserve Data"})
  } else if (is.null(input$twoBTHDecline)) { # Short Period of Decline
    # output$twoBTHError <- renderText({"Missing Short Period of Decline Data"})
  } else if (is.null(input$twoBTHDisability)) { # Stable But Serious Disability
    # output$twoBTHError <- renderText({"Missing Stable But Serious Disability Data"})
  } else if (is.null(input$twoBTHChronic)) { # Chronic Conditions
    # output$twoBTHError <- renderText({"Missing Chronic Conditions Data"})
  } else if (input$twoBTHMaternal == "") { # demog.pregnancy
    # output$twoBTHError <- renderText({"Missing Maternity Data"})
  } else if (!("util.act.secondary_ae_attended" %in% colnames(attributes))) { # util.act.secondary_ae_attended
    # output$twoBTHError <- renderText({"Missing Acute Health Data [A&E/needs change]"})
  } else { # Free to execute ~~ Need to check if each numInput has a value
    # print(str(attributes))
    gc()
    attributes <- attributes %>% mutate(.,
                                        util.bth.LimitedReserve = sumCols(., input$twoBTHLimitedReserve, "twoBTHLimitedReserve", input),
                                        util.bth.Decline = sumCols(., input$twoBTHDecline, "twoBTHDecline", input),
                                        util.bth.Disability = sumCols(., input$twoBTHDisability, "twoBTHDisability", input),
                                        util.bth.Chronic = sumCols(., input$twoBTHChronic, "twoBTHChronic", input),
                                        util.bth.Frailty = sumCols(., input$twoBTHFrailty, "twoBTHFrailty", input),
                                        util.bth.Maternity = sumCols(., input$twoBTHMaternal, "twoBTHMaternal", input)
    ) %>% 
      mutate(., util.segmentation.BtHSegment=case_when(util.bth.Frailty > 0 ~ "Frailty",
                                  
                                                       util.bth.LimitedReserve > 0 ~ "Limited Reserve",
                                                       
                                                       util.bth.Decline > 0 ~ "Short Period of Decline",
                                                       
                                                       util.bth.Disability > 0 ~ "Stable But Serious Disability",
                                                       
                                                       util.bth.Chronic > 0 ~ "Chronic Conditions",
                                                       
                                                       # demog.sex == "female" & # assume only females have maternal = 1
                                                       util.bth.Maternity == 1 ~ "Maternal", #TODO
                                                       
                                                       util.act.secondary_ae_attended > 0 ~ "Acutely Ill",
                                                       
                                                       TRUE ~ "Healthy"
      )) %>%
      mutate(util.segmentation.BtHDefinition = case_when(util.segmentation.BtHSegment == "Frailty" ~ "Is in Electronic Frailty Index (EFI) category 'Frail' or 'Moderate'",
                                                         util.segmentation.BtHSegment == "Limited Reserve" ~ "Has one or more of: chronic kidney disease, heart failure, or ever had a myocardial infarction",
                                                         util.segmentation.BtHSegment == "Short Period of Decline" ~ "Has had any form of cancer at some time since 2003",
                                                         util.segmentation.BtHSegment == "Stable But Serious Disability" ~ "Has one or more of: hearing impairment, visual impairment, ataxia, amnesia, aphasia, cerebral palsy, or a brain injury",
                                                         util.segmentation.BtHSegment == "Chronic Conditions" ~ " Has one or more of: ischaematic heart disease (excluding myocardial infarction), arrhythmia (excluding atrial fibrillation), hypertension, diabetes (type 1 or 2), non-alcoholic fatty liver disease, depression, serious mental illness, personality disorder, inflammatory arthritis, asthma, chronic obstuctive pulmonary disorder, or an 'other' significant cardiovascular condition (not explicitly described)",
                                                         util.segmentation.BtHSegment == "Maternal" ~ "Is female and pregnant",
                                                         util.segmentation.BtHSegment == "Acutely Ill" ~ "Has attended A&E (for any reason) within the previous 12 months",
                                                         util.segmentation.BtHSegment == "Healthy" ~ "Is not covered by one of the other segments (n.b. may contain patients with long term conditions or health needs not covered by other segment definitions - e.g. organ transplant, iron-deficiency anaemia, eczema, psoriasis, thyroid disease, coeliac disease, hepatitis B or C, endometriosis, substance dependence, eating disorders, etc.)",
                                                         TRUE ~ "other"
      )) %>% select(-c(util.bth.LimitedReserve, util.bth.Decline, util.bth.Disability, util.bth.Chronic, util.bth.Frailty, util.bth.Maternity))
    
  }
  
  
  q <- function(x) {if (is.na(x)) {0} else {x}}
  # browser()
  gc()
  activity$cost <- sapply(activity$cost, q) %>% unlist()
  data <- activity %>% filter(.data[["dep_date"]] >= maxDate2) %>% select(id, pod_l1, cost) %>%  
    group_by(id, pod_l1) %>% summarise(cost = sum(cost),
                                       act = n()) %>% ungroup() %>% as.data.frame()
  gc()
  # browser()
  # t <- unique(data$pod_l1)[which(!is.na(unique(data$pod_l1)))[1]]
  for(i in unique(data$pod_l1)) {
    # Add cost and activity counts per pod_l1 values
    # id, pod_l1, act, cost
    td <- left_join(attributes %>% select_at("id") %>% as.data.frame(),
                    data[data$pod_l1 == i,],
              by = "id"
    ) %>% as.data.frame()
    
    for(j in c("act","cost")){
      td[,j] <- lapply(td[,j], q) %>% unlist()
    }
    
    attributes[[paste0("util.pod_l1.", i, ".cost")]] <- td$cost
    attributes[[paste0("util.pod_l1.", i, ".act")]] <- td$act
  }
  # browser()
  # attributes <- attributes[,!names(attributes)%in%(
  #   c("demog.sex", "demog.age")[c("demog.sex", "demog.age")%in%c(input$zeroFieldAssignSelectsex,input$zeroFieldAssignSelectage)]
  #   )]

  if("demog.age" != input$zeroFieldAssignSelectage) {
    attributes <- attributes[ , !(names(attributes) %in% "demog.age")]
  }
  if("demog.sex" != input$zeroFieldAssignSelectsex) {
    attributes <- attributes[ , !(names(attributes) %in% "demog.sex")]
  }
  
  # browser()
  # We need to get rid of NAs in the data
  # for(i in colnames(attributes)) {
  #   if(is.numeric(attributes[[i]])) {
  #     
  #   } else if(is.character(attributes[[i]])) {
  #     
  #   } else if(is.factor(attributes[[i]])) {
  #     
  #   }
  #   
  # }
  
  
  return(attributes)
}



groupLookUp <- function(dat2, value) {
  if (value == "") {
    return(NA)
  }
  which(dat2$groupByListText == value) %>%
    dat2$groupByList[.] %>% unlist() %>% return()
}

makeNice <- function(data) {
  data <- sapply(data, function(x) {
    s <- strsplit(x, ".", fixed = TRUE)[[1]]
    s[length(s)]
  })
  sapply(data, function(x) {
    # Eliminate "_" and capitalise
    s <- strsplit(x, "_", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse=" ")
    # Eliminate "." and capitalise
    s <- strsplit(s, ".", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse=" ")
    # Eliminate " " and capitalise
    s <- strsplit(s, " ", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse=" ")
  })
}

dep <- function(input, dat#, 
                # area = input$zeroFieldAssignSelectlocalit,
                # saveAct = T
                ) {
  gc()
  
  print("Starting dat2")
  # Assume date exists, and is in ymd format
  maxDate <- max(dat$activity$dep_date, na.rm = T)
  maxDate2 <- ymd(maxDate) - years(1)
  if(is.na(maxDate2)) {
    maxDate2 <- as.POSIXlt(as.Date(maxDate)) # "2021/02/29"
    maxDate2$year <- maxDate2$year-1
    maxDate2 <- as.Date(maxDate2)
  }
  
  dat2 <- list()
  dat2$cols <- colnames(dat$attributes)
  dat2$clinicCols <- dat$attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames()
  dat2$actCols <- colnames(dat$activity)
  l <- ncol(dat$activity)
  dat2$ltcNames <- input$ltcDropdown
  dat2$actPodCols <- colnames(dat$activity)[c(2:(l-4))]
  dat2$pods <- dat2$actCols[2:(ncol(dat$activity) - 4)]
  dat2$dep <- input$zeroFieldAssignSelectindex
  dat2$area <- input$zeroFieldAssignSelectlocalit#area
  
  if (dat2$dep == "") {dat2$dep <- NA}
  if (dat2$area == "") {dat2$area <- NA}
  
  a <- colnames(dat$attributes %>% select(starts_with("area.")))
  dat2$areaOrder <- c(dat2$area, a[!a%in%c(dat2$area)])
  
  dat2$ethnicity <- input$zeroFieldAssignSelectethn
  if (dat2$ethnicity == "") {dat2$ethnicity <- NA}
  
  
  dat2$wider <- input$DeterminantCheckBox#input$ltcDropdown
  if(is.null(dat2$wider)) {dat2$wider <- NA}
  dat2$theorows <- dat$activity %>% group_by_at(dat2$actPodCols) %>% summarise(n = n()) %>% select_at(dat2$actPodCols) %>% ungroup()
  dat2$theorows[is.na(dat2$theorows)] <- "Missing/None"
  dat2$sex <- input$zeroFieldAssignSelectsex
  dat2$age <- input$zeroFieldAssignSelectage
  ## Need to make sure all fields are plottable - i.e. not continous, if so then split into bands, no NAs, etc.
  
  
  ## Need to format names
  removePrefix <- function(value) {
    s <- strsplit(value, ".", fixed = TRUE)[[1]]
    s[length(s)]
  }
  dat2$ltcCols <- input$ltcDropdown
  names(dat2$ltcCols) <- sapply(dat2$ltcCols, removePrefix)
  # Realistically, I need to repeat this for all possible groups of names
  dat2$areaCols <- dat$attributes %>% select(starts_with("area."))%>% colnames()
  names(dat2$areaCols) <- sapply(dat2$areaCols, removePrefix) %>% sapply(makeNice)
  
  dat2$clinicCols <- dat$attributes %>% select(starts_with("clinic.")) %>% select(!starts_with("clinic.misc")) %>% colnames()
  names(dat2$clinicCols) <- sapply(dat2$clinicCols, removePrefix) %>% sapply(makeNice)
  
  dat2$demogCols <- dat$attributes %>% select(starts_with("demog."))%>% colnames()
  names(dat2$demogCols) <- sapply(dat2$demogCols, removePrefix) %>% sapply(makeNice)
  
  dat2$socioCols <- dat$attributes %>% select(starts_with("socio."))%>% colnames()
  names(dat2$socioCols) <- sapply(dat2$socioCols, removePrefix) %>% sapply(makeNice)
  
  dat2$widerCols <- input$DeterminantCheckBox
  names(dat2$widerCols) <- sapply(dat2$widerCols, removePrefix) %>% sapply(makeNice)
  
  
  # attributes$util.ltc_sum <- attributes %>% select_at(input$ltcDropdown) %>% rowSums()
  # attributes$util.simple_ltc_sum <- attributes %>% select_at(input$simpleLTCs) %>% rowSums()
  # attributes$util.complex_ltc_sum <- attributes %>% select_at(input$complexLTCs) %>% rowSums()
  dat2$simpleLTCCols <- input$simpleLTCs
  dat2$complexLTCCols <- input$complexLTCs
  dat2$cartModel <- NA
  present <- function(x) {removePrefix(makeNice(x))}
  
  
  uniqs <- sapply(dat2$demogCols, function(x) {
    length(unique(dat$attributes[[x]]))
  })
  # Assume at least one demog field has < 10 unique values
  dat2$cohortDefaultCols <- dat2$demogCols[which(uniqs < 10)[1]]
  dat2$cohortDefaultVals <- unique(dat$attributes[[dat2$cohortDefaultCols]])[!is.na(unique(dat$attributes[[dat2$cohortDefaultCols]]))]
  
  # There may be a future need to add to this list
  dat2$groupByList <- list("util.ltc_sum", dat2$dep, "util.age_band_10", dat2$area[1], dat2$ethnicity, dat2$wider[1], "util.segmentation.BtHSegment")
  dat2$groupByListText <- list("LTC Count / Multimorbidity", "Deprivation", "Age Bands (10)", unname(makeNice(dat2$area[1])),"Ethnicity","Wider Determinants","BtH Segments")
  # "Area"                                                        # present(dat2$area[1]) / "ICP (current localities)"
  #                                                               
  
  
  if(!"util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
    dat2$groupByList <- dat2$groupByList[-7]
    dat2$groupByListText <- dat2$groupByListText[-7]
  }
  ## Summary page graphs
  dat2$yChoices <- c("Population (Percentage)",
                     "Population (Count)",
                     "Spend (Total, £s)",
                     "Spend (Per Capita, £s)",
                     "Activity (Total)",
                     "Activity (Per Capita)")
  
  # 1 Demog
  
  # 2 Clinical
  dat2$ltc <- "util.ltc_sum"

  ##############################################################
  print("Activity 2nd graph (cumulative spending)")
  
  dat2$actGraph2 <- dat$attributes %>%
    select_at("total_cost") %>% 
    arrange_at("total_cost") %>%
    mutate(.,cost_cumsum_prop = cumsum(total_cost) / sum(total_cost),
           pop_prop = row_number() / nrow(.))
  # Need a smart way of dropping a couple hundred thousand rows
  while(nrow(dat2$actGraph2) > 2000) {
    toKeep <- c()
    i = 1
    while(i < nrow(dat2$actGraph2)) {
      q <- which(dat2$actGraph2$cost_cumsum_prop < dat2$actGraph2$cost_cumsum_prop[i] + 0.01 & dat2$actGraph2$cost_cumsum_prop > dat2$actGraph2$cost_cumsum_prop[i])
      if(length(q) < 1) {
        toKeep <- c(toKeep, i:nrow(dat2$actGraph2))
        break()
      }
      s <- sample((1:length(q))+i, ceiling(length(q)/10))
      toKeep <- c(toKeep, s)
      i <- i + length(q)
    }
    dat2$actGraph2 <- dat2$actGraph2[unique(toKeep),]
  }
  dat2$actGraph2$text <- paste(round(dat2$actGraph2$pop_prop * 100, digits = 1),"% of the population uses ", "\n",
                               round(dat2$actGraph2$cost_cumsum_prop * 100, digits = 1), "% of spending",
                               sep = "")
  print("Done")

  # old_dat2 <- try(readRDS("./data/dat2.rds"),silent = TRUE)
  # if(class(old_dat2) != "try-error") {
  #   nums <- unique(old_dat2$actLookUp[,2])
  #   for(i in nums) {
  #     print(i)
  #     file.remove(paste0("./data/theoAct/", i, "act_part.rds"))
  #   }
  # }
  
  # Cut activity and save down in chunks
  # want no more than 10000 in each group
  # if(saveAct) {
  # if(T) {
  #   saveRDS(dat$activity %>% filter(.data[["dep_date"]] >= maxDate2), paste0("./data/act_.rds"))
  #   
  #   ids <- unique(dat$activity$id)
  #   gn <- ceiling(length(ids) / 10000) # is how many groups we want
  #   df <- lapply(1:gn, function(x) {
  #     c <- ids[((x-1)*10000 + 1):(x*10000)]
  #     c <- which(dat$activity$id %in% c)
  #     c <- dat$activity[c,]
  #     
  #     saveRDS(c,paste0("./data/theoAct/", x, "act_part.rds"))
  #     # return the part of the table for lookup
  #     data.frame(id = ids[((x-1)*10000 + 1):(x*10000)], lookUp = x) # need to cut NAs off
  #   })
  #   df <- do.call(rbind, df)
  #   df <- df[!is.na(df$id),]
  #   dat2$actLookUp <- rbind(df,data.frame(id = which(!dat$attributes$id%in%ids), lookUp = -1))
  # } else {
  #   dat2$actLookUp <- readRDS("./data/dat2.rds")$actLookUp 
  # }
  
  
  print("Min and max times for theoplot")
  # browser()
  
  dat2$multiTheoColSNum <- 0
  dat2$multiTheoColS <- c()
  
  dat2$mintime <- min(dat$activity$arr_date, na.rm = T)
  dat2$maxtime <- max(dat$activity$dep_date, na.rm = T)
  dat2$allPods <- lapply(2:(ncol(dat$activity)-4), function(x) {
    as.list(unique(dat$activity[,x]))
  })
  

  # Initial Analysis Dataset is the Population Dataset - do I need to write this in a separate file to the disk?
  # If not, will just attach to dat2
  dat2$AnalysisDatasets <- data.frame("Population_Dataset" = rep(T, nrow(dat$attributes)))
  dat2$AnalysisDatasetsStats <- data.frame("Population_Dataset" = sum(dat$attributes$total_act))

  
  return(dat2)
  
}


