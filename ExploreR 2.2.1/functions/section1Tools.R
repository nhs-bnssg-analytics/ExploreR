
loadGroupBy <- function(dat2, id, exclude) {
  indexes <- c(which(is.na(dat2$groupByList)), exclude)
  dat2$groupByListText[-indexes] %>% return()
}

loadGroupBySelect <- function(session, dat2, id, exclude = NULL) {
  # Exclude the page's own focus
  if(is.null(exclude)) {
    names <- dat2$groupByListText[!is.na(dat2$groupByList)]
  } else if (exclude == "demog") {
     names <- loadGroupBy(dat2, id, 3)
  } else if (exclude == "clinic") {
    names <- loadGroupBy(dat2, id, 1)
  } else if (exclude == "deprivation") {
    names <- loadGroupBy(dat2, id, 2)
  } else if (exclude == "segmentation") {
    names <- loadGroupBy(dat2, id, 7)
  } else if (exclude == "area") {
    names <- loadGroupBy(dat2, id, 4)
  } else if (exclude == "wider") {
    names <- loadGroupBy(dat2, id, 6)
  }
  # For reference: 
  # dat2$groupByList <- list(dat2$ltc, dat2$dep, "util.age_band_10", dat2$area, dat2$ethnicity, NA, NA)
  # dat2$groupByListText <- list("LTC Count", "Deprivation", "Age", "Area","Ethnicity","Wider Determinants","BtH Segments")
  # (never exclude age)
  updateSelectizeInput(session, id, choices = names,
                       selected = input[[id]],
                       options = list(
                         placeholder = 'Please select a field below (optional)',
                         onInitialize = I('function() { this.setValue(""); }')
                       ))

}
updateSectionOneGroupBy <- function(session, dat2) {
  
  ## Demog tab
  loadGroupBySelect(session, dat2, "oneDemogGroupBy", "demog")
  
  ## Clinical Tab
  loadGroupBySelect(session, dat2, "oneClinGroupBy", "clinic")
  
  ## Activity Tab
  # Different type of graphs - nothing to update - or maybe some? The y-axis might be updateable, but would need to auto-select the first option
  
  ## Deprivation Tab
  loadGroupBySelect(session, dat2, "oneDeprivationGroupBy", "deprivation")
  
  ## Geographical Tab
  loadGroupBySelect(session, dat2, "oneGeoGroupBy", "area")
  
  ## Wider Determinants Tab
  loadGroupBySelect(session, dat2, "oneWidDetGroupBy", "wider")
  
  ## BtH Tab
  loadGroupBySelect(session, dat2, "twoBTHFocus", "segmentation")
}

groupLookUp <- function(dat2, value) {
  if (value == "") {
    return(NA)
  }
  which(dat2$groupByListText == value) %>%
    dat2$groupByList[.] %>% unlist() %>% return()
}
newClinPlot <- function(data, y, group, xtitle = "Number of LTCs", title = "Multimorbidity", guideText = "missing Tooltip") {
  p <- ggplot(data, aes_string(x = "plotField", y = "percentage", fill = if(!is.na(group)) {"group"}, label = "percentage",
                               text = "text"
  )) +
    geom_bar(stat = "identity") +
    xlab(xtitle) + ylab(y) +
    theme(axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 9))
    # scale_x_discrete(guide = guide_axis(n.dodge=3))
  if(!is.na(title)) {
    p <- p + ggtitle(title)
  }
  if (is.na(group)) {
    p <- p + guides(fill=FALSE)
  } else {
    p <- p + labs(fill=guideText)
  }
  return(ggplotly(p,
                  tooltip = "text"
  ))
}
clinicPlotSolveData <- function(dat, data, y, group = "area.Lower Super Output Area (LSOA)") {
  colnames(data) <- "plotField"
  data$plotField <- factor(data$plotField)
  if (!is.na(group)) {
    data <- cbind(as.data.frame(dat$attributes[,group] %>% factor()), data) %>% group_by_all()
    colnames(data) <- c("group", colnames(data)[2]) 
  } else {
    data <- data %>% group_by_at(vars("plotField"))
  }
  if (y == "Population (Percentage)") {
    data <- data %>% summarise(n = n()) %>% mutate(percentage = n/nrow(data) * 100) %>% ungroup() %>% as.data.frame()
  } else if (y == "Population (Count)") {
    data <- data %>% summarise(n = n()) %>% mutate(percentage = n) %>% ungroup() %>% as.data.frame()
  } else if (y == "Spend (Total, £s)") {
    data <- data %>% ungroup() 
    data$cost <- dat$attributes$total_cost 
    data <- data %>% group_by_at(colnames(data)[-ncol(data)]) %>%
      summarise(percentage = round(sum(cost))) %>% ungroup() %>% as.data.frame()
    
  } else if (y == "Spend (Per Capita, £s)") {
    data$cost <- dat$attributes$total_cost
    data <- data %>% group_by_at(colnames(data)[-ncol(data)]) %>%
      summarise(percentage = round(mean(cost))) %>% ungroup() %>% as.data.frame()
  } else if (y == "Activity (Total)") {
    data <- data %>% ungroup()
    data$act <- dat$attributes$total_act
    data <- data %>% group_by_at(colnames(data)[-ncol(data)]) %>%
      summarise(percentage = round(sum(act))) %>% ungroup() %>% as.data.frame()
    
  } else if (y == "Activity (Per Capita)") {##
    data <- data %>% ungroup()
    data$act <- dat$attributes$total_act
    data <- data %>% group_by_at(colnames(data)[-ncol(data)]) %>%
      summarise(percentage = round(mean(act))) %>% ungroup() %>% as.data.frame()
  }
  data
}
clinicPlotSolve <- function(dat, data, y, group, xtitle = "Number of LTCs", title = "Multimorbidity", guideText = "missing Tooltip") {
  # TODO is this the function for plotting cover graphs?
  
  data <- clinicPlotSolveData(dat, data, y, group = group)
  
  # Generate hovertext pre-plot
  supIfNeeded <- function(data, y) {
    if(grepl("Population (C", y, fixed=TRUE)) {sapply(data,function(x) {
      if(x <= 5) {"supressed"} else {format(round(x, digits = 2),big.mark = ",")}
    })} else {
      format(round(data, digits = 2),big.mark = ",")
    }
  }
  data$text <- 
    if(is.na(group)) {
      paste(
        xtitle, ": ", data[,"plotField"], " \n",
        y, ": ", supIfNeeded(data[,"percentage"], y), " \n",
        sep = ""
      )
    } else {
      paste(
        xtitle, ": ", data[,"plotField"], "\n",
        y, ": ", supIfNeeded(data[,"percentage"], y), "\n",
        guideText, ": ", data[,"group"], "\n",
        sep = ""
      )
    }
  # blue <- "blue"#"#3366FF"
  # browser()
  
  p <- if(is.na(group)) {
    ggplot(data, aes_string(x = "plotField", y = "percentage", label = "percentage",
                            text = "text"
    ))  + scale_x_discrete(guide = guide_axis(n.dodge=3))+
      geom_bar(stat = "identity", fill = "#3366FF", alpha = 0.5) + 
      xlab(xtitle) + ylab(y) +
      scale_y_continuous(labels = scales::label_number_si())
  } else {
    ggplot(data, aes_string(x = "plotField", y = "percentage", fill = "group", label = "percentage",
                            text = "text"
    ))  + scale_x_discrete(guide = guide_axis(n.dodge=3))+
      geom_bar(stat = "identity") + # , fill = if(is.na(group)) {"#3366FF"} else {group}, alpha = if(is.na(group)) {0.5} else {1}
      xlab(xtitle) + ylab(y) +
      scale_y_continuous(labels = scales::label_number_si())
  }
  
  # p <- ggplot(data, aes_string(x = "plotField", y = "percentage", fill = if(!is.na(group)) {"group"} else {'"#3366FF"'}, label = "percentage",
  #                              text = "text"
  # ))  + scale_x_discrete(guide = guide_axis(n.dodge=3))+
  #   geom_bar(stat = "identity") + # , fill = if(is.na(group)) {"#3366FF"} else {group}, alpha = if(is.na(group)) {0.5} else {1}
  #   xlab(xtitle) + ylab(y) +
  #    scale_y_continuous(labels = scales::label_number_si())
  
  if(!is.na(title)) {
    p <- p + ggtitle(title)
  }
  if (is.na(group)) {
    p <- p + guides(fill=FALSE)
  } else {
    p <- p + labs(fill=guideText)+
    scale_fill_viridis_d()
  }
  
  return(ggplotly(p,
                  tooltip = "text"
  ))
}


makeNice <- function(data) {
  ## Function to make labels / data look nicer of character
  # data can be a string or a character vector
  
  # First remove any and all prefixes
  data <- sapply(data, function(x) {
    s <- strsplit(x, ".", fixed = TRUE)[[1]]
    s[length(s)]
  })
  as.character(data)
  #######
  
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
sw <- function(string) {
  new_s <- names(string)
  names(new_s) <- string
  new_s
}

makeNicePPY <- function(data) {
  paste0(makeNice(data), " PPY")
}

removePrefix2 <- function(data) {
  sapply(data, function(x) {
    s <- strsplit(x, ".", fixed = TRUE)[[1]]
    s[length(s)] %>% makeNice
    })
}


## source: https://github.com/msberends/AMR/blob/master/R/age.R
age_groups <- function(x, split_at) {
  split_at <- sort(unique(as.integer(split_at)))
  if (!split_at[1] == 0) {
    split_at <- c(0, split_at)
  }
  # turn input values to 'split_at' indices
  y <- x
  lbls <- split_at
  for (i in seq_len(length(split_at))) {
    y[x >= split_at[i]] <- i
    # create labels
    lbls[i - 1] <-
      paste0(unique(c(split_at[i - 1], split_at[i] - 1)), collapse = "-")
  }
  
  # last category
  lbls[length(lbls)] <- paste0(split_at[length(split_at)], "+")
  
  agegroups <- factor(lbls[y], levels = lbls, ordered = TRUE)
  
  agegroups
}
any_groups <- function(x, split_at) {
  split_at <- sort(unique((split_at)))
  if (!split_at[1] == 0) {
    split_at <- c(0, split_at)
  }
  # turn input values to 'split_at' indices
  y <- x
  lbls <- split_at
  for (i in seq_len(length(split_at))) {
    y[x >= split_at[i]] <- i
    # create labels
    lbls[i - 1] <-
      paste0(unique(c(split_at[i - 1], split_at[i] )), collapse = "-")
  }
  
  # last category
  lbls[length(lbls)] <- paste0(split_at[length(split_at)], "+")
  agegroups <- factor(lbls[y], levels = lbls, ordered = TRUE)
  
  agegroups
}

makeNiceVCol <- function(data) {
  ## Function to make labels / data look nicer of character
  # data can be a string or a character vector
  sapply(data, function(x) {
    # Eliminate "." and capitalise
    s <- strsplit(x, ".", fixed = TRUE)[[1]]
    s <- s[length(s)]
    # Split on _ and Capitalise
    
    s <- strsplit(s, "_", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse=" ")
    
    # Concetanate with original field for reference?
    s <- paste0(s, " ( in data as field: ",x, ")")
    
  })
}

## Function for producing lapply-ed UI for selecting values at data loading.
makeUI <- function(values, segment = "Segment1") {
  lapply(values, function(x) {
    column(width = 6,
           selectInput(id = paste0(segment,x,"Select"),
                       label = "Select operator for highlighting field values",
                       choices = c("=", "in numeric range", ">=", "<=", "in (multiple choices select)")),
           uiOutput(paste0(segment,x,"SelectUI")),
           if(string == TRUE) {
             
           }
           
           )
    
    
  })
}

# # TODO potentially obsolete
removeDots <- function(x) {
  sapply(data, function(x) {
    # Eliminate "." and capitalise
    s <- strsplit(x, ".", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse="")

  })
}
removeScores <- function(x) {
  # browser()
  sapply(x, function(y) {
    # Eliminate "_" and capitalise
    s <- strsplit(y, "_", fixed = TRUE)[[1]]
    s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep="", collapse="")
    
  })
}

any_groups <- function(x, split_at) {
  split_at <- sort(unique((split_at)))
  if (!split_at[1] == 0) {
    split_at <- c(0, split_at)
  }
  # turn input values to 'split_at' indices
  y <- x
  lbls <- split_at
  for (i in seq_len(length(split_at))) {
    y[x >= split_at[i]] <- i
    # create labels
    lbls[i - 1] <-
      paste0(unique(c(split_at[i - 1], split_at[i] )), collapse = "-")
  }
  
  # last category
  lbls[length(lbls)] <- paste0(split_at[length(split_at)], "+")
  agegroups <- factor(lbls[y], levels = lbls, ordered = TRUE)
  
  agegroups
}


removePrefix <- function(value) {
    s <- strsplit(value, ".", fixed = TRUE)[[1]]
    if(s[-1] %>% paste0(collapse = ".") == "") {
      s %>% paste0(collapse = ".")
    } else {
      s[-1] %>% paste0(collapse = ".")
    }
}


getSelectedShinyTreeValues <- function(dat2, tree) {
  ntree <- dat2$theorows %>% as.data.frame()
  t <- sapply(1:nrow(ntree), function(i) {
    removeScores(ntree[i,]) %>% paste0(collapse = ".")
  })
  treeCols <- which(t %in% (get_selected(tree, format = "names") %>%
                              lapply(function(x){
                                paste0(c(attr(x, "ancestry"), x[1]), collapse = ".")
                              })%>%
                              unlist()))
  selected = ntree[treeCols,]
  selected = sapply(1:nrow(selected), function(x) {
    data <- selected[x, ]
    paste0(data, collapse = ".")
  }) %>% unlist()
  list(selected, ntree, treeCols)
}

getActGraphData <- function(dat, dat2, selectedGroup) {
  acts = unique(dat2$theorows$pod_l1)[makeNice(unique(dat2$theorows$pod_l1))%in%input$oneActX]
  data <- dat$attributes %>% 
    select_at(c("id", lapply(acts, function(x) {
      if(grepl("Spend", input$oneActCoA, fixed = T)) {
        paste0("util.pod_l1.", x, ".cost")
      } else {
        paste0("util.pod_l1.", x, ".act")
      }
    }) %>% unlist(), selectedGroup)) %>% 
    as.data.frame()
  if(grepl("Spend", input$oneActCoA, fixed = T)) {
    actOrCost = ".cost"
  } else {
    actOrCost = ".act"
  }
  # browser()
  print("Initial data")
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
  print("On total or capita")
  if(!grepl("Capita", input$oneActCoA, fixed = T)) {
    data <- groupAndSum(data, selectedGroup, actOrCost = actOrCost, sum)
    ## Add total here?
    if(("Total"%in%input$oneActX)) {
      print("Inserting total")
      newdat <- data %>% group_by_at(selectedGroup) %>% summarise(total = sum(cost)) %>% ungroup() %>% as.data.frame()
      newdat <- cbind(rep("Total", nrow(newdat)), newdat)
      colnames(newdat) <- colnames(data)
      data <- rbind(newdat, data %>% as.data.frame())
    }
  } else {
    # Per capita version
    t <- data %>% group_by_at(selectedGroup) %>%
      summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), function(x) {length(which(x > 0))}) %>%
      pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), names_to = "pod_l1")
    if(("Total"%in%input$oneActX)) {
      print("Inserting total")
      
      # data -> sum across every util col -> group, divide by n()
      t2 <- data %>% group_by_at(selectedGroup) %>% summarise(total = rowSums(across(starts_with("util."))),
                                                              total_transformed = case_when(total > 0 ~ 1,
                                                                                            T ~ 0)) %>% 
        summarise(total = sum(total),
                  n = n(),
                  total_n = sum(total_transformed),
                  cost = total/total_n)
    }
    
    data <- groupAndSum(data, selectedGroup, actOrCost = actOrCost, sum)
    data$cost <- data$cost / t$value
    data$cost[is.na(data$cost)] <- 0
    if(("Total"%in%input$oneActX)) {
      newdat <- data.frame("Total",t2[[colnames(data)[2]]],t2$cost)
      colnames(newdat) <- colnames(data)
      data <- rbind(
        newdat
        , data %>% as.data.frame())
    }
  }
  # browser()
  data <- data %>% as.data.frame()
  
  print("Final part")
  data[,"pod_l1"] <- data[,"pod_l1"] %>% as.character() %>% makeNice() #(data[,"pod_l1"] %>% as.data.frame())[,1]
  data$text <-
    paste(input$oneActCoA, " is ", format(round(data[,"cost"], digits = 2), big.mark = ","), "\n",
          "At ", makeNice(selectedGroup), " = ", data[,selectedGroup], "\n",
          "And POD ", data[,"pod_l1"], "\n",
          sep = ""
    )
  data
}



analysisDatasetUpdateFunction <- function(toLoad, input, output, dat2, dat, text) {
  print("Updating UI")
  cm <- dat$attributes%>%select(starts_with("clinic.misc")) %>% colnames
  names(cm) <- sapply(cm, removePrefix) %>% sapply(makeNice)
  
  t <- dat2$groupByList[!is.na(dat2$groupByList)]
  names(t) <- dat2$groupByListText[!is.na(dat2$groupByList)]
  
  t2 <- c("total_cost", "total_act")
  names(t2) <- sapply(c("total_cost", "total_act"),makeNicePPY)
  
  segColNames <- c(
    dat2$demogCols[which(dat2$demogCols%in%colnames(dat$attributes))],
    c(cm, dat2$ltcCols),
    dat2$socioCols,
    t,t2 ) %>% unlist()
  
  output[[paste0("text",UI)]] <- renderUI({
    lapply(1:length(dat2$AnalysisDataset$rules), function(x) {
      s2 <- dat2$AnalysisDataset$rules[[x]][2]
      names(s2) <- s2 %>% removePrefix %>% makeNice
      fluidRow(
        if(x > 1) {
          column(width = 12,
                 p(tags$b(dat2$AnalysisDataset$join[[x - 1]]))
          )
        },
        fluidRow(
          column(width = 4,
                 column(width = 12,
                        selectInput(inputId = paste0("zeroAnalysisDatasetRule",x,"FieldType"),
                                    label = "Select Field Type",
                                    choices = c("Demographic", "Clinical/LTCs", "Area", "Socio-economic (deprivation)", "Costs and Activity", "GlobalGroups"),
                                    selected = dat2$AnalysisDataset$rules[[x]][1]
                        )
                 )
          )
        ),
        column(width = 4,
               pickerInput(inputId = paste0("zeroAnalysisDataset", "Rule", x, "Field"),
                           label = "Select Field",
                           choices = segColNames,
                           selected = s2)
        ),
        column(width = 3,
               pickerInput(inputId = paste0("zeroAnalysisDataset", "Select",x),
                           label = "Select operator for highlighting field values",
                           choices = c("=", "in numeric range", ">=", "<=", "in (multiple choices select)"),
                           selected = dat2$AnalysisDataset$rules[[x]][3]
               )
        ),
        column(width = 3,
               uiOutput(paste0("zeroAnalysisDataset", "UI",x))
        ),
        column(width = 2,
               actionButton(paste0("zeroAnalysisDataset", "Rule",x,"Delete"), "Delete Field")
        )
      )
    })
  })
}

# clinOptions <- function(data) {
#   l <- sapply(data, function(x) {
#     length(strsplit(x, ".", fixed = TRUE)[[1]])
#   })
#   if(all(l > 2)) {
#     r <- sapply(data, function(x) {
#       s <- strsplit(x, ".", fixed = TRUE)[[1]]
#       paste0(s[-1], collapse = ".")
#     })
#     r2 <- sapply(r, function(x) {
#       s <- strsplit(x, ".", fixed = TRUE)[[1]]
#       s[length(s)]
#     })
#     names(r2) <- sapply(r, function(x) {
#       s <- strsplit(x, ".", fixed = TRUE)[[1]]
#       paste0(s[-length(s)], collapse = " ")
#     })
#     # Note: in the lapply, need to swap names and values
#     r3 <- lapply(unique(names(r2)), function(x) {
#       toReturn <- r2[which(names(r2) == x)]
#       # toReturnN <- names(toReturn)
#       # names(toReturn) <- toReturn
#       # toReturn <- toReturnN
#       toReturn
#     })
#     names(r3) <- unique(names(r2))
#     r3
#   } else {
#     makeNice(data)
#   }
# }

clinOptions <- function(data) {
  l <- sapply(data, function(x) {
    length(strsplit(x, ".", fixed = TRUE)[[1]])
  })
  if(all(l > 2)) {
    # Remove first value from colnames
    # then use 2nd-last-1th value to produce titles
    namesT <- sapply(data, function(x) {
      s <- strsplit(x, ".", fixed = TRUE)[[1]]
      paste0(s[-c(1,length(s))], collapse = " ")
    })
    
    r2 <- lapply(unique(namesT), function(x) {
      toR <- data[which(namesT == x)]
      names(toR) <- makeNice(names(toR))
      toR
    })
    names(r2) <- makeNice(unique(namesT))
# todo fix this; names have _ and shouldn't
    r2
  } else {
    # makeNice(data)
    data
  }
}
