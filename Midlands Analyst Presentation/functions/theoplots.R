
constructHoverText2 <- function(row){
  # row is of form "id" "pod_l1"     "pod_l2"    "pod_l3"    "arr_date"   "dep_date"   "spec"   "cost"  with potentially more pod levels
  q = paste0(lapply(1:(length(row) - 5), function(x) {
    paste0("PODL",x, ": ", makeNice(row[1+x]), "<br>")
  }) %>% unlist())
  q = do.call(paste0, as.list(q))
  text = paste0(
    "Arrival: ", row["arr_date"], "<br>", 
    "Depart: ", row["dep_date"], "<br>",
    q,# "<br>",
    "Specialty: ",  makeNice(row["spec"]), "<br>",
    "Cost (£s): ", round(as.numeric(row["cost"]))
  )

  return(text)
}


plotPatientActivityPlotly2 <- function(df, dat2, dates, addLine = NULL) {
  # df is a df of patient activity
  # dat2$allPods
  # dat2$mintime <- min(dat$activity$arr_date)
  # dat2$maxtime <- min(dat$activity$dep_date)
  # FOR NOW, ASSUME PODL1 to be all the levels required
  df <- df %>% filter(arr_date >= dates[1] & dep_date <= dates[2])
  df[[colnames(df)[2]]] <- makeNice(df[[colnames(df)[2]]])
  # %>% removeScores
  colstring <- c("#E69F00","#800080", "#800080", "#0072B2", "#0072B2",
                 "#FF0000", "#FF0000", "#FF0000", "#0C9C09","#0C9C09")
  
  ntree <- dat2$theorows %>% as.data.frame()
  t <- sapply(1:nrow(ntree), function(i) {
    removeScores(ntree[i,]) %>% paste0(collapse = ".")
  })
  treeCols <- which(t %in% (get_selected(input$fourTheotree, format = "names") %>%
                              lapply(function(x){
                                paste0(c(attr(x, "ancestry"), x[1]), collapse = ".")
                              })%>%
                              unlist()))
  selected = ntree[treeCols,1]

  if(length(treeCols) < 1) {
    l <- length(dat2$allPods[[1]])
    cols <- dat2$allPods[[1]]%in%dat2$allPods[[1]]
  } else {
    cols <- (dat2$allPods[[1]]%>%unlist())%in%selected
    l <- length(c)
  }
  df_dummy <- data.frame(id = rep(1, l),
                         # This part will need to be changed to reflect plotting axis
                         # Refer to the shinytree for which of these to hide - more complicated setting option?
                         pod_l1 = (if(length(treeCols) < 1) {dat2$allPods[[1]]} else {dat2$allPods[[1]][cols]}) %>% as.character() %>% makeNice() %>% factor(
                           # ., levels = sort(as.character(.))
                           ),#
                         arr_date = rep(dates[1],l),#min(df$arr_date),dat2$mintime
                         dep_date = rep(dates[2],l)#max(df$dep_date),# dat2$maxtime
  )
  output <- plot_ly(df_dummy, color = ~pod_l1#, colors = colstring[length(unique(df_dummy$pod_l1))]
  ) %>%
    add_segments(x = ~arr_date, 
                 xend = ~dep_date,
                 y = ~pod_l1,
                 yend = ~pod_l1, 
                 hoverinfo = "none",
                 opacity = 0,
                 line = list(width = 1),
                 showlegend=F) %>%
    # add_markers(x = ~arr_date, # Transparent markers of dummy data
    #             y = ~pod_l1,
    #             
    #             opacity = 0,
    #             hoverinfo = "none",
    #             showlegend=F) %>%
    layout(yaxis=list(title="",
                      categoryorder = "array",
                      categoryarray = ~pod_l1), 
           xaxis=list(title="", range = c(dates[1],dates[2])
                      )
           )
  
  if(nrow(df) != 0){
    
    df <- cbind(df, "txt" = apply(df,1,constructHoverText2))
    
    output <- output %>%
      add_markers(x = ~arr_date,
                  y = ~pod_l1,
                  data = df,
                  text= ~txt,
                  # position=position_jitterdodge(),
                  # text= textvec,
                  hoverinfo = "text",
                  showlegend=F,
                  marker = list(size = 8)) %>%
      add_segments(x = ~arr_date, 
                   xend = ~dep_date,
                   y = ~pod_l1,
                   yend = ~pod_l1, 
                   hoverinfo = "none",
                   line = list(width = 1),
                   showlegend=F)
  }
  if(!is.null(addLine)) {
    output <- output %>% add_segments(x = addLine,
                                      xend = addLine,
                                      y = unique(df$pod_l1)[1],
                                      yend = unique(df$pod_l1)[length(unique(df$pod_l1))],
                                      text = "Marked Event",
                                      hoverinfo= 'text',
                                      hovertext = "Marked Event",
                                      line = list(color = 'rgb(250, 22,22)'),
                                      data = NULL,
                                      showlegend=F)
  }
  
  return(output)
}

getTree <- function(data) {
  if (!is.data.frame(data)) {
    a <- lapply(unique(data), function(y) {
      list(y)
    })
    names(a) <- unique(data) %>% removeScores
    a
  } else {
    a <-lapply(unique(data[,1]), function(x) {
      data[which(data[,1] == x & !is.na(data[,2])),-1] %>% getTree() %>%
        return()
    })
    names(a) <- unique(data[,1]) %>% removeScores
    a
  }
}

theo_attributes <- function(id, dat, dat2) {
  print(id)
  # Not in att, but in act?
  output$fourTheoSingleTheoAttributes <- renderText({
    info <- dat$attributes[which(dat$attributes$id == id),]
    
    HTML(paste(paste0("<b> Age: </b>", as.character(info[["util.age_band_10"]])),#info[[dat2$age]]), # 
               paste0("<b> Gender: </b>", info[[dat2$sex]]),
               paste0("<b> Deprivation Decile (1: most deprived): </b>", info[dat2$dep]),
               
               paste0("<b> Minor LTCs: </b>", paste0(dat2$simpleLTCCols[which(info[dat2$simpleLTCCols] > 0)] %>% removePrefix2 %>% makeNice(), collapse = " <br/> &emsp; &emsp; &nbsp;" )),
               paste0("<b> Complex LTCs: </b>", paste0(dat2$complexLTCCols[which(info[dat2$complexLTCCols] > 0)] %>% removePrefix2 %>% makeNice(), collapse = " <br/> &emsp; &emsp; &nbsp;" )),
               sep = '<br/>'))
  })
  
}

theoData <- function(id, dat, dat2, tree = input$fourTheotree, att = T) {
  
  r <- dat2$actLookUp[which(dat2$actLookUp$id == id),2]
  if(length(r) > 0) {
    if(r!=(-1)) {
      d <- readRDS(paste0("./data/theoAct/", r, "act_part.rds"))
      d <- d[d$id == id,]
    } else {
      d <- readRDS(paste0("./data/theoAct/", 1, "act_part.rds"))
      d <- d[d$id == id,]
    }
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
    
    d_names <- sapply(1:nrow(d), function(x) {
      data <- d[x, startsWith(colnames(d), "pod")]
      paste0(data, collapse = ".")
    }) %>% unlist()
    d <- d[d_names%in%selected,]
    # return(d)
  } else {
    # Need to return an empty dataframe
    d <- readRDS(paste0("./data/theoAct/", 1, "act_part.rds"))[F,]
    # d
  }
  
  output$fourTheoSingleTheoActivity <- renderUI({
    list(p(paste0(nrow(d), " instances of activity. Total cost: £", round(sum(d$cost),2))))
  })
  output$fourTheoSingleTheoActivityTable <- renderTable({
    d
  })
  if(att) {
    theo_attributes(id, dat, dat2)
  }
  
  return(d)
}



theoDataMulti <- function(ids, dat, dat2, tree = input$fourTheoMultiActivityToPlot) {
  r <- dat2$actLookUp[which(dat2$actLookUp$id %in% ids),2]
  if(length(r) > 0) {
    
    ntree <- dat2$theorows %>% as.data.frame()
    t <- sapply(1:nrow(ntree), function(i) {
      removeScores(ntree[i,]) %>% paste0(collapse = ".")
    })
    treeCols <- which(t %in% (get_selected(tree, format = "names") %>%
                                lapply(function(x){
                                  paste0(c(attr(x, "ancestry"), x[1]), collapse = ".")
                                })%>% unlist()))
    selected = ntree[treeCols,]
    selected = sapply(1:nrow(selected), function(x) {
      data <- selected[x, ]
      paste0(data, collapse = ".")
    }) %>% unlist()
    
    data <- lapply(unique(r), function(x) {
      # For each unique table, take the corresponding tables
      if(x!=(-1)) {
        d <- readRDS(paste0("./data/theoAct/", x, "act_part.rds"))
      } else {
        d <- readRDS(paste0("./data/theoAct/", 1, "act_part.rds"))
      }
      d <- d[d$id %in% ids,]
      d_names <- sapply(1:nrow(d), function(x) {
        data <- d[x, startsWith(colnames(d), "pod")]
        paste0(data, collapse = ".")
      }) %>% unlist()
      d <- d[d_names%in%selected,]
      d
    })
    d <- do.call(rbind, data)
    
  } else {
    d <- readRDS(paste0("./data/theoAct/", 1, "act_part.rds"))[F,]
  }
  
  return(d)
}



plotMultiLineTheo <- function(dat, dat2, input, id) {

  if(!is.null(input$fourTheoMultiActivityToPlot)) {
    tryCatch({
      ids <- id
      showModal(
        modalDialog(
          title = "Fetching Data",
          footer = list(),
          easyClose = F
        )
      )
      data <- theoDataMulti(ids, dat, dat2, input$fourTheoMultiActivityToPlot)
      if(nrow(data) != 0) {
        showModal(
          modalDialog(
            title = "Starting Plot",
            footer = list(),
            easyClose = F
          )
        )
        selected <- getSelectedShinyTreeValues(dat2, input$fourTheoMultiActivityToPlot)
        ntree <- selected[[2]]
        treeCols <- selected[[3]]
        selected <- selected[[1]]
        colours <- c("green", "blue", "yellow", "black", "red", "grey")
        gc()
        data2 <- cbind(data, "text" = apply(data,1,constructHoverText2))
        # In theory, fetching the data already filters to only relevant
        print("Has data")

        data2$filterNames <- sapply(1:nrow(data2), function(x) {
          paste0(data2[x,dat2$actPodCols] %>% as.character %>% removeScores, collapse = ".")
        })
        print("Colours")

        ntree <- dat2$theorows %>% as.data.frame()
        t <- sapply(1:nrow(ntree), function(i) {
          removeScores(ntree[i,]) %>% paste0(collapse = ".")
        })
        
        treeCols <- which(t %in% (get_selected(input$fourTheoMultiActivityToPlot, format = "names") %>%
                                    lapply(function(x){
                                      paste0(c(attr(x, "ancestry"), x[1]), collapse = ".")
                                    })%>%
                                    unlist()))
        
        data2$colour <- sapply(data2$filterNames, function(x) {
          # print(dat2$multiTheoColS[startsWith(x, dat2$multiTheoColS)])
          c("green", "blue", "yellow", "black", "red", "grey")[as.numeric(
          input[[paste0(dat2$multiTheoColS[startsWith(x, dat2$multiTheoColS)],  paste0(rep("a",dat2$multiTheoColSNum), collapse = ""))]]#, rep("a",dat2$multiTheoColSNum)
          )]
        }) %>% unlist()

        print("has Colours")

        data3 <- data2
        data3$id<-as.character(data3$id)
        
        data3[is.na(data3$dep_date),"dep_date"] <- data3[is.na(data3$dep_date),"arr_date"]
        print(Sys.time())
        output$fourTheoMultiPlot <- renderUI({
          plotlyOutput("fourTheoMultiPlotP", height = paste0(length(unique(data3$id)) * 35 + 80, "px"))
        })
        output$fourTheoMultiPlotText <- renderText({paste0("Please note ",input$fourTheoMultiPatentNum - length(unique(data3$id))," inviduals have no recorded activity, and are not plotted.")})
        # Need segments - anything that has endtime = NA will reflect it in the hovertext, but not on graph
        
        # This will need to be removed
        # if(!input$theoIDsShow) {
        tempDf <- data.frame(id = unique(data3$id), newid = 1:length(unique(data3$id)))
        data3$id <- sapply(data3$id, function(x) { tempDf$newid[which(tempDf$id == x)]}) %>% unlist() %>% as.character()
        # }
        showModal(
          modalDialog(
            title = "Rendering Plot",
            footer = list(),
            easyClose = F
          )
        )
        
        pal <- data3$colour #c("red", "green", "blue", "goldenrod", "magenta")
        pal <- setNames(pal, data3$filterNames)#data3$colour) # c("Europe", "Asia", "Americas", "Oceania", "Africa"))

        # c('#bcbd22','#d62728')
        # pal <- gsub(c("green", "red" ),c('#bcbd22','#d62728'),data3$colour)
        # pal <- gsub(c("red" ),c('#d62728'),pal)
        # '#1f77b4',  # muted blue
        # '#ff7f0e',  # safety orange
        # '#2ca02c',  # cooked asparagus green
        # '#d62728',  # brick red
        # '#9467bd',  # muted purple
        # '#8c564b',  # chestnut brown
        # '#e377c2',  # raspberry yogurt pink
        # '#7f7f7f',  # middle gray
        # '#bcbd22',  # curry yellow-green
        # '#17becf'   # blue-teal
        multiGraph <- plot_ly(data3, y = id, text = text, alpha = 0.5) %>%
            add_markers(x = ~arr_date,
                        y = ~id,
                        data = data3,
                        text= ~text,
                        hoverinfo = "text",
                        showlegend=F,
                        marker = list(color = pal, size = 8)
                   ) %>%
            add_segments(x = ~arr_date,
                         xend = ~dep_date,
                         y = ~id,
                         yend = ~id,
                         hoverinfo = "none",
                         line = list(width = 1),
                         showlegend=F,
                         # colour = pal,
                   colors = pal
                   ) %>%
            add_markers(x = ~dep_date,
                        y = ~id,
                        data = data3,
                        text= ~text,
                        hoverinfo = "text",
                        showlegend=F,
                        marker = list(color = pal, size = 8)
                   ) %>%
          layout(yaxis=list(title="Patients",
                            categoryorder = "array",
                            categoryarray = ~id),
                 xaxis=list(title="", range = input$dateidMulti
                 )
          )
        ###
        # browser()
        output$fourTheoMultiPlotP <- renderPlotly({
          multiGraph
          # ggplotly(
          #   # TODO need to set limits using date input for graph - type mismatch between date and POSIXct
          #   ggplot(data3, aes(y = id, text = text, alpha = 0.3), colour = colour) +
          #     geom_segment(aes(x = arr_date, y = id, xend = dep_date, yend = id), color = data3$colour) +
          #     geom_point(aes(x = arr_date, y = id), color = data3$colour) +
          #     geom_point(aes(x = dep_date, y = id), color = data3$colour) +
          #     xlab("") + ylab("Patient") +
          #     scale_x_datetime(date_breaks = "3 month", date_labels =  "%b %Y", limits = as.POSIXct(input$dateidMulti)) # FIX BEFORE RELEASE OR TYPE MISMATCH MAY CAUSE CRASHES
          #   , tooltip = "text")
        })
      } else {
        output$fourTheoMultiPlotText <- renderText({paste0("No activity found, sampled at ", Sys.time())})
        output$fourTheoMultiPlot <- renderUI({
          plotlyOutput("fourTheoMultiPlotP", height = paste0(50, "px"))
        })
        output$fourTheoMultiPlotP <- renderPlotly({
          plotly_empty()
          # Maybe render a blank plot over the selected time period instead?
        })
      }
      updateTabsetPanel(session, "multiOptions", selected = "Theoplot")
    }, error = function() {browser()})
  } else {
    output$fourTheoMultiPlotText <- renderText({"No activity type selected to be plotted (Options tab)."})
    output$fourTheoMultiPlot <- renderUI({
      plotlyOutput("fourTheoMultiPlotP", height = paste0(50, "px"))
    })
    output$fourTheoMultiPlotP <- renderPlotly({
      plotly_empty()
    })
  }
  removeModal()
}






