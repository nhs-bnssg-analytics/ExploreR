poppyramid <- function(data, group, clinBar, count = TRUE, stacked = TRUE, input) {
  # scale_colour_discrete <- scale_colour_colorblind
  # scale_fill_discrete <- scale_colour_colorblind
  
  count <- if(count == "TRUE") {TRUE} else {FALSE}
  stacked <- if(stacked == "TRUE") {TRUE} else {FALSE}
  # data needs to have columns demog.age, demog.sex and group (if group isn't NA)
  
  if(!is.na(group)) {
    plotData <- data %>% 
      subset(demog.sex %in% c("male", "female")) %>% 
      group_by_at(
        if(clinBar == "") {
          c("demog.age",
            "demog.sex",
            group)
        } else {
          c("demog.age",
            "demog.sex",
            group,
            clinBar)
        }) %>%
      count(name = "freq") %>%
      mutate(freq=ifelse(demog.sex == "male", yes = freq, no = -freq))
    if(!count) {
      plotData <- plotData %>% mutate(freq = freq/nrow(data)*100)
    }
    plotData <- plotData  %>%
      mutate(text_freq=ifelse(abs(freq)<=if(count){5} else {0.1},"*suppressed",trimws(format(abs(freq),big.mark = ","),"both"))) %>%
      as.data.frame()
    plotData[,group] <- plotData[,group] %>% factor()
    
  } else {
    plotData <- data %>% 
      subset(demog.sex %in% c("male", "female")) %>% 
      group_by_at(if(clinBar == "") {
        c("demog.age",
          "demog.sex")
      } else {
        c("demog.age",
          "demog.sex",
          clinBar)
      }
      ) %>%
      count(name = "freq") %>%
      mutate(freq=ifelse(demog.sex == "male", yes = freq, no = -freq)) 
    if(!count) {
      plotData <- plotData %>% mutate(freq = freq/nrow(data)*100)
    }
    plotData <- plotData  %>%
      mutate(text_freq=ifelse(abs(freq)<=if(count){5} else {0.1},"*suppressed",trimws(format(abs(freq),big.mark = ","),"both"))) %>%as.data.frame()
  }
  
  if(clinBar != "") {
    plotData <- plotData %>% mutate_at(clinBar, function(x) {case_when(
      x == 1 ~ "Yes",
      x == 0 ~ "No"
    )})
  }
  source("./functions/section1Tools.R", local = TRUE)
  # Generate hovertext pre-plot
  plotData$text <- 
    if(is.na(group)) {
      paste("There are ", plotData[,"text_freq"], if(count){" "} else {"% "}, plotData[,"demog.age"] , "-year-old ", plotData[,"demog.sex"], "s <br>",
            if(clinBar == "") {""} else {paste0(sapply(plotData[,clinBar], function(x) {if (x == "Yes") {" with "} else {" without "}}),makeNice(removePrefix(clinBar)))},
            sep = "")
    } else {
      paste("There are ", plotData[,"text_freq"], if(count){" "} else {"% "}, plotData[,"demog.age"] , "-year-old ", plotData[,"demog.sex"], "s <br> in ",
            input$oneDemogGroupBy, " ", plotData[,group],
            if(clinBar == "") {""} else {paste0(sapply(plotData[,clinBar], function(x) {if (x == "Yes") {" with "} else {" without "}}),makeNice(removePrefix(clinBar)))},
            sep = "")
    }
  if(clinBar != "") {
    plotData$clinBar <- plotData[,clinBar]
  }

  ## ggplot looks better than plotly here - consider swapping for it?
  ###### NOTE: opacity is disabled by ggplotly. Fix unknown
  
  plotData$demog.sex <- makeNice(plotData$demog.sex)


  #####################################
  # browser()
  plotData[is.na(plotData)] <- "unknown"
  
  plots <- ((if(!stacked) {split(plotData, 
                 f = if(clinBar == "" & is.na(group)) {
                   "1"
                 } else if(clinBar != "" & is.na(group)) {
                   plotData$clinBar
                 } else if(clinBar == "" & !is.na(group)) {
                   plotData[[group]]
                 } else {list(plotData[[group]], plotData$clinBar)})} else {list(plotData)}) %>%
    lapply(function(x){
      a <- list(
        x = 0,
        y = max(plotData$demog.age),
        text = if(!stacked) {
          if(clinBar != "" & !is.na(group)) {
            paste0(makeNice(removePrefix(group)), "=", sprintf("<b>%s</b>", unique(x[[group]])), " and has ",makeNice(removePrefix(removePrefix(clinBar))), "=",sprintf("<b>%s</b>", unique(x[[clinBar]])))
          } else if(clinBar != "" & is.na(group)) {
            paste0(makeNice(removePrefix(removePrefix(clinBar))), "=",sprintf("<b>%s</b>", unique(x[[clinBar]])))
          } else if(clinBar == "" & !is.na(group)) {
            paste0(makeNice(removePrefix(removePrefix(group))), "=", sprintf("<b>%s</b>", unique(x[[group]])))
          } else {" "}
        } else {(sprintf("<b>%s</b>", "Female                   Male"))},
        xref = "x",
        yref = "y",
        showarrow = F,
        arrowhead = 7,
        ax = 0,
        ay = 0
      )
      # print(a)
      y <- .[[length(.)]]
      plot_ly(x,
              y = ~demog.age,
              x = ~ freq,
              color = if(!stacked) {~demog.sex} else {if(!is.na(group) & clinBar != "") {~clinBar} else if(!is.na(group) & stacked) {~get(group)} else if(clinBar != "") {~clinBar} else {~demog.sex}},
              legendgroup = if(clinBar == "" & is.na(group)) {
                ~demog.sex
              } else if(clinBar != "" & is.na(group)) {
                ~clinBar
              } else if(clinBar == "" & !is.na(group)) {
                ~get(group)
              } else {~clinBar},
              orientation = "h",
              text = ~text,
              hoverinfo="text"#,
              # colorscale='Viridis'
              ) %>%
        add_bars(opacity=0.5,
                 width=1,
                 showlegend = identical(x, y)) %>%
        layout(
          annotations = a,
          barmode = if(stacked) {"relative"},
          xaxis=list(tickvals=pretty(x$freq),
                     ticktext=paste0(scales::comma(abs(pretty(x$freq))),if(!count) {"%"} else {""}),
                     title=if(!count){"Percentage"} else {"Count"}),
          yaxis=list(title="Age"),
          legend=list(orientation="h"))
    }))
  # browser()
  pl <- if(!stacked) {
    plots[if(clinBar != "" & !is.na(group)) {#Both
      rbind(1:(length(plots)/2), (length(plots)/2)+(1:(length(plots)/2))) %>% as.vector
      # c(seq(1,length(plots),2), seq(2,length(plots),2))
    } else {1:length(plots)}] %>%
      subplot(nrows = 
                if(clinBar != "") {
                  ceiling(length(plots)/2)
                } else if(!is.na(group)) {
                  ceiling(length(plots)/2)
                } else {1}
              , shareY = T
      )
  } else {
    plots[[1]]
  }

  pl%>% return()
}




