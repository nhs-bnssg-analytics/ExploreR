
segment_proportion_bars <- function(dat2,
                                    stacked,
                                    percent_or_freq,
                                    fill_colours = "default",
                                    # if displaying each segmenment separetely, the number of columns
                                    # in the plot grid - currently only works for max 2 columns
                                    number_of_columns=2,
                                    focusName){

  number_of_segments <- length(unique(dat2$util.segmentation.BtHSegment)) 
  library(scales)
  segment_colours <- if(fill_colours=="colourblind"){
    colorblind_pal()(number_of_segments)
  } else if(fill_colours=="default"){
    hue_pal()(number_of_segments)
  } else {
    brewer_pal(type="qual")(number_of_segments)
  }
  
  plot_dat <- dat2 %>% 
    mutate(text = if(percent_or_freq=="percent"){
      paste0(round(pcn_percent,2),
             "% of ",
             focusName, 
             " ",
             focus,
             " patients",
             "\nAre in the ",
             util.segmentation.BtHSegment,
             " segment")
    } else {
      paste0(format(pcn_freq,big.mark=","),
             " ",
             focusName, 
             " ",
             focus,
             " patients",
             "\nAre in the ",
             util.segmentation.BtHSegment,
             " segment")
    })
  
  plot <- plot_dat %>% 
    ggplot(aes(x=factor(focus),
               y=!!rlang::sym(paste0("pcn_",percent_or_freq)),
               fill=util.segmentation.BtHSegment,
               text=text
    ),
    ) +
    geom_col() +
    scale_fill_manual(values=segment_colours) +
    theme_minimal() +
    labs(x="",
         y="",
         fill = "Segments") +
    coord_flip() +
    scale_y_continuous(labels=ifelse(percent_or_freq=="percent",
                                     function(x) paste0(x,"%"),
                                     scales::comma))+
    scale_fill_viridis_d()
  
  if(stacked == "separate"){
    plot <- plot +
      facet_wrap(~util.segmentation.BtHSegment,
                 ncol = number_of_columns
      ) +
      theme(legend.position = "none",
            panel.spacing.y=unit(-0.5,"lines")
      )
  }
  return(ggplotly(plot,tooltip="text"))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


addActGraphs <- function(dat, dat2, listText, listCol, selectedGroup, acts, data) {
  # browser()
  # Calculates new graphs based on activity
  if(listText%in%dat2$groupByListText) {
    i <- which(dat2$groupByListText == listText)
  } else {
    i <- length(dat2$groupByListText) + 1
  }
  dat2$groupByListText[[i]] <<- listText
  dat2$groupByList[[i]] <<- listCol
  # print("Cohort gotten")
  
  updateSelectInput(session, "oneActVertical", choices = dat2$groupByListText[!is.na(dat2$groupByList)])
  updateSectionOneGroupBy(session, dat2)
  
  selectedGroup <- listCol
  acts <- unique(dat2$theorows$pod_l1)
  data <- dat$attributes %>% select_at(c("id", lapply(acts, function(x) {c(paste0("util.pod_l1.", x, ".cost"), paste0("util.pod_l1.", x, ".act"))}) %>% unlist(), selectedGroup)) %>% as.data.frame()
  
  print("No utils")
  dat2$actGraphs[[i]] <<-
    lapply(c("Total Cost", "Cost per Capita", "Activity", "Activity per Capita"), function(y) {
      print(y)
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
      
      if(y == "Total Cost") {
        data <- groupAndSum(data, selectedGroup, actOrCost = ".cost", sum)
      } else if (y == "Cost per Capita") {
        t <- data %>% group_by_at(selectedGroup) %>%
          summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), function(x) {length(which(x > 0))}) %>%
          pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), names_to = "pod_l1")
        data <- groupAndSum(data, selectedGroup, actOrCost = ".cost", sum)#
        data$cost <- data$cost / t$value
        # print(data)
      } else if (y == "Activity") {
        data <- groupAndSum(data, selectedGroup, actOrCost = ".act", sum)
      } else if (y == "Activity per Capita") {
        t <- data %>% group_by_at(selectedGroup) %>%
          summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), function(x) {length(which(x > 0))}) %>%
          pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), names_to = "pod_l1")
        
        data <- groupAndSum(data, selectedGroup, actOrCost = ".act", sum)
        data$cost <- data$cost / t$value
      }
      print("ifs")
      # Insert Total
      newdat <- data %>% group_by_at(selectedGroup) %>% summarise(total = sum(cost)) %>% ungroup() %>% as.data.frame()
      newdat <- cbind(rep("Total", nrow(newdat)), newdat)
      colnames(newdat) <- colnames(data)
      data <- rbind(newdat, data %>% as.data.frame())
      data[,"pod_l1"] <- (data[,"pod_l1"] %>% as.data.frame())[,1] %>% as.character() %>% makeNice()
      data$text <-
        paste(y, " is ", format(round(data[,"cost"],2), big.mark = ","), "\n",
              "At ", listText, " = ", data[,selectedGroup], "\n",
              "And POD ", data[,"pod_l1"], "\n",
              sep = ""
        )
      data
    })
  
}

bridges_to_health <- function(dat, field = NA) {
  dataOutput <- if(is.na(field)) {
    dat$attributes %>% 
      select(util.segmentation.BtHSegment,
             util.segmentation.BtHDefinition) %>% mutate(focus = "")
  } else {
    dat$attributes %>% 
      select(!!sym(field),
             util.segmentation.BtHSegment,
             util.segmentation.BtHDefinition
      ) %>% rename_at(field,~"focus")
  } 
  dataOutput %>%
    group_by(focus,
             util.segmentation.BtHSegment,
             util.segmentation.BtHDefinition) %>%
    summarise(pcn_freq = n()) %>% 
    ungroup() %>% 
    group_by(focus) %>% 
    mutate(pcn_percent = 100*pcn_freq/sum(pcn_freq)) %>% 
    ungroup() %>% 
    group_by(util.segmentation.BtHSegment,
             util.segmentation.BtHDefinition) %>% 
    mutate(bnssg_freq = sum(pcn_freq)) %>% 
    ungroup() %>% 
    group_by(focus) %>% 
    mutate(bnssg_percent = 100*bnssg_freq/sum(bnssg_freq)) %>% 
    ungroup() %>% 
    return()
}


addBtHGraph <- function(dat, dat2, listText, listCol) {
  # browser()
  if(listText%in%dat2$groupByListText) {
    # If already have BtH graph, replace corresponding tables
    i <- which(dat2$groupByListText == listText)
    print("Already Exists")
    print(i)
    if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
      bth <- dat2$bth
      bth[[i-1]] <- bridges_to_health(dat, listCol)
      return(bth)
    }
  } else {
    # If new, add to last position, shifting
    if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
      def <- dat2$bth[[length(dat2$bth)]]
      bth <- dat2$bth
      bth[[length(dat2$bth)]] <- bridges_to_health(dat, listCol)
      bth[[length(dat2$bth) + 1]] <- def
      
      i <- length(dat2$groupByListText) + 1
      print("New")
      print(i)
      return(bth)
    }

  }
}

getBtHGraph <- function(dat, dat2, col) {
  if("util.segmentation.BtHSegment"%in%colnames(dat$attributes)) {
    if(col == "") {
      bridges_to_health(dat, NA)
    } else {
      bridges_to_health(dat, dat2$groupByList[which(dat2$groupByListText == input$twoBTHFocus)][[1]])
    }
  }
}

