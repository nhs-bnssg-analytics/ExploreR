addActGraphs <- function(dat, dat2, listText, listCol, selectedGroup, acts, data) {
  lapply(c("Total Cost", "Cost per Capita", "Activity", "Activity per Capita"), function(y) {
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
    # Insert Total
    newdat <- data %>% group_by_at(selectedGroup) %>% summarise(total = sum(cost)) %>% ungroup() %>% as.data.frame()
    newdat <- cbind(rep("Total", nrow(newdat)), newdat)
    colnames(newdat) <- colnames(data)
    data <- rbind(newdat, data %>% as.data.frame())
    data[,"pod_l1"] <- (data[,"pod_l1"] %>% as.data.frame())[,1] %>% as.character() %>% makeNice()
    data$text <-
      paste(y, " is ", format(data[,"cost"], big.mark = ","), "\n",
            "At ", listText, " = ", data[,selectedGroup], "\n",
            "And POD ", data[,"pod_l1"], "\n",
            sep = ""
      )
    data
  })
}




if(listText%in%dat2$groupByListText) {
  i <- which(dat2$groupByListText == listText)
  print("Already Exists")
  print(i)
  if("Segment"%in%colnames(dat$attributes)) {
    dat2$bth[[i-1]] <<- bridges_to_health(dat, listCol)
  }
} else {
  # browser()

  def <- dat2$bth[[length(dat2$bth)]]
  dat2$bth[[length(dat2$bth)]] <<- bridges_to_health(dat, listCol)
  dat2$bth[[length(dat2$bth) + 1]] <<- def
  
  i <- length(dat2$groupByListText) + 1
  print("New")
  print(i)
}

dat2$groupByListText[[i]] <<- listText
dat2$groupByList[[i]] <<- listCol
# print("Cohort gotten")

# updateSelectInput(session, "oneActVertical", choices = dat2$groupByListText[!is.na(dat2$groupByList)])
# updateSectionOneGroupBy(session, dat2)

selectedGroup <- listCol
acts <- unique(dat2$theorows$pod_l1)
data <- dat$attributes %>% select_at(c("id", lapply(acts, function(x) {c(paste0("util.pod_l1.", x, ".cost"), paste0("util.pod_l1.", x, ".act"))}) %>% unlist(), selectedGroup)) %>% as.data.frame()


dat2$actGraphs[[i]] <<- addActGraphs(dat, dat2, listText, listCol, selectedGroup, acts, data)
  # lapply(c("Total Cost", "Cost per Capita", "Activity", "Activity per Capita"), function(y) {
  #   sumn <- function(x) {
  #     n <- length(x)
  #     if(n == 0) {return(0)} else {return(sum(x)/n)} # total/ number of people in part
  #   }
  #   groupAndSum <- function(data, selectedGroup, actOrCost = ".cost", f) {
  #     # This needs a new function, passing cost+activity in pairs for scaling.
  #     data <- data %>% group_by_at(selectedGroup) %>% summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), f) %>%
  #       pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, actOrCost)})%>%unlist(), names_to = "pod_l1") %>%
  #       mutate(pod_l1 =
  #                sapply(pod_l1, function(y) {
  #                  s <- strsplit(y, ".", fixed = TRUE)[[1]]
  #                  s <- s[c(-1,-2,-length(s))]
  #                  s
  #                })
  #       )
  #     data<-data[,c("pod_l1",selectedGroup,colnames(data)[3])]
  #     colnames(data)[3]<-"cost"
  #     data
  #   }
  #   
  #   if(y == "Total Cost") {
  #     data <- groupAndSum(data, selectedGroup, actOrCost = ".cost", sum)
  #   } else if (y == "Cost per Capita") {
  #     t <- data %>% group_by_at(selectedGroup) %>%
  #       summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), function(x) {length(which(x > 0))}) %>%
  #       pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), names_to = "pod_l1")
  #     data <- groupAndSum(data, selectedGroup, actOrCost = ".cost", sum)#
  #     data$cost <- data$cost / t$value
  #     # print(data)
  #   } else if (y == "Activity") {
  #     data <- groupAndSum(data, selectedGroup, actOrCost = ".act", sum)
  #   } else if (y == "Activity per Capita") {
  #     t <- data %>% group_by_at(selectedGroup) %>%
  #       summarise_at(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), function(x) {length(which(x > 0))}) %>%
  #       pivot_longer(lapply(acts, function(x) {paste0("util.pod_l1.", x, ".act")})%>%unlist(), names_to = "pod_l1")
  #     
  #     data <- groupAndSum(data, selectedGroup, actOrCost = ".act", sum)
  #     data$cost <- data$cost / t$value
  #   }
  #   print("ifs")
  #   # Insert Total
  #   newdat <- data %>% group_by_at(selectedGroup) %>% summarise(total = sum(cost)) %>% ungroup() %>% as.data.frame()
  #   newdat <- cbind(rep("Total", nrow(newdat)), newdat)
  #   colnames(newdat) <- colnames(data)
  #   data <- rbind(newdat, data %>% as.data.frame())
  #   data[,"pod_l1"] <- (data[,"pod_l1"] %>% as.data.frame())[,1] %>% as.character() %>% makeNice()
  #   data$text <-
  #     paste(y, " is ", format(data[,"cost"], big.mark = ","), "\n",
  #           "At ", listText, " = ", data[,selectedGroup], "\n",
  #           "And POD ", data[,"pod_l1"], "\n",
  #           sep = ""
  #     )
  #   data
  # })