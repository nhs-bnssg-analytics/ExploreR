




constructHoverText <- function(row){
  # row is of form "id" "pod_l1"     "pod_l2"    "pod_l3"    "arr_date"   "dep_date"   "spec"   "cost"  with potentially more pod levels
  
  podn <- length(row) - 5
  
  text = paste0(
    "Arrival: ", row["arr_date"], "<br>", 
    "Depart: ", row["dep_date"], "<br>",
    lapply(1:podn, function(x) {
      psate0("PODL",x, ": ", row[1+x])
    }) %>% unlist(), "<br>",
    "Specialty: ", row["spec"], "<br>",
    "Cost (£s): ", round(as.numeric(row["cost"]))
  )
  
  return(text)
}



plotPatientActivityPlotly <- function(df, dat2){
  # df is a df of patient activity
  # dat2 is dat2
  
  # dat2$allPods
  # dat2$mintime <- min(dat$activity$arr_date)
  # dat2$maxtime <- min(dat$activity$dep_date)
  # FOR NOW, ASSUME PODL1 to be all the levels required
  
  
  colstring <- c("#E69F00","#800080", "#800080", "#0072B2", "#0072B2",
                 "#FF0000", "#FF0000", "#FF0000", "#0C9C09","#0C9C09")
  
  # xmin
  # xmax
  dat2$mintime <- min(dat$activity$arr_date)
  dat2$maxtime <- min(dat$activity$dep_date)
  
  l <- length(dat2$allPods)
  
  df_dummy <- data.frame(id = rep(1, l),
                         # This part will need to be changed to reflect plotting axis
                         podl1 = dat2$allPods %>% as.character(),
                         arr_date = rep(dat2$mintime,l),
                         dep_date = rep(dat2$mintime,l)
  )
  
  output <- plot_ly(df_fill, color = ~podl1, colors = colstring[length(df_dummy$podl1)]) %>%
    add_markers(x = arr_date, # Transparent markers of dummy data
                y = ~podl1,
                opacity = 0,
                hoverinfo = "none",
                showlegend=F) %>%
    layout(yaxis=list(title=""), 
           xaxis=list(title="", range = c(dat2$mintime, dat2$maxtime)))
  
  if(nrow(df) != 0){
    df <- cbind(df, "txt" = apply(df,1,constructHoverText))
    
    output <- output %>%
      add_markers(x = ~arr_date,
                  y = ~podl1,
                  data = df,
                  text= ~txt,
                  text= textvec,
                  hoverinfo = "text",
                  showlegend=F,
                  marker = list(size = 8)) %>%
      add_segments(x = ~arr_date, 
                   xend = ~dep_date,
                   y = ~podl1,
                   yend = ~podl1, 
                   hoverinfo = "none",
                   line = list(width = 1),
                   showlegend=F) %>% plotDQ()
  }
  return(output)
}

