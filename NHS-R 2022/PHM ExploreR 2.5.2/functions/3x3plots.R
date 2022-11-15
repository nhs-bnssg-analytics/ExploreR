



getThreeByThreePlotsData <- function(data, input, fields = input$twoLTCAgeLTCSelection) {
  data$matrixFields <- data %>% select_at(fields) %>% rowSums()
  
  if(input$ltcSelection == "Major Conditions") {
    field <- "util.complex_ltc_sum"
  } else if(input$ltcSelection == "Minor Conditions") {
    field <- "util.simple_ltc_sum"
  } else {
    field <- "util.ltc_sum"
  }
  field <- "matrixFields"
  data <- data %>% 
    rename("demog.Age" = dat2$age) %>%
    select_at(c(field, "demog.Age", "total_cost")) %>%
    mutate(g.age=case_when(demog.Age<=input$clinAge1 & demog.Age >= 0 ~ paste0("Paeds (0-",input$clinAge1,")"),
                           demog.Age>input$clinAge1 & demog.Age <=input$clinAge2 ~ paste0("Adults (",input$clinAge1+1,"-",input$clinAge2,")"),
                           demog.Age>input$clinAge2 ~ paste0("Elderly (",input$clinAge2+1,"+)"),
                           is.na(demog.Age) ~ "unknown"))
  
  data$g.complexity <- case_when(data[[field]]<=input$clinLTC1 ~ "Low complexity",
                                 data[[field]]>input$clinLTC1 & data[[field]]<=input$clinLTC2 ~ "Middle complexity",
                                 data[[field]]>0 ~ "High complexity",
                                 is.null(data[[field]]) ~ "unknown")
  data
  
}


getThreeByThreePlots <- function(data, input) {

  plotdat <- data %>%
      group_by_at(vars(c("g.age","g.complexity")))%>%
      summarise(unscaled_population=n(),
                unscaled_total_spend=sum(total_cost),
                unscaled_spend_ppy=unscaled_total_spend/unscaled_population) %>%
      ungroup() %>%
      mutate(population=unscaled_population/max(unscaled_population),
             total_spend=unscaled_total_spend/max(unscaled_total_spend),
             #not clear how they're actually scaling the ppy - it's not like this
             spend_ppy=unscaled_spend_ppy/max(unscaled_spend_ppy))  %>%
      gather(key=measure,value=value,
             -g.age,
             -g.complexity,
             -unscaled_population,
             -unscaled_total_spend,
             -unscaled_spend_ppy) %>%
      gather(key=text,
             value=unscaled_value,
             -g.age,
             -g.complexity,
             -measure,
             -value) %>%
      filter(measure==str_remove(text,pattern = "unscaled_")) %>%
      mutate(measure=factor(measure,
                            levels=c("population","total_spend","spend_ppy"),
                            labels=c("population","total_spend","spend_ppy"),
                            ordered=TRUE),
             text=factor(text,
                         levels=c("unscaled_population","unscaled_total_spend","unscaled_spend_ppy"),
                         labels=c("unscaled_population","unscaled_total_spend","unscaled_spend_ppy"),
                         ordered=TRUE),
             g.age=factor(g.age,
                              levels=c(paste0("Paeds (0-",input$clinAge1,")"), paste0("Adults (",input$clinAge1+1,"-",input$clinAge2,")"), paste0("Elderly (",input$clinAge2+1,"+)")),#c("Paeds (0-17)","Adults (18-74)","Elderly (75+)"),
                              labels=c(paste0("Paeds (0-",input$clinAge1,")"), paste0("Adults (",input$clinAge1+1,"-",input$clinAge2,")"), paste0("Elderly (",input$clinAge2+1,"+)")),#c("Paeds (0-17)","Adults (18-74)","Elderly (75+)"),
                              ordered=TRUE)
             ,
             g.complexity=factor(g.complexity,
                                     levels=c("Low complexity","Middle complexity","High complexity","unknown"),
                                     labels=c("Low complexity","Middle complexity","High complexity","unknown"),
                                     ordered=TRUE)
             ,
             unscaled_value=ifelse(measure=="population",
                                   ifelse(round(unscaled_value,0) < 5,
                                          "<= 5",
                                          format(round(unscaled_value,0),big.mark = ",")),
                                   paste0(trimws(format(round(unscaled_value,0),big.mark = ","),"both")))
      )
  
    plot <- ggplot(plotdat,aes(x=measure,y=value,fill=measure, text = unscaled_value)) +
      geom_col() +
      geom_text(aes(x=measure,y=0,label=unscaled_value),
                vjust="outward",
                size=3) +
      facet_grid(g.complexity ~ g.age,
                 switch="y") +
      theme_bw() +
      scale_fill_manual(values=c("#00C19F","gray68","#00B9E3")) +
      ylim(-0.1,1) +
      theme(axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            plot.caption = element_text(size=12),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank()) +
      scale_x_discrete(labels=c("Population", "Total Spend (GBP)", "Spend PPY (GBP)"),
                       guide = guide_axis(n.dodge=3))

  plot

  
}



