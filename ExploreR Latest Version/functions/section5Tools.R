
# riskStratGroupSummaryTable <- function() {
#   
# }

riskStratGroupSummaryBoxPlots <- function(data, input) {
  ggplotly(
    data%>%ggplot(aes(x = paste0("Group ",factor(predicted_act_group)), y = predicted_act, group = predicted_act_group)) +
      geom_boxplot() +
      coord_flip() +
      ylab(input$twoRisk1Var2)+
      xlab("Stratified group")+
      ggtitle("Boxplots of stratified groups")
  )
}


util_risk_strat <- function(dat, data) {
  rep(0, nrow(dat$attributes))
  data$predicted_act_group <- ntile(data$predicted_act,10)
  d2 <- merge(dat$attributes%>%select(id), data%>%select_at(c("id", "predicted_act_group")), all.x = T, sort = F)
  d3 <- d2$predicted_act_group
  names(d3) <- d2$id
  d3[as.character(dat$attributes$id)]
}

throwModal <- function(message) {
  showModal(
    modalDialog(
      h2(message),
      footer = list(),
      easyClose = F
    )
  )
}
