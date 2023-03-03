# 
# 
# 
# ## DEBUG NOTE: MISSING MOST OF THE POPULATION - ACT_ doesn't contain
# act <- readRDS("c:/ExploreR/data/act_.rds")
# 
# d <- dat$attributes %>% select_at(c("id",dat2$groupByList%>%unlist))
# 
# act <- merge(d,act) %>% select(-id)
# 
# d <- lapply(dat2$groupByList[dat2$groupByList!=dat2$area]%>%unlist, function(i) {
#   act %>% group_by_at(c(dat2$actPodCols, dat2$area,i)) %>% summarise(cost = sum(cost),
#                                                                      activity = n())
# })
# 
# d <- lapply(d, function(x) {
#   x <- x %>% mutate(percent_cost = 100*cost/sum(cost),
#                     percent_activity = 100*activity/sum(activity))
#   x$actType <- sapply(1:nrow(x), function(y) {
#     data <- x[y, dat2$actPodCols]
#     paste0(data[!is.na(data)], collapse = ".")
#   })
#   
#   x
# })
# 
# names(d) <- dat2$groupByList[dat2$groupByList!=dat2$area]
# dat2$aggregated_utilisation <- d
# 
# column_variable <- dat2$groupByList[[1]]
# column_variable_text <- dat2$groupByListText[[1]]
# if(input_demog %in% c("age","age_sex")){
#   "att.demog.age_band_10"
# } else if(input_demog == "sex"){
#   "att.demog.sex"
# } else if(input_demog == "imd"){
#   "att.demog.imd_decile"
# } else {
#   stop("Demographic grouping input not recognised")
# }
# 
# y_axis_label <- if(percent == "percent"){
#   "Percent"
# } else if(percent=="raw" && input_activity_or_cost == "act"){
#   "Count"
# } else if(percent=="raw" && input_activity_or_cost == "cost"){
#   "Cost (\U0A3)"
# }
# percent = "percent"
# input_activity_or_cost = "cost"
# plot_data <- d[names(d) == column_variable][[1]] %>%
#   mutate(text =
#            if(percent=="percent" & input_activity_or_cost == "cost"){
#              paste0(round(percent_cost ,2),
#                     "% of ",
#                     actType, " spending",
#                     " in ",
#                     area.ICP,
#                     " was for patients in ",
#                     "\n",
#                     column_variable_text, " ",
#                     !!rlang::sym(column_variable))
#            } else if(percent=="raw" & input_activity_or_cost == "cost"){
#              paste0(actType, " spending",
#                     " in ",
#                     area.ICP,
#                     " was ", round(cost, 1),#"\U0A3",
#                     " for patients in ",
#                     "\n",
#                     column_variable_text, " ",
#                     !!rlang::sym(column_variable))
#              # paste0(facet_label,
#              #        " for ",
#              #        if(measure=="PCN"){att.demog.pcn} else if(comparator=="bnssg"){"BNSSG"} else {"All Other PCNs"},
#              #        " patients in ",
#              #        "\nAge band ",
#              #        !!rlang::sym(column_variable),
#              #        " cost \U0A3",
#              #        trimws(format(round(value,0),big.mark = ","))
#              # )
#            } else if(percent=="percent" & input_activity_or_cost == "activity"){
#              paste0(round(percent_activity ,2),
#                     "% of ",
#                     actType, " activity",
#                     " in ",
#                     area.ICP,
#                     " was for patients in ",
#                     "\n",
#                     column_variable_text, " ",
#                     !!rlang::sym(column_variable))
#            } else if(percent=="raw" & input_activity_or_cost == "activity"){
#              paste0("There were ",
#                     format(activity,big.mark = ","),
#                     " ",
#                     actType,
#                     " units of activity for ",
#                     area.ICP,
#                     " patients in ",
#                     "\n",
#                     column_variable_text, " ",
#                     !!rlang::sym(column_variable))
#            } else {"something went wrong"}
#          )
# 
#   plot <- 
#     plot_data %>% 
#     ggplot(aes_string(x=dat2$area,
#                y="cost",
#                text="text")) +
#     geom_col(position="dodge") +
#     coord_flip() +
#     facet_grid(rows=vars("actType"),
#                cols=vars(!!rlang::sym(column_variable))
#     ) +
#     theme_minimal() +
#     theme(axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           legend.position = "none",
#           axis.text.x = element_text(angle=-45),
#           strip.text.y = element_text(angle=-90)
#     ) #+
#     # scale_fill_manual(values = fill_colours)
#   
#   help_percent <- function(x){paste0(x,"%")}
#   help_pounds <- function(x){paste0("\U00A3",format(x,big.mark = ",",digits = 0,scientific = FALSE))}
#   
#   if(percent=="percent"){
#     plot <- plot + scale_y_continuous(labels=help_percent)
#   } else if(percent=="raw" && input_activity_or_cost == "act") {
#     plot <- plot + scale_y_continuous(labels=scales::comma)
#   } else if(percent=="raw" && input_activity_or_cost == "cost"){
#     plot <- plot + scale_y_continuous(labels=help_pounds)
#   } else {
#     plot <- plot
#   }
# ggplotly(plot,
#                   height=700,
#                   tooltip="text")
#   
#   
# 
