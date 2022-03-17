## Activity cols are "nhs_number" "pod_l1"     "pod_l2a"    "pod_l2b"    "arr_date"   "dep_date"   "spec_l1a"   "cost1" (generalise for further pods)
#


process_act <- function(data) {

  # Assume date exists, and is in ymd format
  maxDate <- max(data$dep_date, na.rm = T)
  maxDate2 <- ymd(maxDate) - years(1)
  if(is.na(maxDate2)) {
    maxDate2 <- as.POSIXlt(as.Date(maxDate)) # "2021/02/29"
    maxDate2$year <- maxDate2$year-1
    maxDate2 <- as.Date(maxDate2)
  }
  
  # New plan for this function: save activity
  
  
  return(
     data %>% filter(.data[["dep_date"]] >= maxDate2) %>% group_by(id) %>%
    summarise(total_cost = sum(cost),
              total_act = n()) %>% 
    ungroup()
  )

  
}

















