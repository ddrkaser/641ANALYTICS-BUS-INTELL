library(survival)
library(survminer)
loan %>% group_by(ProjectNo) %>%
  summarize(bid_duration = difftime(PubTime, last_bid_time, units = "hours"), status = unique(ProjectStatus))
s<- Surv(loan$)