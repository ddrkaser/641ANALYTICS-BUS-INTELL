b_m<- readRDS("F:/RenRen/r/December 22/output/bid_main_b2015_0105.rds")
#b_m_1m<- b_m[1:1000000,]
#b_m_2m<- b_m[1000001:2000000,]
#b_m_3m<- b_m[2000001:3000000,]
#b_m_4m<- b_m[3000001:4000000,]
#b_m_5m<- b_m[4000001:nrow(b_m),]
#saveRDS(b_m_5m, "F:/RenRen/output/available projects/b_m_5m.rds")

prj_tot<- data.frame() 

for (i in 1:2){
  bid_time <- b_m[i, "BidTime"]
  prior_project <- b_m[which(difftime(bid_time, b_m$PubTime, units = "days") > 0 & 
                               difftime(bid_time, b_m$PubTime, units = "days") < 7),]
  
  prj_byrating<- prior_project %>% group_by(ProjectNo) %>%
    filter((ProjectStatus != "已流标" & last_bid_time >= bid_time) | ProjectStatus == "已流标") %>%
    count(CreditRating) %>%
    ungroup() %>%
    count(CreditRating) %>%
    as.data.frame() %>%
    spread(CreditRating, n)
  
  prj_tot<- bind_rows(prj_tot, prj_byrating)
  prj_tot$row_num[i]<- i
}

b_m$row_num<- 1:nrow(b_m) 
b_m2<- merge(b_m, prj_tot, by = 'row_num', all.x = T)

