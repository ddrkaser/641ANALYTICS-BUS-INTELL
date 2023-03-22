library(DescTools)  #winsorize
library(stargazer)
library(dplyr)
library(tidyr)  # gather
library(ggplot2)
library(ggpubr)
library(lubridate)
library(gplots)
library(lfe)

b_m<- readRDS("F:/RenRen/r/December 22/output/bid_main_b2015_1223.rds")
b_m$CreditRating<- 8 - b_m$CreditRating

## loan level info
loan<- readRDS("F:/RenRen/r/December 22/output/loan.rds")

## loan level 
loan %>%
  group_by(irr) %>%
  summarize(Bid_Perc = mean(Bid_Perc, na.rm = TRUE)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Bid_Perc, y = irr)) +
  geom_point(color = "#999999") + 
  geom_smooth(method = "lm", size = 5, color = "#E69F00") + 
  #scale_color_manual(values = c("#999999", "#E69F00")) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(y = "IRR (%)", x = "Loan-Level Financing Percentage by Normal Type Investors") + 
  theme(text = element_text(size = 30))  

#### daily bid level normal type percentage
b_m %>% group_by(BidDate) %>%
  summarize(Perc_N_Bid = mean(normal, na.rm = T), irr = mean(irr, na.rm = T)) %>%
  ungroup() %>%
  group_by(Perc_N_Bid) %>%
  summarize(irr = mean(irr, na.rm = T)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Perc_N_Bid, y = irr)) +
  geom_point(color = "#999999") + 
  geom_smooth(method = "lm", size = 5, color = "#E69F00") + 
  #scale_color_manual(values = c("#999999", "#E69F00")) + 
  theme_minimal() + theme(legend.position = "none") + 
  labs(y = "IRR (%)", x = "Daily Percentage of Normal-Type Bids") + 
  theme(text = element_text(size = 30))  

### 1 hour after loan listings
b_m <- b_m %>% group_by(ProjectNo) %>%
  arrange(BidTime) %>%
  mutate(Perc_Funded = cumsum(BidAmount)/LoanAmount, last_bid_time = max(BidTime)) %>%
  as.data.frame()

b_m_1_hr<- b_m[which(difftime(b_m$BidTime, b_m$PubTime, units="hours") > 1),] # 42.88% of bids are after 1 hour

prj_tot<- data.frame() ### start from 140803
for (i in 140803:nrow(b_m)){
  bid_time <- b_m[i, "BidTime"]
  prior_project <- b_m[which(difftime(bid_time, b_m$PubTime, units = "days") > 0 & 
                               difftime(bid_time, b_m$PubTime, units = "days") < 7),]
  
  prj_byrating<- prior_project %>% group_by(ProjectNo) %>%
    filter((ProjectStatus != "已流标" & last_bid_time <= bid_time) | ProjectStatus == "已流标") %>%
    count(CreditRating) %>%
    ungroup() %>%
    count(CreditRating) %>%
    as.data.frame() %>%
    spread(CreditRating, n)
  
  prj_tot<- bind_rows(prj_tot, prj_byrating)
  prj_tot$row_num[i]<- i
    }

b_m$row_name<- 1:nrow(b_m) 
b_m2<- merge(b_m, prj_tot, by.x = 'row_name', by.y = 'row_num', all.x = T)


################## heterogeneous performance, top and bottom performance
## investments during an year, simple return: loan amount * irr
performance<- b_m %>% filter(normal == 1) %>%
  group_by(yr_m, BidderID) %>%
  summarize(tot_rtn = sum(BidAmount * irr, na.rm = T)) %>%
  ungroup() %>%
  group_by(yr_m) %>%
  mutate(quintile = ntile(tot_rtn, 5)) %>%
  ungroup() %>%
  group_by(yr_m, quintile) %>%
  summarize(avg_rtn = mean(tot_rtn, na.rm = T)) %>%
  as.data.frame() ## 1 is the lowest, while 5 is the higest
performance$quintile<- as.factor(performance$quintile)

ggplot(performance, aes(x = yr_m, y = avg_rtn/100, color = quintile)) + 
  geom_point(aes(color = quintile), size = 4) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#999999","#56B4E9","#009E73","#CC79A7","#E69F00")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "Investment Return ($)", x = "Date") + 
  theme(text = element_text(size = 30))  

###################
performance<- b_m %>% filter(normal == 1) %>%
  group_by(yr_m, BidderID) %>%
  summarize(tot_rtn = sum(BidAmount * irr, na.rm = T)) %>%
  ungroup() %>%
  group_by(yr_m) %>%
  mutate(quintile = ntile(tot_rtn, 5)) %>%
  as.data.frame() ## 1 is the lowest, while 5 is the higest
performance$quintile<- as.factor(performance$quintile)
fre <- data.frame(table(performance$BidderID))
### bid at least ten times
performance_long_user<- performance[performance$BidderID %in% fre[which(fre$Freq >= 10),]$Var1,]
### define good performer and bad performer: 50% or above belong to quintile 1 or quintile 5
pf1<- performance_long_user %>% group_by(BidderID) %>%
  count(quintile) %>%
  mutate(quintile_perc = n/sum(n)) %>%
  summarize(user_quintile = case_when(quintile == 5 & quintile_perc >= 0.5 ~ "top_quintile",
                                      quintile == 1 & quintile_perc >= 0.5 ~ "bottom_quintile")) %>%
  na.omit(user_quintile) %>%
  as.data.frame() %>%
  merge(performance_long_user, by = "BidderID") %>%
  group_by(user_quintile, yr_m) %>%
  summarize(avg_rtn = mean(tot_rtn, na.rm = T)) %>%
  as.data.frame()

ggplot(pf1, aes(x = yr_m, y = avg_rtn/100, color = user_quintile)) + 
  geom_point(aes(color = user_quintile), size = 4) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#999999","#E69F00")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  labs(y = "Investment Return ($)", x = "Date") + 
  theme(text = element_text(size = 30))  

############## switch behavior after 2013
#saveRDS(bid_main, 'F:/RenRen/r/December 22/output/bid_main_b2015_0106.rds')
b_m<- readRDS('F:/RenRen/r/December 22/output/bid_main_b2015_0106.rds')
b_m[,55:61]<- NULL

## lender characteristics
## the ratio of normal type bid to financial type bid, avg of normal bid, avg of financial bid
test<- bid_main[1:10,]
all_bidder<- unique(bid_main$BidderID)
bid_main$pri_normal_bid_avg<- NA
bid_main$pri_financial_bid_avg<- NA
bid_main$pri_normal_bids<- NA
bid_main$pri_financial_bids<- NA

for (i in 11335:length(all_bidder)){
  bid_history<- bid_main[which(bid_main$BidderID == all_bidder[i]),]
  row_num<- which(bid_main$BidderID == all_bidder[i])
  
  for (j in 1:length(row_num)){
    r<- row_num[j]
    all_bid_before<- bid_history[which(bid_history$Bid < bid_history$Bid[j]),]
    normal_bid_before<- all_bid_before[which(all_bid_before$normal == 1),]
    bid_main$pri_normal_bids[r]<- nrow(normal_bid_before)
    financial_bid_before<- all_bid_before[which(all_bid_before$normal == 0),]
    bid_main$pri_financial_bids[r]<- nrow(financial_bid_before)
    
    #if (!is.na(normal_bid_before)) {
      bid_main$pri_normal_bid_avg[r]<- mean(normal_bid_before$BidAmount, na.rm = T)
    #}
    #if (!is.na(financial_bid_before)) {
      bid_main$pri_financial_bid_avg[r]<- mean(financial_bid_before$BidAmount, na.rm = T) 
    #}
  }
  print(i)
}


aa<- find_mean_before(test)

b_m_after13<- bid_main[which(bid_main$year >= 2013),]


g1<- glm(switch_type~pridef*pri_normal, b_m_after13, family = binomial(link='logit'))
summary(g1)
g1_c<- glm(switch_type~pridef*pri_normal + log(pri_normal_bid_avg) + log(pri_financial_bid_avg) + log(pri_normal_bids) + 
             log(pri_financial_bids) + log(user_length_n+1), b_m_after13, family = binomial(link='logit'))
summary(g1_c)
g2<- glm(switch_toF~pridef*pri_normal, b_m_after13, family = binomial(link='logit'))
summary(g2)
g2_c<- glm(switch_toF~pridef*pri_normal + log(pri_normal_bid_avg) + log(pri_financial_bid_avg) + log(pri_normal_bids) + 
           log(pri_financial_bids) + log(user_length_n+1), b_m_after13, family = binomial(link = 'logit'))
summary(g2_c)

stargazer(g1,g1_c, g2,g2_c, align = T, no.space = T, keep.stat = c("n","adj.rsq"))

### Learning behaivor: loan characteristics
bid_main<- merge(bid_main, pf1, by = "BidderID", all.x = T)
bid_main$top_performer<- ifelse(bid_main$user_quintile == "top_quintile", 1, ifelse(bid_main$user_quintile == "bottom_quintile", 0, bid_main$user_quintile))
bid_main$bottom_performer<- ifelse(bid_main$user_quintile == "bottom_quintile", 1, ifelse(bid_main$user_quintile == "top_quintile", 0, bid_main$user_quintile))

lb1<- felm(irr~  pridef + log(pri_normal_bid_avg) + log(pri_financial_bid_avg) + log(pri_normal_bids) + 
             log(pri_financial_bids) + log(user_length_n+1) + APR  + log(LoanAmount) + RepaymentPeriod + CreditRating |yr_m + BidderID, data = bid_main)
summary(lb1)
lb2<- felm(irr ~  pridef*top_performer + log(pri_normal_bid_avg) + log(pri_financial_bid_avg) + log(pri_normal_bids)*top_performer + 
             log(pri_financial_bids) + log(user_length_n+1) + APR  + log(LoanAmount) + RepaymentPeriod + CreditRating |yr_m + BidderID, data = bid_main)
summary(lb2)
lb3<- felm(irr ~  pridef*bottom_performer + log(pri_normal_bid_avg) + log(pri_financial_bid_avg) + log(pri_normal_bids) + 
             log(pri_financial_bids)*bottom_performer + log(user_length_n+1) + APR  + log(LoanAmount) + RepaymentPeriod + CreditRating |yr_m + BidderID, data = bid_main)
summary(lb3)

stargazer(lb1,lb2, lb3, align = T, no.space = T, keep.stat = c("n","adj.rsq"))

############## loan performance and investor type
f_r1<- felm(irr~APR  + log(LoanAmount) + RepaymentPeriod + CreditRating + 
              CertifiMark + normal|yr_m + BidderID, data = bid_main)
summary(f_r1)
f_r2<- felm(irr~log(BidAmount) + APR  + log(LoanAmount) + RepaymentPeriod + CreditRating + 
              CertifiMark + Age + Education + married + loan_length +
              Income + hasHouse + houseLoan + hasCar + carLoan + male + normal|yr_m + BidderID, data = bid_main)
summary(f_r2)
f_r3<- felm(irr~log(BidAmount) + APR  + log(LoanAmount) + RepaymentPeriod + CreditRating + 
              CertifiMark + Age + Education + married + loan_length +
              Income + hasHouse + houseLoan + hasCar + carLoan + male + event, 
            data = subset(bid_main, normal == 1))
summary(f_r3)

###############
saveRDS(bid_main, 'F:/RenRen/r/December 22/output/bid_main_b2015_0110.rds')
bid_main<- readRDS('F:/RenRen/r/December 22/output/bid_main_b2015_0110.rds')
test = b_m[which(b_m$ProjectNo == 3),]
