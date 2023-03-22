library(fixest)
library(lfe)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
library(ggiplot)
library(ggpubr)
theme_set(theme_pubr())

### original data
bk<- read.csv('F:/fintech/output/Dec 2022/revisit1/6_cty_control_match2.csv')
bk$fips_year<- paste(bk$fips, bk$year, sep = "")
bk$branch_drop<- ifelse(!is.na(bk$year_dec), 1, 0)
bk$post_drop<- bk$year - bk$year_dec
bk$treat_post<- ifelse(!is.na(bk$year_dec) & bk$year > bk$year_dec, 1, 0)
  
app<- unique(bk[,c("instiID", "app_year")]) %>%
  count(app_year) %>%
  #mutate(Counts = cumsum(n)) %>%
  filter(!is.na(app_year))  %>%
  as.data.frame() %>%
  ggplot(aes(x = factor(app_year), y = n)) +
  geom_bar(fill = "#E69F00", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, size = 10) +  theme_pubclean() +
  labs(y = "# Banks That Have Mobile Apps", x = "Year") + 
  theme(text = element_text(size = 25)) 
app

branch<- unique(bk[which(bk$num_br_diff < 0),c("fips_instiID", 'year')]) %>%
  count(year) %>%
  filter(!is.na(year))  %>%
  as.data.frame() %>%
  ggplot(aes(x = factor(year), y = n)) +
  geom_bar(fill = "#E69F00", stat = "identity") +
  geom_text(aes(label = n), vjust = -0.3, size = 10) +  theme_pubclean() +
  labs(y = "# Branch Closing Events", x = "Year") + 
  theme(text = element_text(size = 25)) 
branch

bk[is.na(bk$app_year),]$app_year<- 2222
within_same_county<- bk[which(bk$num_br_diff < 0),c('year', 'year_dec_correct', 'fips_instiID', 'fips', 'app_year', 'branch_cty', 'deposit_cty')] 
target_control<- bk[bk$fips %in% within_same_county$fips,]


for (i in 1:nrow(within_same_county)){
  df<- target_control[which(is.na(target_control$year_dec_correct) & target_control$year == within_same_county$year[i] & target_control$fips == within_same_county$fips[i]),]
  if (nrow(df) > 0){
    if (within_same_county$app_year[i] <= within_same_county$year_dec_correct[i]){
      df<- df[which(df$app_year <= df$year_dec_correct),]
    } else {df<- df[which(df$app_year > df$year_dec_correct),]} 
      if (nrow(df) > 0){
        within_same_county$matched[i]<- list(df$fips_instiID)
    }
  }
}


bk[bk$fips %in% within_same_county$fips,] %>%
  select(fips_instiID, fips) %>%
  unique() %>%
  count(fips) %>%
  as.data.frame() %>%
  ggplot(aes(x = n)) +
  geom_histogram(position="identity", alpha=0.5)
  
test<- bk[which(bk$fips_instiID == 1703112311),]                              


############## matched
bk_m<- read.csv("F:/fintech/output/Dec 2022/revisit1/6_cty_match_bank14 (w_replacement_app).csv")
bk <- da2
bk$SB_Amt_Orig_m<- bk$SB_Amt_Orig/1000
bk$SB_Amt_Orig_TILow_m<- bk$SB_Amt_Orig_TILow/1000
bk$SB_Amt_Orig_TIUpp_m<- bk$SB_Amt_Orig_TIUpp/1000
bk$SB_Amt_Orig_TIMod_m<- bk$SB_Amt_Orig_TIMod/1000
bk$SB_Amt_Orig_TIMid_m<- bk$SB_Amt_Orig_TIMid/1000
bk$deposit_cty_m<- bk$deposit_cty/1000
bk$tot_deposit_cty_m<- bk$tot_deposit_cty/1000
bk$tot_SB_Amt_Orig_cty_m<- bk$tot_SB_Amt_Orig_cty/1000
bk$SB_Amt_share<- bk$SB_Amt_Orig/bk$tot_SB_Amt_Orig_cty
bk$fips_year<- paste(bk$fips, bk$year, sep = "")
bk<- bk %>%
  group_by(fips_bank) %>%
  arrange(year) %>%
  mutate(SB_Amt_Orig_diff = (SB_Amt_Orig - lag(SB_Amt_Orig + 1))/lag(SB_Amt_Orig + 1), 
         SB_Loan_Orig_diff = (SB_Loan_Orig - lag(SB_Loan_Orig + 1))/lag(SB_Loan_Orig + 1),
         deposit_cty_diff = (deposit_cty - lag(deposit_cty))/lag(deposit_cty)) %>%
  data.frame()
bk$treat_post<- ifelse(!is.na(bk$year_dec) & bk$year > bk$year_dec, 1, 0)

f1<- felm(SB_Amt_Orig_m~ treat_post*have_app2 + branch_cty + deposit_cty_m + 
            loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID|0|instiID, data = bk)
f1_1<- felm(SB_Loan_Orig~ treat_post*have_app2 + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID|0|instiID, data = bk)
f1_2<- felm(SB_Amt_Orig_diff ~ treat_post*have_app2 + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID|0|instiID, data = bk)
f1_3<- felm(SB_Loan_Orig_diff  ~ treat_post*have_app2 + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID|0|instiID, data = bk)
f1_4<- felm(log(SB_Amt_Orig+1) ~ treat_post*have_app2 + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID|0|instiID, data = bk)
stargazer(f1,f1_1, f1_2, f1_3, f1_4,no.space = T,keep.stat = c("n", "adj.rsq"), type = "text")

f2<- felm(SB_Amt_Orig_m~ treat_post*have_app|fips_year + instiID, data = bk)
f2_1<- felm(SB_Amt_Orig_m~ treat_post*have_app + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID, data = bk)
f2_2<- felm(SB_Amt_share ~ treat_post*have_app|fips_year + instiID, data = bk)
f2_3<- felm(SB_Amt_share ~ treat_post*have_app + branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID, data = bk)
stargazer(f2, f2_1, f2_2, f2_3, no.space = T,keep.stat = c("n", "adj.rsq"))



f1_bank<- felm(SB_Amt_Orig_m~ treat_post*have_app + 
              branch_cty + deposit_cty_m + 
              loan_asset + equity_asset + size + roa + log_wage|fips_year + instiID, data = bk)
f1_bank2<- felm(SB_Amt_Orig_m~ treat_post*have_app + 
                 branch_cty + deposit_cty_m + 
                 loan_asset + equity_asset + size + roa + log_wage + 
                 Unemployment_rate + log(HPI) + log(Total_population)|fips_instiID + year, data = bk)
summary(f1_v1)
stargazer(f1, f1_bank,f1_bank2,no.space = T,keep.stat = c("n", "adj.rsq"))
          

f1_low_income<- felm(SB_Amt_Orig_TILow_m ~ treat_post*have_app , data = bk)
f1_mid_income<- felm(SB_Amt_Orig_TIMid_m ~ treat_post*have_app , data = bk)
f1_mod_income<- felm(SB_Amt_Orig_TIMod_m ~ treat_post*have_app , data = bk)
f1_upp_income<- felm(SB_Amt_Orig_TIUpp_m ~ treat_post*have_app , data = bk)

stargazer(f1_low_income,f1_mid_income,f1_mod_income,f1_upp_income,no.space = T, 
          keep.stat = c("n", "adj.rsq"), type = "text")


#### difference between with and without app
bk_same_app_yr<- unique(drop_wo_app[,c('fips_instiID', 'year_dec', 'event','treatment', 'app_year')])

drop_w_app<- bk[which(bk$treatment == 1 & bk$app_year <= bk$year_dec),]
drop_w_app_event<- unique(drop_w_app$event)
drop_w_app<- bk[bk$event %in% drop_w_app_event,]
drop_w_app2<- drop_w_app %>%
  group_by(event, year) %>%
  arrange(treatment) %>%
  summarize(sml_amt = diff(SB_Amt_Orig_m), 
            sbl_low = diff(SB_Amt_Orig_TILow_m),
            sbl_mod = diff(SB_Amt_Orig_TIMod_m),
            sbl_mid = diff(SB_Amt_Orig_TIMid_m),
            sbl_upp = diff(SB_Amt_Orig_TIUpp_m),
            deposit_cty_m = diff(deposit_cty_m)) %>%
  as.data.frame() 

event_year<- unique(bk[,c('event', 'year', 'post')])
event_year<- event_year[!is.na(event_year$post),]
drop_w_app2<- merge(drop_w_app2, event_year, by = c('event', 'year'), all.x = T)
test<- bk[which(bk$event == 1),c("year", 'fips_instiID', 'treatment', 'SB_Amt_Orig_m')]

drop_wo_app<- bk[which(bk$treatment == 1 & bk$app_year > (bk$year_dec + 4)),]
drop_wo_app_event<- unique(drop_wo_app$event)
drop_wo_app<- bk[bk$event %in% drop_wo_app_event,]
drop_wo_app2<- drop_wo_app %>%
  group_by(event, year) %>%
  arrange(treatment) %>%
  summarize(sml_amt = diff(SB_Amt_Orig_m), 
            sbl_low = diff(SB_Amt_Orig_TILow_m),
            sbl_mod = diff(SB_Amt_Orig_TIMod_m),
            sbl_mid = diff(SB_Amt_Orig_TIMid_m),
            sbl_upp = diff(SB_Amt_Orig_TIUpp_m)) %>%
  as.data.frame() 
drop_wo_app2<- merge(drop_wo_app2, event_year, by = c('event', 'year'), all.x = T)


drop<- bk %>%
  group_by(event, year) %>%
  arrange(treatment) %>%
  summarize(sml_amt = diff(SB_Amt_Orig_m), 
            sbl_low = diff(SB_Amt_Orig_TILow_m),
            sbl_mod = diff(SB_Amt_Orig_TIMod_m),
            sbl_mid = diff(SB_Amt_Orig_TIMid_m),
            sbl_upp = diff(SB_Amt_Orig_TIUpp_m),
            deposit_cty_m = diff(deposit_cty_m)) %>%
  as.data.frame() 
drop2<- merge(drop, event_year, by = c('event', 'year'), all.x = T)

f1<- feols(deposit_cty_m ~ i(post, ref = -1, keep = -5:5)|event, data = drop_w_app2)
f_wo_app<- feols(deposit_cty_m ~ i(post, ref = -1, keep = -5:5)|event, data = drop2)
iplot(list(f_wo_app, f1), xlim = c(-5,5), col = c("#E69F00", "#999999"), xlab = "Year Relative to Bank Branch Closings", 
      ylab = "Coefficient",main = " ", ci.lwd = 6, 
      cex = 5, ref.line = TRUE, 
      ref.line.par = list(col = "#009E73", lwd = 6))
iplot(f1)
iplot(f_wo_app)
#x = list(-5.5, -5.5), y = list(-45, -50)
#x = list(-5.5, -5.5), y = list(-2.5, -2)
##############
bk$branch_drop<- ifelse(!is.na(bk$year_dec), 1, 0)
bk$post_drop<- bk$year - bk$year_dec
bk_balanced<- bk %>%
  filter(deposit_cty > 0) %>%
  group_by(fips_instiID) %>%
  arrange(year) %>%
  filter(first(branch_cty) > 3 & first(year) > 2015) 
bk_balanced2<- bk[!bk$fips_instiID %in% bk_balanced$fips_instiID,]

bk$post_app<- bk$year - bk$app_year
bk<- bk %>%
  group_by(fips_instiID) %>%
  arrange(year) %>%
  mutate(SB_Amt_Orig_diff = (SB_Amt_Orig_m - lag(SB_Amt_Orig_m))/lag(SB_Amt_Orig_m + 1), 
         SB_Loan_Orig_diff = (SB_Loan_Orig - lag(SB_Loan_Orig))/lag(SB_Loan_Orig + 1),
         deposit_cty_diff = (deposit_cty - lag(deposit_cty))/lag(deposit_cty + 1)) %>%
  as.data.frame()
bk$app_after4g<- ifelse(bk$app_year >= "2014", 1, 0)


f3<- feols(SB_Amt_Orig_TILow_m ~ i(post_app, ref = -1, keep = -5:5)|fips_year + instiID, cluster = c("instiID", "year"), data = bk_balanced2)
iplot(f3, xlim = c(-5,5), col = "#E69F00", xlab = "Year Relative to Bank Mobile App Release", 
      ylab = "Coefficient",main = " ", ci.col = "#999999", ci.lwd = 6, 
      cex = 10, ref.line = TRUE, 
      ref.line.par = list(col = "#009E73", lwd = 6)) 

bk_balanced2$SB_Amt_Orig_m<- bk_balanced2$SB_Amt_Orig/1000
bk_balanced2$SB_Amt_Orig_TILow_m<- bk_balanced2$SB_Amt_Orig_TILow/1000
bk_balanced2$SB_Amt_Orig_TIUpp_m<- bk_balanced2$SB_Amt_Orig_TIUpp/1000
bk_balanced2$SB_Amt_Orig_TIMod_m<- bk_balanced2$SB_Amt_Orig_TIMod/1000
bk_balanced2$SB_Amt_Orig_TIMid_m<- bk_balanced2$SB_Amt_Orig_TIMid/1000



bk<- bk_m
f1_num<- felm(SB_Loan_Orig~ treat_post*have_app|fips_instiID + year, data = bk)
f1_amt_diff<- felm(SB_Amt_Orig_diff ~ treat_post*have_app|fips_year + instiID, data = bk)
f1_num_diff<- felm(deposit_cty_diff~ treat_post*have_app|fips + year + instiID, data = bk)

summary(f1_num_diff)




f1_low_income<- feols(deposit_cty_diff ~ i(post_app,ref = 0, keep = -5:5)|fips_year + instiID, data = bk2)
iplot(f1_low_income, xlim = c(-5,5))

f1_mod_income<- feols(SB_Amt_Orig_TILow_m~ i(post_app,ref = -1, keep = -5:5) + log(deposit_cty) + branch_cty + 
                        loan_asset + equity_asset + size + roa + log_wage + log(tot_deposit_cty) + tot_branch_cty + 
                        Unemployment_rate + Unemployment_rate + log(HPI) + log(Total_population)|year_fips + instiID, data = sbl_tech)
f1_mid_income<- feols(log(SB_Amt_Orig_TIMid + 1)~ i(event_window,ref = -1, keep = -5:5) + log(deposit_cty) + branch_cty + 
                        loan_asset + equity_asset + size + roa + log_wage + log(tot_deposit_cty) + tot_branch_cty + 
                        Unemployment_rate + Unemployment_rate + log(HPI) + log(Total_population)|year_fips + instiID, data = sbl_tech)
f1_high_income<- feols(log(SB_Amt_Orig_TIUpp + 1)~ i(event_window, ref = -1, keep = -5:5) + log(deposit_cty) + branch_cty + 
                         loan_asset + equity_asset + size + roa + log_wage + log(tot_deposit_cty) + tot_branch_cty + 
                         Unemployment_rate + Unemployment_rate + log(HPI) + log(Total_population)|year_fips + instiID, data = sbl_tech)


loan_asset + equity_asset + size + roa + log_wage
test<- test[which(bk$fips_instiID == "240135210"),]
branch <- read.csv ("F:/fintech/SOD/matchBRID2.csv")
test2<- branch[which(branch$state == "MD" & branch$instiID == 5210 & branch$county == 13),]
