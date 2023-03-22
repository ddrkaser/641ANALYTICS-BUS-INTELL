bid_main<- b_m
all_bidder<- unique(bid_main$BidderID)
bid_main$pridef<- NA
bid_main$pridef_perc<- NA
bid_main$pri_normal<- NA


for (i in 1:length(all_bidder)){
  bid_history<- bid_main[which(bid_main$BidderID == all_bidder[i]),]
  row_num<- which(bid_main$BidderID == all_bidder[i])
  bid_time<- bid_history$BidDate
  
  for (j in 1:length(row_num)){
    bid_pri2yr<- bid_history[which(bid_history$BidDate <= (bid_history$BidDate[j] - 365*2)),]
    r<- row_num[j]
    
    if (nrow(bid_pri2yr) > 0){
      latest_pri2yr<- tail(bid_pri2yr, 1)
      bid_main$pridef[r] <- latest_pri2yr$default
      bid_main$pri_normal[r]<- latest_pri2yr$normal
      def_perc<- sum(bid_pri2yr$default)/nrow(bid_pri2yr)
      bid_main$pridef_perc[r]<- def_perc
    } 
  }
}
