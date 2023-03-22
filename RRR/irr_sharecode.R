
### file rp2_a
rp_list<- split(rp2_a, rp2_a$ProjectNo)

cc<- data.frame(matrix(ncol = 2, nrow = length(rp_list)))
colnames(cc)<- c("ProjectNo", "irr")


###################### loan xirr function and then run
for (i in 1:length(rp_list)){
  aa<- rp_list[i][[1]]
  cc$ProjectNo[i]<- aa$ProjectNo[1]
  bb<- data.frame(c(aa$PubTime[1], aa$ActRepayDate_t), c(-aa$LoanAmount[1], aa$PrincInte_real))
  colnames(bb)<- c("dates", "amount")
  dd<- bb %>%
    group_by(dates) %>%
    summarize_all(sum) %>%
    data.frame()
  dd<- dd[!is.na(dd$dates),]
  if (nrow(dd)> 2){
    cc$irr[i]<- xirr(dd)
  }
  else if (nrow(dd) == 2){
    cc$irr[i]<- ((dd$amount[2]/(-dd$amount[1]))^(as.numeric(dd$dates[2]-dd$dates[1])/365)-1)*100
  } else {
    cc$irr[i]<- "-100"
  }
}

################ irr packcage
xirr <- function(dataset) {
  
  # creating a function to calculate npv value
  npv <- function(range, dataset){
    for(test.rate in range) {
      
      max.date <- max(dataset$dates)
      
      temp <- dataset %>%
        mutate(npv = amount * ((1 + test.rate/100)^(as.numeric(max.date - dates)/365))) %>%
        select(npv) %>%
        .[1]
      if(sum(dataset$amount) > 0) {
        if(sum(temp) > 0) {
          min.rate <- test.rate
          next
        } else {
          max.rate <- test.rate
          break
        }
      } else {
        if(sum(temp) < 0) {
          min.rate <- test.rate
          next
        } else {
          max.rate <- test.rate
          break
        }
      }
    }
    return(list(min.rate = min.rate, max.rate = max.rate))
  }
  
  
  names(dataset) <- c("dates", "amount")
  
  max.rate <- c()
  min.rate <- c()
  
  if(sum(dataset$amount) > 0) {
    
    range <- seq(from = 0, to = 10000, by = 100)    
    hundreds <- npv(range, dataset)
    
    range <- seq(from = hundreds$min.rate, to = hundreds$max.rate, by = 10)
    tens <- npv(range, dataset)
    
    range <- seq(from = tens$min.rate, to = tens$max.rate, by = 1)
    ones <- npv(range, dataset)
    
    range <- seq(from = ones$min.rate, to = ones$max.rate, by = 0.01)
    decimals <- npv(range, dataset)
    
    return(mean(unlist(decimals)))   
    
  } else {
    
    range <- seq(from = 0, to = -10000, by = -100)
    hundreds <- npv(range, dataset)
    
    range <- seq(from = hundreds$min.rate, to = hundreds$max.rate, by = -10)
    tens <- npv(range, dataset)
    
    range <- seq(from = tens$min.rate, to = tens$max.rate, by = -1)
    ones <- npv(range, dataset)
    
    range <- seq(from = ones$min.rate, to = ones$max.rate, by = -0.01)
    decimals <- npv(range, dataset)
    
    return(mean(unlist(decimals))) 
  }
}



