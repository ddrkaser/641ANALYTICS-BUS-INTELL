#library(tidyverse)
library(data.table)

df_bike = read.csv('data/Ch1_bike_sharing_data.csv',header=TRUE, stringsAsFactors=FALSE)
str(df_bike)
class(df_bike)
bike = setDT(df_bike)
str(bike)

# fliter
bike[season ==4]
bike[season ==4 | season ==1]
bike[season %in% c(1,4)]
bike[!season %in% c(1,4)]

bike[season ==4 & weather ==1]
bike[season != 4 & weather ==1]
nowinter = bike[season != 4 & weather ==1]

#Select cols
bike [,.( datetime ,windspeed)]
bike[,c('datetime','windspeed')]

cols = c('datetime','windspeed')
bike[,..cols]
#de-select
bike [,-c('datetime','windspeed')]
bike [,!c('datetime','windspeed')]

#select by specifying start and end column names
#select cols from datetime to workingday
bike [,datetime:workingday]
#de-select cols from datetime to workingday
bike [,-(datetime:workingday)]


#Select and rename
bike_rename = bike[,.(time = datetime, wind = windspeed)]

#Compute or do in j
#count rows that atemp < temp
bike[,sum(atemp<temp)]
#only return index
bike[,atemp<temp]
#return DT
bike[atemp<temp]

#apply functions
bike[,mean(temp)]
bb = bike[,.( mean(temp),sd(temp),min(temp),max(temp))]
names(bb) = c('Mean','SD','Min','Max')

#group by
#df[RowSection,ColumnSection,Grouping Section]
#group by season
bike[,.( mean(temp),sd(temp),min(temp),max(temp)),by=season]
bike [,.( mean(temp),sd(temp),min(temp),max(temp)),by=.(season ,weather)]
bike [,.( mean(temp),sd(temp),min(temp),max(temp)),by=c('season' ,'weather')]


bws = bike[,.( mean(temp),sd(temp),min(temp),max(temp)),by=.(season, weather)]
names(bws)=c('season','weather' ,'Mean' ,'SD','Min','Max')
bws = bws[order(season,weather)]

# filter the original bike data, by only the days that had a temperature that was less than the average temperature for its particular season
#create a new col called seasonAvgTemp that contains ave temp by its season
bike[,seasonAvgTemp := mean(temp),by=season]
below_avg_temp =bike[temp < seasonAvgTemp]
#create mutiple new cols
bike[,c('seasonAvgTemp' ,'seasonMinTemp'):=.( mean(temp),min(temp)),by=season]
result = bike[weather == 1,c('seasonAvgTemp' ,'seasonMinTemp'):=.( mean(temp),min(temp)),by=season]
#keyby, group by + ordering
result = bike[,c('weatherAvgTemp' ,'weatherMinTemp'):=.( mean(temp),min(temp)),keyby=weather]

#shift function
ticker=c(rep('a' ,5),rep('b' ,5))
oi = c(seq (2 ,10 ,2),seq (5 ,25 ,5))
df_exp=data.table(ticker ,oi)
df_exp[, c('oi2', 'oi3', 'oi4'):=shift(oi , n = 1:3)]

#Subset in i and do in j
bike[season == 3 & count > 20, .(mean_temp = mean(temp), max_wind = max(windspeed))]
#how many days in fall with daily count > 20
bike[season == 3 & count > 20, length(datetime)]
#to make life easier, .N equals to count(*) in SQL
bike[season == 3 & count > 20, .N]
#subset in i, do in j, group by
bike[, .N, by=weather]
bike[season == 3, .N, by=weather]

# by accept expressions as well
bike[,.N,.(weather)]
result = bike[,.N,.(rainy_weather = weather == 4)]

#.SD contains all DT cols and it is col-wise
bike[,lapply(.SD, mean),by=season]
#error because of datetime col
#filter season == 1, group by weather, compute mean of subset cols by using .SDcols.
bike[season== 1,lapply(.SD, mean),by=weather, .SDcols = c('temp','windspeed')]

