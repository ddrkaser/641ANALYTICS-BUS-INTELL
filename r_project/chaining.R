library(data.table)

df_bike = read.csv('data/Ch1_bike_sharing_data.csv',header=TRUE, stringsAsFactors=FALSE)
bike = setDT(df_bike)

#recall order
result = bike[,c('seasonAvgTemp' ,'seasonMinTemp'):=.( mean(temp),min(temp)),by=season]
result = result[order(season,weather)]
#chaining
rm(result)
result = bike[,c('seasonAvgTemp' ,'seasonMinTemp'):=.( mean(temp),min(temp)),by=season][order(-season,weather)]

