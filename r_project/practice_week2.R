library(data.table)
library(stringr)

df_bike = read.csv('data/Ch2_raw_bikeshare_data.csv',header=TRUE, stringsAsFactors=FALSE)
str(df_bike)
class(df_bike)
bikeraw = setDT(df_bike)
str(bikeraw)

is.na(bikeraw)
table(is.na(bikeraw))

#return rows that contains any missing values
na_rows <- bikeraw[!complete.cases(bikeraw)]

#table like count values
table(bikeraw$season)

#return the col index that contains 'google'
str_detect(bikeraw ,'google')
str_detect(bikeraw ,'NA')

#we found all NAs are from source col
bikeraw[is.na(sources)]
bikeraw[is.na(sources),NROW(sources)]
bikeraw[is.na(sources), .N]

bikeraw[grep('[a-z A-Z]',humidity)]$humidity = 'abc61'
bikeraw[grep(r'[\w]',humidity)]
bikeraw[str_detect(humidity, '[a-z A-Z]')]

bikeraw[grep('[a-z A-Z]',humidity),humidity :='61']

bikeraw[,humidity :=as.numeric(humidity)]


unique(bikeraw$holiday)
bikeraw[,holiday := factor(holiday , levels = c(0,1),labels = c('no','yes'))]
bikeraw$holiday = factor(bikeraw$holiday , levels = c(0,1),labels = c('no','yes'))

unique(bikeraw$workingday)
bikeraw[,workingday := factor(workingday , levels = c(0,1),labels = c('no','yes'))]

unique(bikeraw$season)
bikeraw[,season := factor(season , levels = c(1,2,3,4), labels = c('spring','summer','fall','winter'), ordered = T)]

unique(bikeraw$weather)
bikeraw[,weather := factor(weather , levels = c(1,2,3,4),labels = c('clr_part_cloud','mist_cloudy','lt_rain_snow','hvy_rain_snow'))]
bike_ordered = bikeraw[order(weather)]

bikeraw[,weather := factor(weather , levels = c(1,2,3,4),labels = c('clr_part_cloud','mist_cloudy','lt_rain_snow','hvy_rain_snow'),ordered = T)]

bikeraw [,.( datetime)]
bikeraw$datetime

#convert to date
bikeraw[,datetime :=as.Date(datetime , '%m/%d/%Y %H:%M')]
#convert to datetime
bikeraw[,datetime := strptime(datetime , '%m/%d/%Y %H:%M')]

unique(bikeraw$sources)
bikeraw[,sources := tolower(sources)]
bikeraw[,sources := trimws(sources)]
bikeraw[is.na(sources),sources :='unknown']
unique(bikeraw$sources)


unique(bikeraw[grep('www.\\w*.[a-z]*',sources)]$sources)
bikeraw[grep('www.[a-z]*.[a-z]*',sources),sources :='web']
unique(bikeraw$sources)

#gsub
a = c('4x','6a','234 6aax','abc56 ')
b = c('rty1a ','cc6b ','2h3g4 ','abc56 ')
c = c('rty1a ','cc6b ','2h3g4 ','abc56 ')
dt = data.table(a,b,c)
dt[,a:= gsub('[a-z A-Z]','',a)]
dt[,b:= gsub('[a-z A-Z]','',b)]
dt
#
strip = function(x){
  gsub('[a-z A-Z]','',x)
}
#use :=
dt[,c('a','b'):=.(strip(a), strip(b))]
#use .SD
dt[,lapply(.SD, strip),.SDcols=c('a','b')]
#keep all cols
dt[,c('a','b') :=lapply(.SD, strip),.SDcols=c('a','b')]
