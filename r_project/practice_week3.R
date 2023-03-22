library(data.table)
library(ggplot2)
#library(stringr)

df = read.csv('data/Ch3_marketing.csv',header=TRUE, stringsAsFactors=FALSE)
str(df)
class(df)
mark = setDT(df)

table(mark$pop_density)

mark[,pop_density := factor(pop_density ,levels=c('Low','Medium','High'),ordered = T)]
summary(mark$google_adwords)
summary(mark$pop_density)

ggplot(mark ,aes(x=pop_density)) + geom_bar()
ggplot(mark ,aes(x=pop_density)) + geom_bar(fill='blue')

ggplot(mark ,aes(y=google_adwords)) + geom_boxplot(fill='blue')
ggplot(mark ,aes(x=google_adwords)) + geom_histogram(fill='yellow',color='black' , binwidth =50)
ggplot(mark ,aes(x=google_adwords)) + geom_histogram(fill='green',color='black', bins =10)

ggplot(mark ,aes(y=twitter)) + geom_boxplot(fill='yellow')
ggplot(mark ,aes(x=twitter)) + geom_histogram(fill='green',color='black', bins =10)


#The cut() function subdivides values into groups
mark[,empFactor:=cut(employees ,2)]
table(mark$empFactor)

table(mark$empFactor , mark$pop_density)
ggplot(mark ,aes(x=pop_density ,fill=empFactor)) + geom_bar()
ggplot(mark ,aes(y=marketing_total ,x=pop_density)) +geom_boxplot(fill='yellow')

ggplot(mark ,aes(x=revenues ,y=google_adwords)) + geom_point(color='purple')

#Correlation
cor(mark$google_adwords ,mark$revenues)
mark[,cor(google_adwords ,revenues)]
cor.test(mark$google_adwords ,mark$revenues)

cor.test(mark$twitter ,mark$revenues)
cor.test(mark$facebook ,mark$revenues)
mark [,.(cor.test(twitter ,revenues),cor.test(facebook ,revenues))]

ggplot(mark ,aes(x=revenues ,y=google_adwords)) +
  geom_point(color='purple') +
  ggtitle("Google")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(mark ,aes(x=revenues ,y=twitter)) +
  geom_point(color='blue') +
  ggtitle("twitter")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(mark ,aes(x=revenues ,y=facebook)) +
  geom_point( color='red') +
  ggtitle("facebook")+
  theme(plot.title=element_text(hjust=0.5))

pairs(mark)

cor(mark [ ,1:6])
