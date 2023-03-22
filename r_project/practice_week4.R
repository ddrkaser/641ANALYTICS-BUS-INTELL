library(data.table)
library(ggplot2)
#library(stringr)

df = read.csv('data/Ch4_marketing.csv',header=TRUE, stringsAsFactors=FALSE)
advert=copy(df)
setDT(advert)
str(advert)
advert = fread('data/Ch4_marketing.csv')

#test = advert[,.(summary(google_adwords),summary(facebook), summary(twitter))]

#advert[,.(summary(facebook))]
summary(advert$facebook)
summary(advert$google_adwords)
summary(advert$twitter)

g=advert[,google_adwords]
f=advert[,facebook]
tw=advert[,twitter]
nv=c(g,f,tw)

nc=c(rep('g',NROW(g)),rep('f',NROW(f)),rep('tw',NROW(tw)))
advert2=data.table(nv ,nc)
ggplot(advert2 , aes(x=nc ,y=nv)) + geom_boxplot ()
pairs(advert)

ggplot(advert , aes(x=google_adwords , y=marketing_total)) + geom_point(color='blue')
ggplot(advert , aes(x=revenues , y=marketing_total)) + geom_point(color='purple')

#Simple Linear Regression
model1=lm(revenues~marketing_total , data = advert)
model1
#check model summary
summary(model1)

#plot estimated regression line
ggplot(advert , aes(x=revenues , y=marketing_total)) +
  geom_point(color='purple') +
  geom_smooth(method = 'lm')

#list all model attributes
str(model1)

#call model residuals
model1$residuals

#test residuals Normality
#The residuals form a normal distribution around the regression line with mean value of 0.
resdf = data.table('res'=model1$residuals)
ggplot(resdf ,aes(x=res)) + geom_histogram(bins =10, fill='purple',color='black')
ggplot(resdf ,aes(sample=res)) + stat_qq(color='blue')+ stat_qq_line()

#Equal Variance
#Residuals form a random pattern distributed around a mean of 0.
#get predicted values from lm model
resdf[,pred := model1$fitted.values]

ggplot(resdf ,aes(x=pred, y=res)) +
  geom_point(color='purple')+
  geom_smooth(method='lm')

#use new marketing totals to predict revenues
summary(advert$marketing_total)
advert[marketing_total >430, marketing_total]
newrev = data.table(marketing_total=seq (460 ,470 ,5))
predict.lm(model1 ,newrev ,interval = 'predict')

#on 99% confidence interval
predict.lm(model1 ,newrev ,level =.99, interval = 'predict')

#sample 30% of data
set.seed (4510)
liladvert = advert[sample(.N,.3*.N)]
samp_model = lm(revenues~marketing_total ,data=liladvert)
summary(samp_model)
confint(samp_model)

#use some new data for example to test the assumptions LINE
x=1:10
y=c(1 ,1.41 ,1.73 ,2 ,2.24 ,2.45 ,2.65 ,2.83 ,3 ,3.16)
fit = lm(y~x)
sampdt = data.table(x,y)
fit = lm(y~x)
sampdt = data.table(x,y)
sampdt[,res:=fit$residuals]
sampdt[,pred :=fit$fitted.values]

ggplot(sampdt ,aes(x=x,y=y)) + geom_point(color = 'purple') +
  geom_smooth(method = "lm") + labs(title="Linearity?")

ggplot(sampdt ,aes(x=res)) + geom_histogram(bins =10,fill='blue',color='white') +
  labs(title= "Normality?")

ggplot(sampdt ,aes(x=pred ,y=res)) + geom_point(color='purple') +
  geom_smooth(method = 'lm') + labs(title="Equal Variance?")

#Multiple Regression
model2 = lm(revenues ~ google_adwords + facebook + twitter , data=advert)
resdf2 = data.table('res'=model2$residuals)
resdf2[,pred :=model2$fitted.values]
ggplot(resdf2 ,aes(x=res)) + geom_histogram(bins =10,fill='blue',color='white')

ggplot(resdf2 ,aes(sample=res)) + stat_qq(color="blue") +stat_qq_line ()

ggplot(resdf2 ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm')

summary(model2)

#without twitter
model3 = lm(revenues ~ google_adwords + facebook, data=advert)
resdf3 = data.table('res'=model3$residuals)
resdf3[,pred :=model3$fitted.values]

ggplot(resdf3 ,aes(x=res)) + geom_histogram(bins =10,fill='blue',color='white')

ggplot(resdf3 ,aes(sample=res)) + stat_qq(color="blue") +stat_qq_line ()

ggplot(resdf3 ,aes(x=pred ,y=res)) + geom_point(color='purple') + geom_smooth(method = 'lm')

summary(model3)
