library(data.table)
library(ggplot2)
#library(stringr)

df = read.csv('data/titanic.csv',header=TRUE, stringsAsFactors=FALSE)
str(df)
class(df)
titanic = setDT(df)

titanic$Pclass=as.factor(titanic$Pclass)
titanic$Survived=as.factor(titanic$Survived)
titanic$Sex=as.factor(titanic$Sex)

unique(titanic$Age)

ggplot(titanic , aes(x=Survived)) + geom_bar()

#Survival Rate
ggplot(titanic , aes(x=Survived)) + geom_bar() +
  theme_bw() +
  labs(y= "Passenger Count", title ="Titanice Survival Rates")

#Survival Rate By Gender
ggplot(titanic , aes(x=Sex , fill=Survived)) + geom_bar() +
  theme_bw() +
  labs(y= "Passenger Count", title= "Titanice Survival Rates by Gender")


#Survival Rate By Class
ggplot(titanic , aes(x=Pclass , fill=Survived)) + geom_bar() +
  theme_bw() +
  labs(y= "Passenger Count", title = "Titanice Survival Rates by Class")


#Survival Rate By Gender AND Class (facet wrap/grid)
ggplot(titanic , aes(x=Sex , fill=Survived)) + geom_bar() +
  theme_bw() + facet_wrap(~ Pclass) +
  labs(y= "Passenger Count", title = "Titanice Survival Rates by Gender and Class")

ggplot(titanic , aes(x=Sex , fill=Survived)) + geom_bar() +
  theme_bw() + facet_grid(~ Pclass) +
  labs(y= "Passenger Count", title = "Titanice Survival Rates by Gender and Class")

#Age Distribution
ggplot(titanic , aes(x=Age)) +
  geom_histogram(binwidth = 5) +
  theme_bw() +
  labs(y= "Passenger Count",x="Age (5)", title = "Titanice Survival Rates")

#Age Distribution (By Survival)
ggplot(titanic , aes(x=Age , fill=Survived)) +
  geom_histogram(binwidth = 5) +
  theme_bw() +
  labs(y= "Passenger Count",x="Age (5)", title = "Titanice Survival Rates")

#Age Distribution Boxplot
ggplot(titanic , aes(x=Survived , y=Age)) +
  geom_boxplot() + theme_bw() +
  labs(y= "Age",x="Survived ", title = "Titanice Survival Rates")

#Titanic - Age/Gender/Class Density Curve
ggplot(titanic , aes(x=Age , fill=Survived)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  labs(y= "Passenger Count",x="Age (5)", title = "Titanice Survival Rates")

#Age/Gender/Class Density Histogram
ggplot(titanic , aes(x=Age , fill=Survived)) +
  geom_histogram(binwidth = 5) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  labs(y= "Passenger Count",x="Age (5)", title = "Titanic Survival Rates")
