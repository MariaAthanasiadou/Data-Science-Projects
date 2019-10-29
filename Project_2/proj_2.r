#Import the necessary libraries
library(UsingR)
library(MASS)
#View the dataset aand the specific variables
attach(babies)
names(babies)
gestation
smoke
#Replace number 9 with the NA value
smoke[smoke==9]<-NA
#Remove NA's values
smoke=na.omit(smoke) 
#Convert the smoke attribute to factor attribute
class(smoke)#numeric
smokeNew<-factor(smoke)
class(smokeNew)#factor
#Replace numbers 2 and 3 with the 0 value
smokeNew[smokeNew==2|smokeNew==3]<-0
#Replace the numbers 0 and 1 with the labels not smoke and smoke respectively
smokeNew<-factor(smokeNew, levels=0:1,labels=c('not smoke','smoke now'))
#View the number of the smoked and non-smoked women
table(smokeNew)
#Separate the dataset into the two groups
babiesNew=split(c(gestation), smokeNew)
#Check of the normal distrubution of the two groups with the stats.d() function
#and you can see that the data are not normally distributed therefore we remove outliers
stats.d(babiesNew$'smoke now')
stats.d(babiesNew$'not smoke')
#Check for the mean,for the confidence intervals,namely μ=40*7=280 vs μ!=280
t.test(babiesNew$'smoke now',alternative="two.sided",mu=280,conf.level=0.95)
#Remove the outliers, check of the normality with stats.d and apply t.test()
bD=babiesNew$'smoke now'[babiesNew$'smoke now'<=300]
stats.d(bD)
t.test(bD,alternative="two.sided",mu=280,conf.level=0.95)
#Choose of λ value 
aux=boxcox(bD~1,lambda=seq(-10,10,1/10),plotit=F)
aux$x[which(aux$y==max(aux$y))]#if λ=7.4
#After we have selected the λ value, we do the check for the normality on the transformed values without the outliers
tr.x=bc.t(bD, 7.4)
stats.d(tr.x)
#Τransform the Ho,namely μ=280 and apply t.test()
mu.0=bc.t(280,7.4)
t.test(tr.x,alternative="two.sided",mu=mu.0,conf.level=0.95)
#Apply the non-parametric Wilcoxon test
wilcox.test(bD,mu=280)
#Remove the outliers, check of the normality with stats.d 
bD2=babiesNew$'not smoke'[babiesNew$'not smoke'<=300]
stats.d(bD2)
#Visualize the histograms for smoked and non-smoked women
bD2New=bD2[bD2>150]
hist(bD)
hist(bD2New)
#Check of variances σ1=σ2 or not
var.test(bD,bD2New,alternative="two.sided")
#Check the means μ1=μ2 or not
t.test(bD,bD2New,alternative="two.sided",var.equal=TRUE)
#Apply ks.test()
ks.test(bD-mean(bD),bD2New-mean(bD2New))
#Apply the non-parametric Wilcoxon test
wilcox.test(bD,bD2New,paired=FALSE)

