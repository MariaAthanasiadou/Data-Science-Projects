#Import the necessary libraries
library(dplyr)
library(psych)
#Import the dataset 
grades<-read.csv("C:/Users/pc/Desktop/STATMARKS.LECTURE.csv",sep=";",header=TRUE)
attach(grades)
names(grades)
#View the structure of the dataset
glimpse(grades)
#View a specific column of the dataset,the no of occurrences of every unique value in a variable. 
Mark.97
table(Mark.97)
#Count the valid values of dataset
colSums(!is.na(grades))
#Remove NA’s values from each column 
mrk97<-na.omit(Mark.97)
mrk98<-na.omit(Mark.98)
mrk99<-na.omit(Mark.99)
#Calculate the percentage of zero values of the dataset
grades_a<-cbind(mrk97,mrk98,mrk99)
results <- colSums(grades_a==0)/nrow(grades_a)*100
#Compute descriptive statistical measures of each variable
describe(mrk97);var(mrk97); quantile(mrk97,c(0.25,0.75));shapiro.test(mrk97)
gradesTotal<- c(Mark.97,Mark.98,Mark.99)
describe(gradesTotal);var(gradesTotal,na.rm=TRUE);quantile(gradesTotal,na.rm=TRUE,c(0.25,0.75));shapiro.test(gradesTotal)
# visualize the boxplot of scores with 0
boxplot(mrk97,mrk98,mrk99,names=c("1997","1998","1999"),xlab="Years",ylab="Scores",main="Boxplots per exam period with 0")
# Remove zeros values from each column
mrk97_0<-mrk97[!mrk97%in%c(0)]
mrk98_0<-mrk98[!mrk98%in%c(0)]
mrk99_0<-mrk99[!mrk99%in%c(0)]
mrk_0_Total<-gradesTotal[!gradesTotal%in%c(0)]
#Compute descriptive statistical measures of each variable
describe(mrk97_0);var(mrk97_0); quantile(mrk97_0,c(0.25,0.75));shapiro.test(mrk97_0)
describe(mrk_0_Total);var(mrk_0_Total,na.rm=TRUE); quantile(mrk_0_Total,na.rm=TRUE,c(0.25,0.75));shapiro.test(mrk_0_Total)
#Compute the standard asymmetry indicators
n97<-length(mrk97_0))
se.skewness<-sqrt(6/n97)
se.kurtosis<-sqrt(24/n97)
# visualize the boxplot of scores without 0
boxplot(mrk97_0,mrk98_0,mrk99_0,names=c("1997","1998","1999"),xlab="Years",ylab="Scores",main="Boxplots per exam period without 0")
#visualize the graphs for 1997-1999 periods
boxplot(mrk_0_Total,names=c("1997-1999"),xlab="Years",ylab="Scores",main="The Boxplot of 1997-1999 periods")
qqnorm(mrkTotal);qqline(mrkTotal)
hist(mrkTotal,prob=TRUE)
