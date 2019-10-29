#Import the necessary libraries
library(lmtest)
#Import the dataset
tress<-read.table("C:/Users/pc/Desktop/trees.txt",header=TRUE)
attach(trees)
names(trees)
#Change measurement units
V=(Volume*30.48^3)/1000 # convert volume to lt
H=(Height*30.48)#convert height to cm
D=(Girth *2.54)#convert diameter to cm
#Create a dataframe
treesNew=cbind(V,H,D)
treesNew=data.frame(treesNew)
#Correlation matrix with Pearson coefficient metric
cor(treesNew)
# Create a scatterplot between volume and diameter
plot(V~D,data=treesNew,xlab="diameter (cm)",ylab="volume (lt)",type="b",las=1,pch=20,col="blue4")
#Create a simple linear regression model(model1)
model.1=V~D
res.reg.1=lm(model.1,data=treesNew)
#Print the summary statistics of the model1
summary(res.reg.1)
confint(res.reg.1)
anova(res.reg.1)
3*(2/length(resid(res.reg.1)))
#Create a automatic regression model(model2) with backward elimination method
model.upper.2=V~.
res.reg.2=lm(model.upper.2, data=treesNew)
summary(res.reg.2)
res.step.reg=step(res.reg.2,direction="backward")
res.step.reg$anova
summary(res.step.reg)
#Harvey-Collier test
harvtest(res.reg.2)
#studentized Breusch-Pagan test
bptest(res.reg.2)
#Durbin-Watson test
dwtest(res.reg.2)
#Breusch-Godfrey test for serial correlation of order up to 4
bgtest(res.reg.2,order=4)
#Check for the normality
stats.d(residuals(res.reg.2))
#Create model3
model.3=log(V)~log(H)+log(D)
res.reg=lm(model.3,treesNew)
summary(res.reg)
confint(res.reg)
4*qf(0.05,2,28,lower.tail=FALSE)
#Harvey-Collier test
harvtest(res.reg)
#studentized Breusch-Pagan test
bptest(res.reg)
#Durbin-Watson test
dwtest(res.reg)
#Breusch-Godfrey test for serial correlation of order up to 4
bgtest(res.reg,order=4)
#Check for the normality
stats.d(residuals(res.reg))
#Comparison of errors
RM2=summary(res.reg.2)
RM3=summary(res.reg)
#Shapiro-Wilk normality test
t.norm.2=shapiro.test(residuals(res.reg.2))
t.norm.3=shapiro.test(residuals(res.reg))
#Define a function for goodness of fit measure
MSE=function(e){MSE=sum(na.omit(e)^2)/length(na.omit(e))}
#Define the theta.fit and theta.predict
theta.fit=function(x,y){r.r=lm(y~x,na.action=na.omit)}
theta.predict=function(r.r,x){if(is.vector(x)==TRUE){aux.res=sum(c(1,x)*r.r$coef)}
else {aux.res=cbind(1,x)%*%r.r$coef}
aux.res}
#Model2
M2=H + D
V1=V/1000
res.reg=lm(V1~M2)
new.data=data.frame(M2)
res.pred=predict(res.reg,new.data,type="response")
my.fit.2=res.pred
MSE.2=sum((V1-my.fit.2)^2)/length(M2);MSE.2
#Model3
M3=log(D)+log(H)
V1=V/1000
res.reg=lm(log(V1)~M3)
res.pred=predict(res.reg,type="response")
my.fit.3=exp(res.pred)
MSE.3=sum((V1-my.fit.3)^2)/length(M3)











