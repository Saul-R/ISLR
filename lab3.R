#0-Packages
library(MASS)
library(ISLR)
str(Boston)
#CODEBOOK:http://www.hebrewseniorlife.org/workfiles/IFAR/RNH%20Mobilize%20Boston%20Study%20Codebook.pdf
names(Boston)

#SIMPLE LINEAR regression
?lm
lm.fit<-lm(medv~lstat,data=Boston)
attach(Boston)#añade los campos de boton a los valores cargados (se pueden llamar sin $)
summary(lm.fit)
confint(lm.fit)
#interval tipe: ISLR pag 103 4th printing
#confidence is for average response prediction (does not include epsilon (AKA irreducible error))
#prediction is for individual response prediction (include epsilon => wider interval
predict(object = lm.fit,
        newdata = data.frame(lstat=(c(5,10,15))),
        interval = "confidence")
predict(object = lm.fit,
        newdata = data.frame(lstat=(c(5,10,15))),
        interval = "prediction")
plot(lstat,medv)
abline(lm.fit,col = "red",lwd = 3)
plot(lstat,medv,col = "red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,20:1,pch=1:20)
par(mfrow = c(2,2))
plot(lm.fit)
par(mfrow = c(1,1))
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
#hatvalues = Leverage
plot(hatvalues(lm.fit))
which.max(x = hatvalues(lm.fit))

#Multiple Linear Regression
mlr.fit<-lm(medv~lstat+age,data = Boston) 
#"data=" is unnecessary after >append(Boston)
summary(mlr.fit)
mlr.fit<-lm(medv~.,data=Boston)
summary(mlr.fit)$r.sq
summary(mlr.fit)$fstat
install.packages("car")
library(car)
#variance inflacion factor =~ collinearity
vif(mlr.fit)
lm.fit1<-lm(medv~.-age,data=Boston)
summary(lm.fit1)
lm.fit1<-update(lm.fit,medv~.-age)

#INTERACTION TERMS
#use y~a:b for simple interaction (beta * (a*b))
#Use y~a*b for full interaction(beta1*a + beta2*b + beta3*(a*b)) (y~a+b+a:b)
summary(lm(medv~lstat*age,data=Boston))

#NON LINEAR TRANSFORMATIONS OF THE PREDICTOR
lm.fit2<-lm(medv~lstat+I(lstat^2),data=Boston)
summary(lm.fit2)
#Really low p val, higher R2 than in simple linear reg
#Test the two models one vs the other
anova(lm.fit,lm.fit2)
#P val associated to lm.fit2 is better than lm.fit?
lm.fit5<-lm(medv~poly(lstat,5))
anova(lm.fit2,lm.fit5)
summary(lm.fit5)
summary(lm(medv~log(rm),data=Boston))


##QUALITATIVE PREDICTORS.  
#Data = Carseats. Exploring the data
fix(Carseats)
names(Carseats)
pairs(Carseats)

lm.fit<-lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
anova(lm(Sales~.,data=Carseats),lm.fit)
contrasts(Carseats$ShelveLoc)
#NOTE:High positive slope for ShelveLocGood (indicates the shelve is in good loc)
#also positive for ShelveLocMedium, less slope (lower value)

#WRITING FUNCTIONS 