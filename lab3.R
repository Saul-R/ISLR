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

