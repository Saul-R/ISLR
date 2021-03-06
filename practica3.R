#APPLIED
#Preset
library(ISLR)
fix(Auto)


##8. This question involves the use of simple linear regression on the Auto
##data set.
##(a) Use the lm() function to perform a simple linear regression with
##mpg as the response and horsepower as the predictor. Use the
##summary() function to print the results. Comment on the output.
lm_q8<-lm(mpg~horsepower,data=Auto)
summary(lm_q8)
#The p-value for the t value it's really close to 0 and the F-statistic is far
#greater than one, we can reject the hipothesis of the mpg being independent 
#from the horsepower.
#Nevertheless, the low value for the estimate (-0.16) indicates that the impact
#per unit of Horsepower is not so big.
#Also the value for the R squared is quite high, meaning more than half of the
#variance is explained by this predictor (on training set, of course).
##i. Is there a relationship between the predictor and the response?
#A: yes (see comments on F-statistic an p-value)
##ii. How strong is the relationship between the predictor and
##the response?
#A: Not VERY strong, the mpg decreases 0.15 units per unit increase in hp (horsepower)
##iii. Is the relationship between the predictor and the response
##positive or negative?
#A: As commented before, negative.
##iv. What is the predicted mpg associated with a horsepower of
##98? What are the associated 95% confidence and prediction
##intervals?
attach(Auto)
predict(lm_q8,data.frame(horsepower=98),interval="confidence")
predict(lm_q8,data.frame(horsepower=98),interval="prediction")

##(b) Plot the response and the predictor. Use the abline() function
##to display the least squares regression line.
plot(horsepower,mpg)
abline(lm_q8,col="red",lw="4")
##(c) Use the plot() function to produce diagnostic plots of the least
##squares regression fit. Comment on any problems you see with
##the fit.
par(mfrow = c(2,2))
plot(lm_q8)
#Residual vs. Fitted shows some pattern, meaning that the data could be non lineal.
#There are many points with high leverage an a couple of outliers
par(mfrow = c(1,1))

##9. This question involves the use of multiple linear regression on the
##Auto data set.
##(a) Produce a scatterplot matrix which includes all of the variables
##in the data set.
pairs(Auto)
##(b) Compute the matrix of correlations between the variables using
##the function cor(). You will need to exclude the name variable,
##which is qualitative.
cor(subset(Auto,select = -name))
##(c) Use the lm() function to perform a multiple linear regression
##with mpg as the response and all other variables except name as
##the predictors. Use the summary() function to print the results.
lm_q9<-lm(mpg~.-name,data=Auto)
summary(lm_q9)
##Comment on the output. For instance:
##i. Is there a relationship between the predictors and the response?
#F statistic (far from 1) shows that the predictor and the response are not
#independent.
##ii. Which predictors appear to have a statistically significant
##relationship to the response?
#Some of the predictors seem to be statistically significant enough
#for the response to be dependent on them (weight,year & Origin, less significant,
#but still enough is displacement)
##iii. What does the coefficient for the year variable suggest?
#The higher the year, the higher the mpg, with the second greatest slope of the
#predictors, meaning it has one of the biggest impact in mpg.
##(d) Use the plot() function to produce diagnostic plots of the linear
##regression fit. Comment on any problems you see with the fit.
##Do the residual plots suggest any unusually large outliers? Does
##the leverage plot identify any observations with unusually high
##leverage?
par(mfrow = c(2,2))
plot(lm_q9)
#observation 14 have VERY inusual high leverage, (not high residual anyway)
par(mfrow=c(1,1))
plot(predict(lm_q9),rstudent(lm_q9))
#Outliers over a value of rstudentized of > 3

##(e) Use the * and : symbols to fit linear regression models with
##interaction effects. Do any interactions appear to be statistically
##significant?
sink("outputMLR.txt")
summary(lm(mpg~(.-name)*(.-name),data=Auto))
sink()
#Most significant is acceleration:origin, displacement:year, acceleration:year.
#These below seem to be also quite significant statistically
#Cylinders:year, Cylinders:acceleration, horsepower:acceleration, year:origin 

##(f) Try a few different transformations of the variables, such as
##log(X), √X, X2. Comment on your findings.  
pairs(Auto)
#some of the responses seem to have a radical form (exponent between 0 and 1)
# for example displacement,horsepower, and weight, compared to mpg
plot(mpg~horsepower, data=Auto)
summary(lm(mpg~horsepower),data=Auto)
plot(mpg~I(horsepower^(1/4)), data=Auto)
summary(lm(mpg~I(horsepower^(1/4))))
#We can see in the second example, the linear model explains more variance than
#the first one, (adjusted R-Squared)  0.6049 vs 0.657
####INTERESTING TODO: Create a function that calculates the optimal power for this model
####not very afraid of overfit, but it can happen (be careful)

##10. This question should be answered using the Carseats data set.
library(ISLR)
##(a) Fit a multiple regression model to predict Sales using Price,
##Urban, and US.
pairs(Carseats[,c("Sales","Price","Urban","US")])
lm_q10<-lm(Sales~Price+Urban+US,data=Carseats)
summary(lm_q10)
##(b) Provide an interpretation of each coefficient in the model. Be
##careful—some of the variables in the model are qualitative!
#The Price seem to be related with Sales, for each unit increase in Price there is
#a 0.054 units decrease in sales. Sales seem to be independent from the Urban
#predictor, with a VERY high p-value (meaning the value for Urban yes is likely
#to happen by chance)
#The Predictor for USYes indicates that there is a increase in 1.2 units in sales
#when the Value for this qualitative variable is Yes.
#The bad news is the R-squared value, under 0.25
##(c) Write out the model in equation form, being careful to handle
##the qualitative variables properly.
#ý<-13.04+Price*(-0.05)+(Urban="Yes")*(-0.02)+(US="Yes")*(1.20)

##(d) For which of the predictors can you reject the null hypothesis
##H0 : βj = 0?
#Just for Urban, as commented before.
##(e) On the basis of your response to the previous question, fit a
##smaller model that only uses the predictors for which there is
##evidence of association with the outcome.
lm_q10.2<-lm(Sales~Price+US,data=Carseats)
summary(lm_q10.2)
##(f) How well do the models in (a) and (e) fit the data?
anova(lm_q10,lm_q10.2)
#R squared is around 0.23, meaning that less than 1 third of the varianze is
#explained by the model.

##(g) Using the model from (e), obtain 95% confidence intervals for
##the coefficient(s).
confint(lm_q10.2,level=0.95)

##(h) Is there evidence of outliers or high leverage observations in the
##model from (e)?
par(mfrow=c(2,2))
plot(lm_q10.2)
par(mfrow=c(1,1))
#one or two outliers
plot(predict(lm_q10.2),rstudent(lm_q10.2))
#no outliers

##11. In this problem we will investigate the t-statistic for the null hypothesis
##H0 : β = 0 in simple linear regression without an intercept. To
##begin, we generate a predictor x and a response y as follows.

 set.seed (1)
 x=rnorm (100)
 y=2*x+rnorm (100)

##(a) Perform a simple linear regression of y onto x, without an intercept.
##Report the coefficient estimate ˆβ, the standard error of
##this coefficient estimate, and the t-statistic and p-value associated
##with the null hypothesis H0 : β = 0. Comment on these
##results. (You can perform regression without an intercept using
##the command lm(y∼x+0).)
lm_q11<-lm(y~x+0)
#the slope is almost perfect, we gave a slope of 2 (plus noise) and we're getting 1.994
summary(lm_q11)
##(b) Now perform a simple linear regression of x onto y without an
##intercept, and report the coefficient estimate, its standard error,
##and the corresponding t-statistic and p-values associated with
##the null hypothesis H0 : β = 0. Comment on these results
summary(lm(x~y+0))
#same t and p values

##(c) What is the relationship between the results obtained in (a) and
##(b)?
#The're basically the same regression (rotated 90º), β_y = 1/β_x

##(d) For the regression of Y onto X without an intercept, the tstatistic
##for H0 : β = 0 takes the form ˆβ/SE( ˆ β), where ˆ β is
##given by (3.38), and where
##SE( ˆ β) = !
##n
##i=1(yi − xi ˆ β)2
##(n − 1)
##n
##i=1 x2 i
##.
##124 3. Linear Regression
##(These formulas are slightly different from those given in Sections
##3.1.1 and 3.1.2, since here we are performing regression
##without an intercept.) Show algebraically, and confirm numerically
##in R, that the t-statistic can be written as
##(√n − 1)
##n
##i=1 xiyi
##(
##n
##i=1 x2i
##)(
##n
##i=1 y2
##i ) − (
##n
##i=1 xiyi )2
##.
#In the notebook
##(e) Using the results from (d), argue that the t-statistic for the regression
##of y onto x is the same as the t-statistic for the regression
##of x onto y.
#Exchangable variables in the formula
##(f) In R, show that when regression is performed with an intercept,
##the t-statistic for H0 : β1 = 0 is the same for the regression of y
##onto x as it is for the regression of x onto y.
summary(lm(x~y))
summary(lm(y~x))

##12. This problem involves simple linear regression without an intercept.
##(a) Recall that the coefficient estimate ˆ β for the linear regression of
##Y onto X without an intercept is given by (3.38). Under what
##circumstance is the coefficient estimate for the regression of X
##onto Y the same as the coefficient estimate for the regression of
##Y onto X?
#sum(x^2)==sum(y^2)
##(b) Generate an example in R with n = 100 observations in which
##the coefficient estimate for the regression of X onto Y is different
##from the coefficient estimate for the regression of Y onto X.
x=rnorm(100)
y=x*0.5
lm(x~y+0)
lm(y~x+0)
##(c) Generate an example in R with n = 100 observations in which
##the coefficient estimate for the regression of X onto Y is the
##same as the coefficient estimate for the regression of Y onto X.
x=rnorm(100)
y=(sample(x,100))
lm(x~y+0)
lm(y~x+0)

##13. In this exercise you will create some simulated data and will fit simple
##linear regression models to it. Make sure to use set.seed(1) prior to
##starting part (a) to ensure consistent results.
set.seed(1)
##(a) Using the rnorm() function, create a vector, x, containing 100
##observations drawn from a N(0, 1) distribution. This represents
##a feature, X.
x<-rnorm(100,0,1)
##(b) Using the rnorm() function, create a vector, eps, containing 100
##observations drawn from a N(0, 0.25) distribution i.e. a normal
##distribution with mean zero and variance 0.25.
eps<-rnorm(100,0,sqrt(0.25))
#careful with variance (sigma squared) and standard deviation (sigma)
##(c) Using x and eps, generate a vector y according to the model
##Y = −1 + 0.5X + . (3.39)
##What is the length of the vector y? What are the values of β0
##and β1 in this linear model?
y<--1+(0.5*x)+eps
length(y)
#β0=-1; β1=0.5
##(d) Create a scatterplot displaying the relationship between x and
##y. Comment on what you observe.
plot(x,y)
#clear linear relationship
##(e) Fit a least squares linear model to predict y using x. Comment
##on the model obtained. How do ˆ β0 and ˆ β1 compare to β0 and
##β1?
lm_q13<-lm(y~x)
summary(lm_q13)
#-0.998, 0.48
#quite approx.
##(f) Display the least squares line on the scatterplot obtained in (d).
##Draw the population regression line on the plot, in a different
##color. Use the legend() command to create an appropriate legend.
plot(x,y)
abline(lm_q13,col=2,lw=1)
abline(-1,0.5,lwd=1,col=3)
legend(-1,legend=c("model fit","pop. regression"),col=2:3,lwd=3)
##(g) Now fit a polynomial regression model that predicts y using x
##and x2. Is there evidence that the quadratic term improves the
##model fit? Explain your answer.
lm_q13.2<-lm(y~x+I(x^2))
summary(lm_q13.2)
summary(lm_q13)
#minimal improve in training, probably it will be worse in test, since we know
#the points follow a linear distribution. Not statistically significant enough for
#x^2 as predictor

##(h) Repeat (a)–(f) after modifying the data generation process in
##such a way that there is less noise in the data. The model (3.39)
##should remain the same. You can do this by decreasing the variance
##of the normal distribution used to generate the error term
## in (b). Describe your results.
set.seed(1)
eps2<-rnorm(100,0,sqrt(0.1))
y2<--1+(0.5*x)+eps2
lm_q13.3<-lm(y2~x)
summary(lm_q13.3)
plot(x,y2)
abline(lm_q13.3,col="red",lw=3)
summary(lm_q13)
#Surprisingly R squared is higher for the case with more noise

##(i) Repeat (a)–(f) after modifying the data generation process in
##such a way that there is more noise in the data. The model
##(3.39) should remain the same. You can do this by increasing
##the variance of the normal distribution used to generate the
##error term  in (b). Describe your results.

set.seed(1)
eps3<-rnorm(100,0,sqrt(0.5))
y3<--1+(0.5*x)+eps3
lm_q13.4<-lm(y3~x)
summary(lm_q13.4)
plot(x,y2)
abline(lm_q13.4,col="red",lw=3)
#Much lower R squared value (less variance explained). Stronger Null hypothesis
# (still far from one F-statistic and close to 0 p-value)
summary(lm_q13)
summary(lm_q13.3)
summary(lm_q13.4)

##(j) What are the confidence intervals for β0 and β1 based on the
##original data set, the noisier data set, and the less noisy data
##set? Comment on your results.

##14. This problem focuses on the collinearity problem.
##(a) Perform the following commands in R:
 set.seed (1)
 x1=runif(100)
 x2 =0.5*x1+rnorm(100)/10
 y=2+2*x1+0.3*x2+rnorm(100)
##The last line corresponds to creating a linear model in which y is
##a function of x1 and x2. Write out the form of the linear model.
##What are the regression coefficients?
#b0+b1*x1+b2*x2+eps=y
#b0=2,b1=2,b2=0.3
##(b) What is the correlation between x1 and x2? Create a scatterplot
##displaying the relationship between the variables.
plot(x1,x2)
#linear relationship, quite strong
##(c) Using this data, fit a least squares regression to predict y using
##x1 and x2. Describe the results obtained. What are ˆ β0, ˆ β1, and
##ˆ β2? How do these relate to the true β0, β1, and β2? Can you
##reject the null hypothesis H0 : β1 = 0? How about the null
##hypothesis H0 : β2 = 0?

summary(lm(y~x1+x2))
#Reject Null Hipothesis for b1 but no for b2
#Intercept is quite close to real, B1 is not so far, but B2 is very inaccurate

##(d) Now fit a least squares regression to predict y using only x1.
##Comment on your results. Can you reject the null hypothesis
##H0 : β1 = 0?
summary(lm(y~x1))
#Stronger  p value, improvement on the model. Reject H0
##(e) Now fit a least squares regression to predict y using only x2.
##Comment on your results. Can you reject the null hypothesis
##H0 : β1 = 0?
summary(lm(y~x2))
#Reject H0. 
##(f) Do the results obtained in (c)–(e) contradict each other? Explain
##your answer.
#since x1 and x2 are linearly dependent, both will give the same information
# taking both will result in one of them being rejected, and taking each of
#them separately will result in H0 to be rejected. If one predictor is relevant,
# then the other will be relevant too

##(g) Now suppose we obtain one additional observation, which was
##unfortunately mismeasured.
 x1g=c(x1 , 0.1)
 x2g=c(x2 , 0.8)
 yg=c(y,6)
##Re-fit the linear models from (c) to (e) using this new data. What
##effect does this new observation have on the each of the models?
##In each model, is this observation an outlier? A high-leverage
##point? Both? Explain your answers.
par(mfrow=c(2,2))
plot(lm(yg~x1g+x2g))
#Really high leverage point
plot(lm(yg~x1g))
#A bit of an outlier
plot(lm(yg~x2g))
#High leverage
# quite sure it will (as in the second model it's an outlier) shift the p-val
# in the model with both x1 x2, from x1 to x2 to become statistically significant

##15. This problem involves the Boston data set, which we saw in the lab
##for this chapter. We will now try to predict per capita crime rate
##using the other variables in this data set. In other words, per capita
##crime rate is the response, and the other variables are the predictors.
library(MASS)
##(a) For each predictor, fit a simple linear regression model to predict
##the response. Describe your results. In which of the models is
##there a statistically significant association between the predictor
##and the response? Create some plots to back up your assertions.
summary(Boston)
str(Boston)
#LOOP NOT WORKING
# myLoop <- function(){
#   output<-vector()
#   for (i in 2:14){
#     output<-c(output,summary(lm(Boston$crim~Boston[,i])))
#     #  names(Boston)[i]
#   }
#   output
# }

attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes
lm.indus = lm(crim~indus)
summary(lm.indus) # yes
lm.chas = lm(crim~chas) 
summary(lm.chas) # no
lm.nox = lm(crim~nox)
summary(lm.nox) # yes
lm.rm = lm(crim~rm)
summary(lm.rm) # yes
lm.age = lm(crim~age)
summary(lm.age) # yes
lm.dis = lm(crim~dis)
summary(lm.dis) # yes
lm.rad = lm(crim~rad)
summary(lm.rad) # yes
lm.tax = lm(crim~tax)
summary(lm.tax) # yes
lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes
lm.black = lm(crim~black)
summary(lm.black) # yes
lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes
lm.medv = lm(crim~medv)
summary(lm.medv) # yes


##(b) Fit a multiple regression model to predict the response using
##all of the predictors. Describe your results. For which predictors
##can we reject the null hypothesis H0 : βj = 0?

lm.all<-lm(crim~.,data=Boston)
summary(lm.all)
#zn, dis, rad, black, medv...¿nox & lstat?

##(c) How do your results from (a) compare to your results from (b)?
##Create a plot displaying the univariate regression coefficients
##from (a) on the x-axis, and the multiple regression coefficients
##from (b) on the y-axis. That is, each predictor is displayed as a
##single point in the plot. Its coefficient in a simple linear regression
##model is shown on the x-axis, and its coefficient estimate
##in the multiple linear regression model is shown on the y-axis.

a<-summary(lm.zn)
summary(lm.zn)$coeffic[2,1]
coefficients(lm.zn)[2]

x<-c(coef(lm.zn)[2],
     coef(lm.indus)[2],
     coef(lm.chas)[2],
     coef(lm.nox)[2],
     coef(lm.rm)[2],
     coef(lm.age)[2],
     coef(lm.dis)[2],
     coef(lm.rad)[2],
     coef(lm.tax)[2],
     coef(lm.ptratio)[2],
     coef(lm.black)[2],
     coef(lm.lstat)[2],
     coef(lm.medv)[2])

y<-coef(lm.all)[2:14]
par(mfrow=c(1,2))
plot(y~x,type="n")
text(x,y,names(x))
plot(y[-4]~x[-4],type="n")
text(x,y,names(x))
#clear outlier in nox (30 univariate, -10 multivar.)
#ptratio is also quite different from one regression to another
differences<-sqrt(abs((x-y)^2/y))
differences
#RSE's
rseUniv<-c(summary(lm.zn)$sigma,
    summary(lm.indus)$sigma,
    summary(lm.chas)$sigma,
    summary(lm.nox)$sigma,
    summary(lm.rm)$sigma,
    summary(lm.age)$sigma,
    summary(lm.dis)$sigma,
    summary(lm.rad)$sigma,
    summary(lm.tax)$sigma,
    summary(lm.ptratio)$sigma,
    summary(lm.black)$sigma,
    summary(lm.lstat)$sigma,
    summary(lm.medv)$sigma)

s.all<-summary(lm.all)

##(d) Is there evidence of non-linear association between any of the
##predictors and the response? To answer this question, for each
##predictor X, fit a model of the form
##Y = β0 + β1X + β2X2 + β3X3 + .

attach(Boston)
lmpoly.zn = lm(crim~poly(zn,3))
summary(lmpoly.zn) 
lmpoly.indus = lm(crim~poly(indus,3))
summary(lmpoly.indus) 
#lmpoly.chas = lm(crim~poly(chas,3)) 
#summary(lmpoly.chas) 
lmpoly.nox = lm(crim~poly(nox,3))
summary(lmpoly.nox) 
lmpoly.rm = lm(crim~poly(rm,3))
summary(lmpoly.rm) 
lmpoly.age = lm(crim~poly(age,3))
summary(lmpoly.age) 
lmpoly.dis = lm(crim~poly(dis,3))
summary(lmpoly.dis) 
lmpoly.rad = lm(crim~poly(rad,3))
summary(lmpoly.rad) 
lmpoly.tax = lm(crim~poly(tax,3))
summary(lmpoly.tax) 
lmpoly.ptratio = lm(crim~poly(ptratio,3))
summary(lmpoly.ptratio) 
lmpoly.black = lm(crim~poly(black,3))
summary(lmpoly.black) 
lmpoly.lstat = lm(crim~poly(lstat,3))
summary(lmpoly.lstat) 
lmpoly.medv = lm(crim~poly(medv,3))
summary(lmpoly.medv) 

rsePoly<-c(summary(lmpoly.zn)$sigma,
           summary(lmpoly.indus)$sigma,
           summary(lm.chas)$sigma,#this one is excluded, (set to equal RSE)
           summary(lmpoly.nox)$sigma,
           summary(lmpoly.rm)$sigma,
           summary(lmpoly.age)$sigma,
           summary(lmpoly.dis)$sigma,
           summary(lmpoly.rad)$sigma,
           summary(lmpoly.tax)$sigma,
           summary(lmpoly.ptratio)$sigma,
           summary(lmpoly.black)$sigma,
           summary(lmpoly.lstat)$sigma,
           summary(lmpoly.medv)$sigma)
par(mfrow=c(2,2))
plot(rsePoly~rseUniv,type="n")
text(rseUniv,rsePoly,names(x))
abline(0,1)
#much higher rse for the univariate model in the medv case (non linear)
plot(crim~medv)
#indus,dis and nox are also quite far from
plot(crim~dis)
lines(sort(dis), fitted(lmpoly.dis)[order(dis)], col='red', type='b') 
plot(crim~nox)
lines(sort(nox), fitted(lmpoly.nox)[order(nox)], col='red', type='b') 
