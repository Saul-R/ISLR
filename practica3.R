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
##ii. Which predictors appear to have a statistically significant
##relationship to the response?
##iii. What does the coefficient for the year variable suggest?
##(d) Use the plot() function to produce diagnostic plots of the linear
##regression fit. Comment on any problems you see with the fit.
##Do the residual plots suggest any unusually large outliers? Does
##the leverage plot identify any observations with unusually high
##leverage?
##(e) Use the * and : symbols to fit linear regression models with
##interaction effects. Do any interactions appear to be statistically
##significant?
##(f) Try a few different transformations of the variables, such as
##log(X), √X, X2. Comment on your findings.  
##3.7 Exercises 123

##10. This question should be answered using the Carseats data set.
##(a) Fit a multiple regression model to predict Sales using Price,
##Urban, and US.
##(b) Provide an interpretation of each coefficient in the model. Be
##careful—some of the variables in the model are qualitative!
##(c) Write out the model in equation form, being careful to handle
##the qualitative variables properly.
##(d) For which of the predictors can you reject the null hypothesis
##H0 : βj = 0?
##(e) On the basis of your response to the previous question, fit a
##smaller model that only uses the predictors for which there is
##evidence of association with the outcome.
##(f) How well do the models in (a) and (e) fit the data?
##(g) Using the model from (e), obtain 95% confidence intervals for
##the coefficient(s).
##(h) Is there evidence of outliers or high leverage observations in the
##model from (e)?

##11. In this problem we will investigate the t-statistic for the null hypothesis
##H0 : β = 0 in simple linear regression without an intercept. To
##begin, we generate a predictor x and a response y as follows.
##> set.seed (1)
##> x=rnorm (100)
##> y=2*x+rnorm (100)
##(a) Perform a simple linear regression of y onto x, without an intercept.
##Report the coefficient estimate ˆβ, the standard error of
##this coefficient estimate, and the t-statistic and p-value associated
##with the null hypothesis H0 : β = 0. Comment on these
##results. (You can perform regression without an intercept using
##the command lm(y∼x+0).)
##(b) Now perform a simple linear regression of x onto y without an
##intercept, and report the coefficient estimate, its standard error,
##and the corresponding t-statistic and p-values associated with
##the null hypothesis H0 : β = 0. Comment on these results.
##(c) What is the relationship between the results obtained in (a) and
##(b)?
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
##(e) Using the results from (d), argue that the t-statistic for the regression
##of y onto x is the same as the t-statistic for the regression
##of x onto y.
##(f) In R, show that when regression is performed with an intercept,
##the t-statistic for H0 : β1 = 0 is the same for the regression of y
##onto x as it is for the regression of x onto y.

##12. This problem involves simple linear regression without an intercept.
##(a) Recall that the coefficient estimate ˆ β for the linear regression of
##Y onto X without an intercept is given by (3.38). Under what
##circumstance is the coefficient estimate for the regression of X
##onto Y the same as the coefficient estimate for the regression of
##Y onto X?
##(b) Generate an example in R with n = 100 observations in which
##the coefficient estimate for the regression of X onto Y is different
##from the coefficient estimate for the regression of Y onto X.
##(c) Generate an example in R with n = 100 observations in which
##the coefficient estimate for the regression of X onto Y is the
##same as the coefficient estimate for the regression of Y onto X.

##13. In this exercise you will create some simulated data and will fit simple
##linear regression models to it. Make sure to use set.seed(1) prior to
##starting part (a) to ensure consistent results.
##(a) Using the rnorm() function, create a vector, x, containing 100
##observations drawn from a N(0, 1) distribution. This represents
##a feature, X.
##(b) Using the rnorm() function, create a vector, eps, containing 100
##observations drawn from a N(0, 0.25) distribution i.e. a normal
##distribution with mean zero and variance 0.25.
##(c) Using x and eps, generate a vector y according to the model
##Y = −1 + 0.5X + . (3.39)
##What is the length of the vector y? What are the values of β0
##and β1 in this linear model?
##3.7 Exercises 125
##(d) Create a scatterplot displaying the relationship between x and
##y. Comment on what you observe.
##(e) Fit a least squares linear model to predict y using x. Comment
##on the model obtained. How do ˆ β0 and ˆ β1 compare to β0 and
##β1?
##(f) Display the least squares line on the scatterplot obtained in (d).
##Draw the population regression line on the plot, in a different
##color. Use the legend() command to create an appropriate legend.
##(g) Now fit a polynomial regression model that predicts y using x
##and x2. Is there evidence that the quadratic term improves the
##model fit? Explain your answer.
##(h) Repeat (a)–(f) after modifying the data generation process in
##such a way that there is less noise in the data. The model (3.39)
##should remain the same. You can do this by decreasing the variance
##of the normal distribution used to generate the error term
## in (b). Describe your results.
##(i) Repeat (a)–(f) after modifying the data generation process in
##such a way that there is more noise in the data. The model
##(3.39) should remain the same. You can do this by increasing
##the variance of the normal distribution used to generate the
##error term  in (b). Describe your results.
##(j) What are the confidence intervals for β0 and β1 based on the
##original data set, the noisier data set, and the less noisy data
##set? Comment on your results.

##14. This problem focuses on the collinearity problem.
##(a) Perform the following commands in R:
##> set .seed (1)
##> x1=runif (100)
##> x2 =0.5* x1+rnorm (100) /10
##> y=2+2* x1 +0.3* x2+rnorm (100)
##The last line corresponds to creating a linear model in which y is
##a function of x1 and x2. Write out the form of the linear model.
##What are the regression coefficients?
##(b) What is the correlation between x1 and x2? Create a scatterplot
##displaying the relationship between the variables.
##(c) Using this data, fit a least squares regression to predict y using
##x1 and x2. Describe the results obtained. What are ˆ β0, ˆ β1, and
##ˆ β2? How do these relate to the true β0, β1, and β2? Can you
##reject the null hypothesis H0 : β1 = 0? How about the null
##hypothesis H0 : β2 = 0?
##126 3. Linear Regression
##(d) Now fit a least squares regression to predict y using only x1.
##Comment on your results. Can you reject the null hypothesis
##H0 : β1 = 0?
##(e) Now fit a least squares regression to predict y using only x2.
##Comment on your results. Can you reject the null hypothesis
##H0 : β1 = 0?
##(f) Do the results obtained in (c)–(e) contradict each other? Explain
##your answer.
##(g) Now suppose we obtain one additional observation, which was
##unfortunately mismeasured.
##> x1=c(x1 , 0.1)
##> x2=c(x2 , 0.8)
##> y=c(y,6)
##Re-fit the linear models from (c) to (e) using this new data. What
##effect does this new observation have on the each of the models?
##In each model, is this observation an outlier? A high-leverage
##point? Both? Explain your answers.

##15. This problem involves the Boston data set, which we saw in the lab
##for this chapter. We will now try to predict per capita crime rate
##using the other variables in this data set. In other words, per capita
##crime rate is the response, and the other variables are the predictors.
##(a) For each predictor, fit a simple linear regression model to predict
##the response. Describe your results. In which of the models is
##there a statistically significant association between the predictor
##and the response? Create some plots to back up your assertions.
##(b) Fit a multiple regression model to predict the response using
##all of the predictors. Describe your results. For which predictors
##can we reject the null hypothesis H0 : βj = 0?
##(c) How do your results from (a) compare to your results from (b)?
##Create a plot displaying the univariate regression coefficients
##from (a) on the x-axis, and the multiple regression coefficients
##from (b) on the y-axis. That is, each predictor is displayed as a
##single point in the plot. Its coefficient in a simple linear regression
##model is shown on the x-axis, and its coefficient estimate
##in the multiple linear regression model is shown on the y-axis.
##(d) Is there evidence of non-linear association between any of the
##predictors and the response? To answer this question, for each
##predictor X, fit a model of the form
##Y = β0 + β1X + β2X2 + β3X3 + .
