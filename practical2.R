library(ISLR)
#2.9
##a)
data(Auto)
str(Auto)
#see how many unique values do you have for each measurement (p)
str(lapply(Auto,unique))

#quant_Cols<-c("cylinders","year","origin","name")
qual_Cols<-c(2,7,8,9)

#split data frame
quant_Auto<-Auto[-qual_Cols]
qual_Auto<-Auto[qual_Cols]


##b)
sapply(quant_Auto,range)

##c)
sapply(quant_Auto,mean)
sapply(quant_Auto,sd)

##d)
sub_quant_Auto<-quant_Auto[-10:-8]
sapply(sub_quant_Auto,range)
sapply(sub_quant_Auto,mean)
sapply(sub_quant_Auto,sd)

##e)
par(mfrow = c(1,1))
pairs(Auto)
pairs(Auto[,c(1,3:5)])
boxplot(Auto$mpg~Auto$cylinder,ylab = "Mpg",xlab = "# of Cylinders")
boxplot(Auto$mpg~Auto$year,ylab = "Mpg",xlab = "Year")
install.packages("scales")
library(scales)
scatter.smooth(Auto$displacement,Auto$mpg,col = alpha("red",0.5))
par(new=TRUE)
scatter.smooth(Auto$horsepower,Auto$mpg,col = alpha("black",0.5))
par(new=TRUE)
scatter.smooth(Auto$weight,Auto$mpg,col = alpha("green",0.5))
#Those 3 seem to be quite related to themselves and also seem to be be closely related to MPG (non linear relationship)

#f) See above


#10
##a)
install.packages("MASS")
library(MASS)
?Boston
dim(Boston)

##b)
pairs(Boston)

##c)

plot(Boston$dis, Boston$crim)
# Closer to work-area, more crime

plot(Boston$age, Boston$crim)
# Older homes, more crime
plot(y = log(Boston$crim),x = Boston$age,type = "hist")
plot(Boston$rad, Boston$crim)
unique(Boston$rad)
bostonSubset<-Boston[Boston$crim]
boxplot(Boston$crim[Boston$rad %in% c(5,24)]~Boston$rad[Boston$rad %in% c(5,24)])
# Higher index of accessibility to radial highways, more crime
plot(Boston$tax, Boston$crim)
# Higher tax rate, more crime
plot(Boston$ptratio, Boston$crim)
# Higher pupil:teacher ratio, more crime

##d)
hist(Boston$crim[Boston$crim>1],breaks = 25)
boxplot(Boston$crim~Boston$chas)

##e)
table(Boston$chas)[2]

##f)
median(Boston$ptratio,na.rm = TRUE)

##g)
install.packages("dplyr")
library(dplyr)
lessOwners<-Boston[Boston$medv == min(Boston$medv),]
summary(Boston)
dplyr::filter(Boston,filter = medv == min(medv))
boxplot(Boston$crim)
par(new = T)
lapply(lessOwners$crim,points,col = "red",pch = 23,bg = "red")
#Quite a lot of criminality...

##g)
sum(Boston$rm > 7)
sum(Boston$rm > 8)
Bostonaux<-mutate(Boston,rm>8)

names(Bostonaux)<-c(names(Boston),"bigHouses")

par(mfrow = c(4,4))
mat[3,4]=15
mat[4,4]=14
mat[4,3]=13
mat[4,2]=12
mat
layout(mat)

for (i in 1:dim(Bostonaux)[2]){
  boxplot(Bostonaux[,i]~Bostonaux$bigHouses,xlab = "Big Houses", ylab = names(Bostonaux)[i])
}
#Low Crime, more shops, usually older. More black people population, lstat(lower status of the population) much lower

