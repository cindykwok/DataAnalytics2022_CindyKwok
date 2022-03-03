#using Cooks distance for outliers , visually
mtcars
head(mtcars)
str(mtcars)
dim(mtcars)
model1 <- lm(mpg ~ cyl + wt, data = mtcars)
model1
plot(model1, pch = 18, col = 'red', which = c(4))
plot(model1, pch = 18, col = 'red', which = c(3)) #residuals

# we can use cooks.distance() function 
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)
sort(round(CooksDistance, 5)) #round values to 5th decimal point 

# outlier detection: cook's distance -> it checks the influence of a data point 
# cook's distance is a summary of how much a regression model changes when ith obs is removed 

library(ISLR)
library(dplyr)
#using the Hitters dataset built in ISLR pkg
head(Hitters)
str(Hitters)
dim(Hitters)
#diff way of ommiting NA 
is.na(Hitters) #checking for vals that are NA
HittersNew <- na.omit(Hitters)
dim(HittersNew)

#using multivariate regression model using all features in data set 
#predicting salary of the baseball player
SalaryPredict <- lm(Salary ~ ., data = HittersNew)
summary(SalaryPredict) #multiple R^2 = 0.5461, adjusted R^2 = 0.5106, closer to 1 = better model 

#Cook's Distance 
cDistance <- cooks.distance(SalaryPredict)
influential <- cDistance[(cDistance > (3 * mean(cDistance, na.rm = TRUE)))] #seeing which players are pass threshold we set (3x mean)
influential
influentialnames <- names(influential) 
influentialnames
outliers <- HittersNew[influentialnames,]
help("anti_join")
HittersNewNoOutlier <- HittersNew %>% anti_join(outliers) #exclude the outliers 

#new model w/o outliers 
SalaryPredict2 <- lm(Salary ~ ., data = HittersNewNoOutlier)
summary(SalaryPredict2) # multiple R^2 = 0.6721, adjusted R^2 = 0.6445, can see it is closer to 1 than the data that included the outliers 



