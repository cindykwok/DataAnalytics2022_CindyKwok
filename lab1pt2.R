#in class exercise pt 1
#------------------------
multivariate <- read.csv("/Users/cindykwok/Desktop/CSCI 4600 Data Analytics /labs and assignments/multivariate.csv")
attach(multivariate)
head(multivariate)
str(multivariate)
View(multivariate)
help(lm) #fitting linear models
mm <- lm(Homeowners~Immigrant)
mm
# gets intercept = 107495, coeff of immigrnt var = -6657
# homeowners = 107495 + (-6657) * Immigrants 
plot(Homeowners~Immigrant)
help(abline)
abline(mm) #adds straight lines through current plot 
abline(mm, col = 2, lwd = 3)

#can pass in Immigrant [x] values and predict the Homeowners [y]
#ex: passing in 0 and 20 for Immigrants 
newImmigrants <- data.frame(Immigrant = c(0,20)) #passing in the vals as list 
mm %>% predict(newImmigrants) 
install.packages("dplyr") #needed for %>%
library(dplyr)
mm %>% predict(newImmigrants)

abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

# in class exercise pt 2
#-------------------------
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt,mpg, data = mtcars) #another way of writing prev line
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() #adding "layers" 
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure) #adding points to the prev graph

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="blue")
qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature, pressure, data = pressure, geom = "line") #another way of writing prev line
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_point() + geom_line()

#creating bar graphs 
#--------------------
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #table of counts 
qplot(mtcars$cyl, binwidth = .5) #cyl continuous 
qplot(factor(mtcars$cyl)) #discrete
qplot(factor(cyl), data = mtcars) #bar graph
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

#creating histograms using ggplot
#--------------------------------
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) #specify bin num w/ breaks
hist(mtcars$mpg, breaks = 5)

qplot(mpg, data = mtcars, binwidth = 4)
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 4) #same as prev line but using ggplot 
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)

#creating box plots using ggplot
#-------------------------------
plot(ToothGrowth$supp, ToothGrowth$len) #creating box plot
boxplot(len~supp, data = ToothGrowth) #if vars are in same data frame, can combine vars on x-axis
boxplot(len~supp+ dose, data= ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot") #same but using ggplot
qplot(supp ,len, data = ToothGrowth, geom = "boxplot") #can use this syntax if vars in same data frame, gives same result as line above

#equiv to above but in ggplot2
ggplot(ToothGrowth, aes(x = supp, y = len)) + geom_boxplot()
#using 3 sep vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot") #same thing as above line, can do if all vars in same data frame

ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()
