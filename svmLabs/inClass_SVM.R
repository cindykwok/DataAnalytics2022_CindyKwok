??Kyphosis
# cost small = margin wide >> more support vectors on margin or violate (misclassify) margin 

install.packages("e1071")
library(e1071)
set.seed(1)

# using svm() func to fit support vector classifier for given val of cost param 
# demonsrating on 2-D example , plot resulting decision boundary 

#generating observations, which belongs to 2 classes 
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,]+1

x
y

#checking if classes are linearly separable
plot(x, col=(3-y)) #here, we see the red data point on the left is on other side of the plane compared to the rest of the red data points

#because of the one red data point, there classes are not linearly separable 
#now >> fitting the suport vector classifier -- in order to use svm() function to perform classification, 
# must encode response as a factor variable > create dataframe with respnse coded as a factor 

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
#lower cost ex: .1 = more support vectors (the crosses)

#FALSE arg tells functino to not scale ach feature to have mean 0 or std 1

#plot support vector classifier obtained 
plot(svmfit, dat)
#args to plot.svm() are output of call to svm() and data used in the call to svm()
#region of feature space assigned to -1 class  >> in yellow (on my computer)
#region assigned to +1 class >> in red (on my computer )

#descision boundary (from plot) b/t the two classes is linear because of arg kernel = "linear"
#can see that only one obs was misclassified (the red x on bottom)

#looking closer at the plot:
  # support vectors are plotted as crosses, remaining obs are plotted as circles 

#see identity of sv by:
svmfit$index

#basic info
summary(svmfit) #>> tells cost 10 used, 7 support vectors, 4 in one class 3 in other

#################################################

#testing with smaller cost, expect more support vectors b/c smaller cost = wider margin = more sv

svmfit <- svm(y~., data = dat, kernel = "linear", cost = .1, scale = FALSE)

plot(svmfit, dat) #can see much more sv based on number of crosses 

svmfit$index #16 svs

summary(svmfit) 


##################################################

#e1071 library has tune() function that performs cross-validation 

set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear", ranges = list(cost = c(.001, .01, .1, 1, 5, 10, 100)))

#can easily access cross-validation errors for each of these models using summary() command
summary(tune.out)
#cost = .1 results in lowest c-v error rate 
#tune() function stores best model obtained >> can be accessed by:
bestmod = tune.out$best.model
summary(bestmod)

#predict() function can be used to predict class label on set of test obs at any given val of the cost param
#generate test data set:

xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1,1), 20, rep = TRUE)
xtest[ytest == 1,] = xtest[ytest==1,]+1
testdat = data.frame(x=xtest, y = as.factor(ytest))

#predict class labels of the test obs using best model obtained thru cross-validation to make prediction
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

# the diagonal = properly classified, can see 1 obs was incorrecly classified, 19 of test obs was correctly classified 

svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE) #changing cost from .1 to .01
ypred=predict(svmfit ,testdat)
table(predict=ypred, truth=testdat$y) #now 2 obs were incorrectly classified b/c smaller cost = wider margin = more possibility to violate 

# consider a situation in which the two classes are linearly separable
# can find a separating hyperplane using the svm() function
# separate the two classes in our simulated data so that they are linearly separable: 
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

# the observations are just barely linearly separable >> fit the support vector classifier and plot resulting hyperplane
# use large value of cost so that no observations are misclassified (large cost = narrow margin)

dat=data.frame(x=x,y=as.factor(y))
svmfit <-svm(y~., data=dat, kernel="linear", cost=1e5) #100,000
summary(svmfit)
plot(svmfit,dat) #3 support vectors,no training errors 

#because obs are very close to decision boundary, model will perform poorly on test data >> try smaller value of cost 
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit ,dat) #misclassified one (red cross (sv) on yellow section past plane)


