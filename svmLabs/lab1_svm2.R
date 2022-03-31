library(e1071)
library(rpart)
install.packages("mlbench")
library(mlbench)
data(Ozone)
## split data into a train and test set
index <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset <- na.omit(Ozone[testindex,-3])
trainset <- na.omit(Ozone[-testindex,-3])
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001) #pretty high cost, lower margin 
svm.pred <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex) #only has one value 13.07

