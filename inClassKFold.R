# k-Fold Cross Validation

# The cv.glm() function can also be used to implement k-fold CV.
# We once again, set a random seed and initialize a vector in which,
# we will store the CV errors corresponding to the polynomial fits of orders 1 to 10. 
# here the K =10
#CV = cross validation 

??cv.glm #?? is same as help
set.seed(20)
help("rep") 
cv.error.10 = rep(0,10) 

for(i in 1:10){  #polynomial fits of orders 1:10
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1] 
}
cv.error.10

# We still see little evidence that using cubic or higher-order polynomials terms,
# leads to lower test error than simply using a quadratics fit.
