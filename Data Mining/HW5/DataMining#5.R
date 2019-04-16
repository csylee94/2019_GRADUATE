#############################################
## LAB
#############################################
library(ISLR); set.seed(1); train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
attach(Auto); mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
set.seed(2); train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
glm.fit = glm(mpg ~ horsepower, data = Auto); coef(glm.fit)
lm.fit = lm(mpg ~ horsepower, data = Auto); coef(lm.fit)
library(boot); cv.err = cv.glm(Auto, glm.fit); cv.err$delta; cv.error = rep(0,5)
for (i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
};cv.error
set.seed(17); cv.error.10 = rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}; cv.error.10
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X)+var(Y)-2*cov(X,Y)))
}; alpha.fn(Portfolio, 1:100)
set.seed(1); alpha.fn(Portfolio, sample(100,100, replace = T)); boot(Portfolio, alpha.fn, R=1000)
boot.fn = function(data, index) return(coef(lm(mpg ~ horsepower, data = data ,subset = index)))
boot.fn(Auto, 1:392); set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T)); boot.fn(Auto, sample(392, 392, replace = T))
boot(Auto, boot.fn, 1000); summary(lm(mpg ~ horsepower, data = Auto))$coef
boot.fn = function(data, index) coefficients(lm(mpg ~ horsepower + I(horsepower^2), data = data, subset = index))
set.seed(1); boot(Auto, boot.fn, 1000); summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
#############################################
## EXERCISE 5.2
#############################################
x = 1:10000; plot(x, 1-(1-1/x)^x, xlab ='n', ylab = 'Prob.')
set.seed(1); store = rep(NA, 10000); for(i in 1:10000) store[i] = sum(sample(1:100, rep = T) == 4) > 0
mean(store)
#############################################
## EXERCISE 5.5
#############################################
require(ISLR); data(Default); set.seed(1);
fit1 = glm(default ~ income + balance, data =Default, family = binomial)
summary(fit1); set.seed(109); train = sample(nrow(Default), nrow(Default)*0.7)
fit2 = glm(default ~ income + balance, data=Default, family=binomial, subset=train)
pred.prob = predict(fit2, Default[-train,], type="response")
pred.class = ifelse(pred.prob > 0.5, "Yes", "No")
table(pred.class, Default[-train,]$default)
round(mean(Default[-train,]$default != pred.class),3)
fit3 = glm(default ~ income + balance + student, data=Default, family=binomial, subset=train)
pred.prob = predict(fit2, Default[-train,], type="response")
pred.class = ifelse(pred.prob > 0.5, "Yes", "No")
table(pred.class, Default[-train,]$default); round(mean(Default[-train,]$default != pred.class),3)
#############################################
## EXERCISE 5.7
#############################################
data(Weekly); set.seed(109)
fit1 = glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial); summary(fit1)
fit2 = glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial, subset = 2:nrow(Weekly)); summary(fit2)
ifelse(predict(fit2, Weekly[1,], type="response") > 0.5, "Up", "Down"); Weekly[1,]$Direction
set.seed(109); loocv.err <- rep(0,nrow(Weekly))
for (i in 1:nrow(Weekly)) {
  myfit = glm(Direction ~ Lag1 + Lag2, data=Weekly[-i,], family=binomial)
  mypred = ifelse(predict(myfit, Weekly[i,], type="response") > 0.5, "Up", "Down")
  loocv.err[i] <- ifelse(Weekly[i,]$Direction == mypred, 0, 1)
}; str(loocv.err); mean(loocv.err)
#############################################
## EXERCISE 5.9
#############################################
require(MASS); require(boot); data(Boston)
mu.hat = mean(Boston$medv); mu.hat
medv.sd = sd(Boston$medv)/sqrt(nrow(Boston)); medv.sd
set.seed(109); mean.fn = function(var, ind) return(mean(var[ind]))
boot.resid <- boot(Boston$medv, mean.fn, R=1000); boot.resid
round(cbind(boot.resid$t0 - 2*sd(boot.resid$t), boot.resid$t0 + 2*sd(boot.resid$t)),3)
round(t.test(Boston$medv)$conf.int,3)
medv.median = median(Boston$medv); medv.median
set.seed(109); median.fn = function(var, ind) return(median(var[ind]))
boot.resid = boot(Boston$medv, median.fn, R=1000); boot.resid
medv.10 = quantile(Boston$medv, 0.1); medv.10
set.seed(109); quantile10.fn = function(var, ind) return(quantile(var[ind], 0.1))
boot.resid = boot(Boston$medv, quantile10.fn, R=1000); boot.resid
