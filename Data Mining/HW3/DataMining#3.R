#######################################################
## LAB
#######################################################
#install.packages('ISLR')
library(MASS);library(ISLR)
fix(Boston) #**
names(Boston)
lm.fit = lm(medv ~ lstat)
lm.fit = lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit = lm(medv ~ lstat); lm.fit; summary(lm.fit)
names(lm.fit); coef(lm.fit); confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(5, 10, 15)),
        interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)),
        interval = "prediction")
plot(lstat, medv); abline(lm.fit); abline(lm.fit, lwd = 3); abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red"); plot(lstat, medv, pch = "+"); plot(1:20, 1:20, pch = 1:20)
par(mfrow = c(2,2)); plot(lm.fit); par(mfrow = c(1,1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit)); which.max(hatvalues(lm.fit))
lm.fit = lm(medv ~ lstat + age, data = Boston); summary(lm.fit)
lm.fit = lm(medv ~ ., data = Boston);summary(lm.fit)
#install.packages('car')
library(car); vif(lm.fit)
lm.fit1 = lm(medv ~.-age, data = Boston) ;summary(lm.fit1)
lm.fit1 = update(lm.fit, ~.-age) #*
summary(lm(medv ~ lstat * age, data = Boston)) #hierachical
lm.fit2 = lm(medv ~ lstat + I(lstat^2)); summary(lm.fit2)
lm.fit = lm(medv ~ lstat); anova(lm.fit, lm.fit2)
par(mfrow = c(2, 2)); plot(lm.fit2)
lm.fit5 = lm(medv ~ poly(lstat, 5)) #*
summary(lm.fit5); summary(lm(medv~log(rm), data = Boston))
fix(Carseats); names(Carseats)
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats); summary(lm.fit)
attach(Carseats); contrasts(ShelveLoc)
LoadLibraries; LoadLibraries()
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries; LoadLibraries()
#######################################################
## EXERCISE 3.8
#######################################################
#(a)
data(Auto)
lm.fit1 = lm(mpg ~ horsepower, data = Auto); summary(lm.fit1)
predict(lm.fit1, data.frame(horsepower = 98),
        interval = 'confidence')
predict(lm.fit1, data.frame(horsepower = 98),
        interval = 'prediction')
#(b)
par(mfrow = c(1,1)); plot(Auto$horsepower, Auto$mpg); abline(lm.fit1, col = 'red', lwd = 2)
#(c)
par(mfrow = c(2,2)); plot(lm.fit1)
#######################################################
## EXERCISE 3.9
#######################################################
#(a)
pairs(Auto)
#(b)
cor(Auto[,-9])
#(c)
lm.fit = lm(mpg ~. -name, data = Auto); summary(lm.fit)
#(d)
par(mfrow = c(2,2)); plot(lm.fit)
#(e)
lm.fit2 = lm(mpg ~.^2, data = Auto[,-9]); step(lm.fit2)
lm.fit3 = lm(mpg ~. -name + cylinders:acceleration + cylinders:year + 
               displacement:weight + displacement:year + displacement:origin + 
               horsepower:acceleration + horsepower:year + acceleration:year + 
               acceleration:origin + year:origin, data = Auto); summary(lm.fit3)
#(f)
lm.fit4 = lm(mpg ~ poly(displacement, 2) + weight + year + origin, data = Auto); summary(lm.fit4)
#######################################################
## EXERCISE 3.13
#######################################################
set.seed(1)
#(a)
x = rnorm(100)
#(b)
eps = rnorm(100, 0, sd = .5)
#(c)
y = -1 + 0.5*x + eps
#(d)
par(mfrow = c(1,1)); plot(x, y)
#(e)
lm.fit1 = lm(y ~ x); summary(lm.fit1)
#(f)
plot(x, y); abline(lm.fit1, col = 'red', lwd = 3)
legend(x = 0.5, y = -2, 'fitted line', col = 'red',lwd = 3)
#(g)
lm.fit2 = lm(y ~ poly(x, 2)); summary(lm.fit2)
#(h)
x2 = rnorm(100); eps2 = rnorm(100, 0, sd = .3); y2 = -1 + 0.5*x2 + eps2
cbind(x2, eps2, y2)
lm.fit3 = lm(y2 ~ x2); summary(lm.fit3)
par(mfrow = c(1,2)); plot(x2, y2); plot(x2, y2); abline(lm.fit3, col = 'red', lwd = 3)
legend(x = 0.5, y = -2, 'fitted line', col = 'red',lwd = 3)
#(i)
x3 = rnorm(100); eps3 = rnorm(100, 0, sd = .7); y3 = -1 + 0.5*x3 + eps3
cbind(x3, eps3, y3)
lm.fit5 = lm(y3 ~ x3); summary(lm.fit5)
par(mfrow = c(1,2)); plot(x3, y3)
plot(x3, y3); abline(lm.fit5, col = 'red', lwd = 3)
legend(x = 1, y = -2, 'fitted line', col = 'red',lwd = 3)
lm.fit6 = lm(y3 ~ poly(x2, 3)); summary(lm.fit6)
#(j)
confint(lm.fit1); confint(lm.fit3); confint(lm.fit5)
#######################################################
## EXERCISE 3.14
#######################################################
#(a)
set.seed(1)
x1 = runif(100); x2 = 0.5*x1 + rnorm(100)/10; y = 2 + 2*x1 +0.3*x2 + rnorm(100)
#(b)
plot(x1, x2)
#(c)
lm.fit = lm(y ~ x1 + x2); summary(lm.fit)
#(d)
lm.fit1 = lm(y ~ x1); summary(lm.fit1)
#(e)
lm.fit2 = lm(y ~ x2); summary(lm.fit2)
#(g)
x1 = c(x1, 0.1); x2 = c(x2, 0.8); y = c(y, 6)
lm.fit = lm(y ~ x1 + x2); summary(lm.fit)
par(mfrow = c(2,2));plot(lm.fit)
lm.fit1 = lm(y ~ x1); summary(lm.fit1)
par(mfrow = c(2,2));plot(lm.fit1)
lm.fit2 = lm(y ~ x2); summary(lm.fit2)
par(mfrow = c(2,2));plot(lm.fit2)