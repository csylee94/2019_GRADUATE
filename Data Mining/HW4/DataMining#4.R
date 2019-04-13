##################################################
## LABS
##################################################
library(ISLR)
names(Smarket); dim(Smarket)
summary(Smarket); cor(Smarket)
cor(Smarket[,-9])
attach(Smarket); plot(Volume)

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, family = binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef        
summary(glm.fits)$coef[,4]
glm.probs = predict(glm.fits, type = 'response')
glm.probs[1:10]
contrasts(Direction)
glm.pred = rep('Down', 1250)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)
(507 + 145)/1250
mean(glm.pred == Direction)
train = (Year < 2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = 'response')
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial,
               subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = 'response')
glm.pred = rep("Down" , 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106/(106 + 76)
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5),
                                       Lag2 = c(1.1, -0.8)), type = 'response')

library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1] > 0.5)
sum(lda.pred$posterior[,1] < 0.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1] > 0.9)

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

library(class)
train.X = cbind(Lag1, Lag2)[train, ]
test.X = cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83+43)/252
knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)
9/(68 + 9)
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4/15

glm.fits = glm(Purchase ~., data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
11/33

##################################################
## EXERCISE 4.10
##################################################
summary(Weekly)
cor(Weekly[,-9])
library("PerformanceAnalytics"); chart.Correlation(Weekly[,-9], histogram=TRUE, pts = 19)

glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = Weekly, family = binomial); summary(glm.fits)
glm.probs = predict(glm.fits, type = 'response'); contrasts(Weekly$Direction)
glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Weekly$Direction); mean(glm.pred == Weekly$Direction)

train = (Weekly$Year < 2009)
Weekly.0910 = Weekly[!train, ]

glm.fits = glm(Direction ~ Lag2, data = Weekly, family = binomial,subset = train); summary(glm.fits)
glm.probs = predict(glm.fits, type = 'response', newdata = Weekly.0910)
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Weekly.0910$Direction); mean(glm.pred == Weekly.0910$Direction)

library(MASS)
lda.fits = lda(Direction ~ Lag2, data = Weekly, subset = train); summary(lda.fits) 
lda.pred <- predict(lda.fits, newdata = Weekly.0910); lda.class = lda.pred$class
table(lda.class, Weekly.0910$Direction);mean(lda.class == Weekly.0910$Direction)

qda.fits = qda(Direction ~ Lag2, data = Weekly, subset = train); summary(qda.fits)
qda.pred <- predict(qda.fits, newdata = Weekly.0910); qda.class = qda.pred$class
table(qda.class, Weekly.0910$Direction); mean(qda.class == Weekly.0910$Direction)

train.X = as.matrix(Weekly$Lag2[train])
test.X = as.matrix(Weekly$Lag2[!train])
train.Direction = Weekly$Direction[train]
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Weekly.0910$Direction)
mean(knn.pred == Weekly.0910$Direction)

lda.fits <- lda(Direction~Lag2+I(Lag1^2), data= Weekly, subset = train)
lda.pred <- predict(lda.fits, newdata = Weekly.0910)$class
table(lda.pred, Weekly.0910$Direction)
mean(lda.pred == Weekly.0910$Direction) 
##################################################
## EXERCISE 4.11
##################################################
Auto$mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
table(Auto$mpg01)
chart.Correlation(Auto[,-c(1,9)], histogram=TRUE, pts = 25)
set.seed(1)
train.ind <- sample(1:nrow(Auto), nrow(Auto)*0.7 , replace=F)
train <- Auto[train.ind,]; test <- Auto[-train.ind,]
names(Auto)
lda.fits = lda(mpg01 ~ displacement + horsepower + weight + acceleration, data = train)
lda.pred <- predict(lda.fits, newdata = test); lda.class = lda.pred$class
table(lda.class, test$mpg01); 1 - mean(lda.class == test$mpg01)

qda.fits = qda(mpg01 ~ displacement + horsepower + weight + acceleration, data = train)
qda.pred <- predict(qda.fits, newdata = test); qda.class = qda.pred$class
table(qda.class, test$mpg01); 1 - mean(qda.class == test$mpg01)

glm.fits = glm(mpg01 ~ displacement + horsepower + weight + acceleration, data = train)
glm.probs = predict(glm.fits, type = 'response', newdata = test)
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.pred, test$mpg01); 1 - mean(glm.pred == test$mpg01)

train.X <- cbind(train$displacement, train$horsepower, train$weight, train$acceleration)
test.X <- cbind(test$displacement, test$horsepower, test$weight, test$acceleration)
knn.pred <- knn(train.X, test.X, train$mpg01, k= 3)
table(knn.pred, test$mpg01); mean(knn.pred != test$mpg01)

