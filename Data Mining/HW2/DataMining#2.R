library(PerformanceAnalytics);library(tidyverse);library(gridExtra)
######################################
## LAB
######################################
x <- c(1,3,2,5); x
x = c(1,6,2); x
y = c(1,4,3); y

length(x); length(y); x+y
ls(); rm(x, y); ls()
rm(list = ls())

x = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2); x
x = matrix(data = c(1,2,3,4), 2, 2, byrow = T); x
sqrt(x); x^2

x = rnorm(50); y = x + rnorm(50, mean = 50, sd = 0.1); cor(x,y)
set.seed(1303); rnorm(50)
set.seed(3); y = rnorm(100); mean(y); var(y); sqrt(var(y)); sd(y)

x = rnorm(100); y = rnorm(100); plot(x,y)
plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis",
     main = "Plot of X vs Y")

#pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

x = seq(1,10); x; x = 1:10; x; x = seq(-pi, pi, length = 50); x
y = x; f = outer(x, y, function(x,y) cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa = (f - t(f))/2; contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

A = matrix(1:16, 4, 4); A; A[2,3]
A[c(1,3),c(2,4)]; A[1:3, 2:4]; A[1:2,]; A[,1:2]; A[1,]; A[-c(1,3),]; dim(A)

#install.packages('ISLR')
library(ISLR); Auto = ISLR::Auto
Auto = read.table('Auto.data')
Auto = read.table('Auto.data', header = T, na.strings = "?")
Auto = read.csv('Auto.csv', header = T, na.strings = "?")
dim(Auto)
Auto[1:4,]
Auto = na.omit(Auto); dim(Auto); names(Auto)

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto); plot(cylinders, mpg)
cylinders = as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)
plot(cylinders, mpg, col = "red", varwidth = T, horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T, xlab= "cylinders", ylab = "MPG")
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower, mpg); identify(horsepower, mpg, name)
summary(Auto); summary(mpg)
######################################
## EXERCISE9
######################################
#(a)
head(Auto); str(Auto)
#(b) & (c)
summary(Auto); range(Auto$mpg); sqrt(diag(cov(Auto[,-9])))
#(d)
Auto2 <- Auto[-c(10:85),]; summary(Auto2); sqrt(diag(cov(Auto2[,-9])))
#(e)
chart.Correlation(Auto[,-9], histogram = F, pch = 20); ggplot(Auto)
#(f)
colnames(Auto); Auto$origin <- as.factor(Auto$origin)
Auto$cylinders <- as.factor(Auto$cylinders)
Auto %>% 
  gather(displacement, horsepower, weight , acceleration, 
         year, key= 'Var', value = 'value') %>% 
  select(mpg, Var, value) %>% 
  ggplot(aes(x = value ,y = mpg)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(title = 'Quantitative Variable') +
  facet_wrap(Var ~., scales = 'free', ncol = 5) + theme_bw() +
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'),
        strip.text  = element_text(size = 14))
Auto %>% 
  gather(origin, cylinders, key= 'Var', value = 'value') %>% 
  select(mpg, Var, value) %>% 
  ggplot(aes(x = value ,y = mpg, group = value)) +
  geom_boxplot() +
  labs(title = 'Qualitative Variable') +
  facet_wrap(Var ~., scales = 'free', ncol = 5) + theme_bw() +
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'),
        strip.text  = element_text(size = 14))
######################################
## EXERCISE10
######################################
#(a)
Boston <- MASS::Boston; str(Boston)
#(b)
chart.Correlation(Boston, histogram = F, size = 15)
#(c)
colnames(Boston)
Boston %>% 
  gather(-crim, -chas, value = 'value',key = 'Var') %>% 
  ggplot(aes(x = value, y = crim)) +
  geom_point() + theme_bw() +
  facet_wrap(~Var, scales = 'free') +
  labs(title = 'Quantitative Variable') +
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'),
        strip.text  = element_text(size = 14))
ggplot(Boston, aes(x= factor(chas), y= crim, group = chas)) +
  geom_boxplot() + coord_flip() + ylim(c(-5,100)) +
  labs(title = 'Qualitative Variable',x='chas') + theme_bw() +
 theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'),
        strip.text  = element_text(size = 14))
#(d)
p1 <- ggplot(Boston, aes(x=1:nrow(Boston), y =crim)) + geom_point() +
  theme_bw() + labs(x = 'observations', title = 'Crim')+
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'))
p2 <- ggplot(Boston, aes(x=1:nrow(Boston), y =tax)) + geom_point() +
  theme_bw() + labs(x = 'observations', title = 'Tax')+
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'))
p3 <- ggplot(Boston, aes(x=1:nrow(Boston), y =ptratio)) + geom_point() +
  theme_bw() + labs(x = 'observations', title = 'Ptratio')+
  theme(axis.title = element_text(size = 14),
        title = element_text(size =16, face = 'bold'))
grid.arrange(p1,p2,p3, ncol =3)
#(e)
table(Boston$chas)
#(f)
median(Boston$ptratio)
#(g)
cheap <- Boston %>% filter(medv == min(medv)); cheap
rbind(sapply(Boston,quantile),
      sapply(Boston,mean))
#(h)
nrow(Boston[Boston$rm>7,]); nrow(Boston[Boston$rm>8,])
rbind(sapply(Boston[Boston$rm>8,], mean),
      sapply(Boston, median))
