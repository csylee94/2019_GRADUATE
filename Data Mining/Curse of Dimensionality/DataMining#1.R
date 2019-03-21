library(scatterplot3d); library(tidyverse); library(gridExtra)
set.seed(109)
mypts <- function(n, p){
  X <- matrix(rnorm(p * n), ncol = p)
  X <- X/sqrt(rowSums(X^2)) * sqrt(qchisq(runif(n, 0, pchisq(1, p)), p))
  return(X)
}
mypts2 <- function(n, p){
  X <- matrix(rnorm(p * n), ncol = p)
  X <- X/sqrt(rowSums(X^2)) * runif(n)^(1/p)
  return(X)
}
mymedmindis <- function(n, p, niter = 1000){
  minpts <- matrix(NA, nrow = niter, ncol = p)
  for(i in 1:niter){
    X <- matrix(rnorm(p * n), ncol = p)
    X <- X/sqrt(rowSums(X^2)) * sqrt(qchisq(runif(n, 0, pchisq(1, p)), p))
    minpts[i,] <- X[which.min(sqrt(rowSums(X^2))),]
  }
  return(median(sqrt(rowSums(minpts^2))))
}
mymedmindis2 <- function(n, p, niter = 1000){
  minpts <- matrix(NA, nrow = niter, ncol = p)
  for(i in 1:niter){
    X <- matrix(rnorm(p * n), ncol = p)
    X <- X/sqrt(rowSums(X^2)) * runif(n)^(1/p)
    minpts[i,] <- X[which.min(sqrt(rowSums(X^2))),]
  }
  return(median(sqrt(rowSums(minpts^2))))
}
medmindis <- function(n, p){
  (1-0.5^(1/n))^(1/p)
}

######PROBLEM2#####
x11 <- data.frame(mypts(100, 2)); y11 <- data.frame(mypts2(100,2))
x12 <- data.frame(mypts(1000, 2)); y12 <- data.frame(mypts2(1000, 2))
x13 <- data.frame(mypts(10000, 2)); y13 <- data.frame(mypts2(10000, 2))
p1 <-ggplot(y11, aes(x =X1, y = X2)) + geom_point() + 
  theme_classic() + ggtitle('N = 100') + 
  theme(plot.title = element_text(size = 18, face = "bold"))
p2 <- ggplot(y12, aes(x =X1, y = X2)) + geom_point() + 
  theme_classic() + ggtitle('N = 1000') + 
  theme(plot.title = element_text(size = 18, face = "bold"))
p3 <- ggplot(y13, aes(x =X1, y = X2)) + geom_point() + 
  theme_classic() + ggtitle('N = 10000') + 
  theme(plot.title = element_text(size = 18, face = "bold"))
grid.arrange(p1,p2,p3, ncol=3)

x21 <- data.frame(mypts(100, 3)); y21 <- data.frame(mypts2(100, 3))
x22 <- data.frame(mypts(1000, 3)); y22 <- data.frame(mypts2(1000, 3))
x23 <- data.frame(mypts(10000, 3)); y23 <- data.frame(mypts2(10000, 3))
par(mfrow=c(1,3))
scatterplot3d(x=y21$X1,y=y21$X2,z=y21$X3,main = 'N = 100',xlab='X1',ylab='X2',zlab='X3',grid = F)
scatterplot3d(x=y22$X1,y=y22$X2,z=y22$X3,main = 'N = 1000',xlab='X1',ylab='X2',zlab='X3',grid = F)
scatterplot3d(x=y23$X1,y=y23$X2,z=y23$X3,main = 'N = 10000',xlab='X1',ylab='X2',zlab='X3',grid = F)

x3 <- mypts(4,5); y3 <- mypts2(4,5)
round(cbind(x3,sqrt(rowSums(x3^2))),3); round(cbind(y3,sqrt(rowSums(y3^2))),3)

#####PROBLEM3#####
tt <-cbind(c(rep(500,6),rep(1000,6)),
           rep(c(rep(5,2),rep(10,2),rep(100,2)),2),
           rep(c(1000,10000),6))
rtt <- NA; rtt2 <- NA
for(i in 1:12){
  rtt <- rbind(rtt, 
               cbind(mymedmindis(tt[i,1],tt[i,2],tt[i,3]),
                     medmindis(tt[i,1],tt[i,2])))
  rtt2 <- rbind(rtt2, 
               cbind(mymedmindis2(tt[i,1],tt[i,2],tt[i,3]),
                     medmindis(tt[i,1],tt[i,2])))
}
round(rtt,4);round(rtt2,4)

#####GRAPH#####
myN <- c(100, 500, 1000, 2500, 5000, 7500, 10000); myp <- seq(1:10)
dat1 <- cbind(myN, sapply(myN, function(x) abs(mymedmindis(x, 5) - medmindis(x,5))))
dat2 <- cbind(myp, sapply(myp, function(x) abs(mymedmindis(1000, x) - medmindis(1000,x))))
dat3 <- cbind(myN, sapply(myN, function(x) abs(mymedmindis(1000, 5, x) - medmindis(1000,5))))
dat4 <- cbind(myN, sapply(myN, function(x) abs(mymedmindis2(x, 5) - medmindis(x,5))))
dat5 <- cbind(myp, sapply(myp, function(x) abs(mymedmindis2(1000, x) - medmindis(1000,x))))
dat6 <- cbind(myN, sapply(myN, function(x) abs(mymedmindis2(1000, 5, x) - medmindis(1000,5))))
colnames(dat1) <- c('N','error'); colnames(dat2) <- c('p','error'); colnames(dat3) <- c('niter','error')
colnames(dat4) <- c('N','error'); colnames(dat5) <- c('p','error'); colnames(dat6) <- c('niter','error')
p4 <- ggplot(as.data.frame(dat1),aes(N, error)) + geom_line() + geom_point() + 
  labs(title = '점의 개수의 변화에 따른 오차', x='Number of Points', y='abs(error)') +
  theme_minimal(base_family =  'AppleGothic')
p5 <-ggplot(as.data.frame(dat2),aes(as.factor(p), error)) + geom_point() + 
  labs(title = '차원의 변화에 따른 오차', x='Dimension', y='abs(error)') + 
  theme_minimal(base_family =  'AppleGothic')
p6 <- ggplot(as.data.frame(dat3),aes(niter, error)) + geom_line() + geom_point() + 
  labs(title = '반복수에 따른 오차', x='Number of Iterations', y='abs(error)') + 
  theme_minimal(base_family =  'AppleGothic')
grid.arrange(p4, p5, p6, ncol = 3)

d1 <- cbind(data.frame(rbind(dat1,dat4)),(c(rep('ALGO1',7),rep('ALGO2',7))))
d2 <- cbind(data.frame(rbind(dat2,dat5)),(c(rep('ALGO1',10),rep('ALGO2',10))))
d3 <- cbind(data.frame(rbind(dat3,dat6)),(c(rep('ALGO1',7),rep('ALGO2',7))))
colnames(d1) <- c('N','error','ALGO'); colnames(d2) <- c('p','error','ALGO'); colnames(d3) <- c('niter','error','ALGO')
p7<- ggplot(d1,aes(N, error, group = ALGO)) + geom_line(aes(linetype = ALGO)) + geom_point() + 
  labs(title = '점의 개수의 변화에 따른 오차', x='Number of Points', y='abs(error)') +
  theme_minimal(base_family =  'AppleGothic')
p8 <-ggplot(d2,aes(p, error, gourp = ALGO)) + geom_point() + geom_line(aes(linetype = ALGO)) +
  labs(title = '차원의 변화에 따른 오차', x='Dimension', y='abs(error)') + 
  theme_minimal(base_family =  'AppleGothic')
p9 <- ggplot(d3,aes(niter, error, group = ALGO)) + geom_line(aes(linetype = ALGO)) + geom_point() + 
  labs(title = '반복수에 따른 오차', x='Number of Iterations', y='abs(error)') + 
  theme_minimal(base_family =  'AppleGothic')
grid.arrange(p7, p8, p9, ncol = 3)

