############################################
# Gibbs sampling example : 예 2.3 
############################################

M<-3000; m<-500  #burn-in=500;  nTotalIteration=3000
mu0<-10; sigsq0<-25; a<-0.5; b<-1
x<-c(10,13,15,11,9,18,20,17,23,21)
n<-length(x)
xbar<-mean(x); var.x<-var(x)
THETA<-matrix(nrow=M, ncol=2)

# initial value of sigsq
sigsq<-var.x
mu <- xbar

# Gibbs sampler
for(nsim in 1:M){
  
  # generate mu
  condpost.var<-1/(1/sigsq0+n/sigsq)
  condpost.mu<-condpost.var*(xbar/(sigsq/n) +mu0/sigsq0)
  mu<-rnorm(1, condpost.mu, sqrt(condpost.var))
  
  # generate sigsq
  condpost.a<-a+n/2
  condpost.b<-b+1/2*((n-1)*var.x+n*(xbar-mu)^2)
  sigsq<-1/rgamma(1, condpost.a, condpost.b)
  
  # save
  THETA[nsim,]<-c(mu,sigsq)
}

#### Fig 2.7 ####
par(mfrow=c(1,3))
plot(THETA[1:5,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:5,], lty=2)
for(i in 1:5) text(THETA[i, 1], THETA[i, 2], i)

plot(THETA[1:15,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:15,], lty=2)
for(i in 1:15) text(THETA[i, 1], THETA[i, 2], i)

plot(THETA[1:100,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:100,], lty=2)
for(i in 1:100) text(THETA[i, 1], THETA[i, 2], i)

#### Fig 2.8 ####
par(mfrow=c(1,1))
plot(THETA[m:M, 1], THETA[m:M, 2], xlab=expression(mu), ylab=expression(sigma^2), 
     main="sample of (mu, sigma^2)")


#### Fig 2.9 ####
par(mfrow=c(1,2))
plot(density(THETA[m:M, 1]), xlab=expression(mu), ylab="posterior", main="")
plot(density(THETA[m:M, 2]), xlab=expression(sigma^2), ylab="posterior", main="") 
