## Initial Setting
set.seed(109); require(coda); library(MASS)
mu0 <- 10; sig0sq <- 25; a <- 0.5; b <- 1
x <- c(10, 13, 15, 11, 9, 18, 20, 17, 23, 21)
dataList = list(x=x, mu0 = mu0, sig0sq = sig0sq, a=a, b=b)

###########################################################
## 공통 사용 함수
###########################################################
post.normal_mu_sigsq = function(theta, dataList){
  x = dataList$x
  mu0 = dataList$mu0
  sig0sq = dataList$sig0sq
  a = dataList$a
  b = dataList$b
  
  mu = theta[1]; sigsq = theta[2]
  #beta와 sigmasq의 결합사후밀도함수
  f = exp(-0.5*length(x)*log(sigsq) - 0.5*sum((x-mu)^2)/sigsq - 
            0.5*(mu-mu0)^2/sig0sq - (a+1)*log(sigsq) - b/sigsq)
  return(f)
}

### Generate random initial values ###
inits.random = function(x){
  resampledX = sample(x, replace = T)
  muInit = mean(resampledX)
  sigsqInit = var(resampledX)
  return(list(mu = muInit, sigsq = sigsqInit))
}

###########################################################
## 기존 알고리즘(등분산 가정)
###########################################################
## Random Walk Metropolis Algorithm ##
Metropolis_normal_mu_sigsq = function(nsim, nburn, delta, dataList, initsList, option = 1){
  #-- initial values of mu and log.sigsq
  mu = initsList$mu
  log.sigsq = log(initsList$sigsq)
  theta.curr = c(mu, log.sigsq)
  p = length(theta.curr)
  
  #-- Start iterations
  para.samples = matrix(0, nsim, p)
  for(iter in 1:(nsim + nburn)){
    z = rnorm(p, 0, 1)
    theta.prop = z * delta + theta.curr
    mu.curr = theta.curr[1]
    sigsq.curr = exp(theta.curr[2])
    mu.prop = theta.prop[1]
    sigsq.prop = exp(theta.prop[2])
    alpha = post.normal_mu_sigsq(c(mu.prop, sigsq.prop), dataList)/
      post.normal_mu_sigsq(c(mu.curr, sigsq.curr), dataList) * sigsq.prop/sigsq.curr
    if(runif(1) < alpha) {theta.next <- theta.prop} else {theta.next <- theta.curr}
    
    theta.curr = theta.next
    
    if(iter > nburn & option == 1) para.samples[iter-nburn, ] = c(theta.next[1], exp(theta.next[2]))
    if(iter > nburn & option == 2) para.samples[iter-nburn, ] = c(theta.next[1], theta.next[2])
  }
  #-- End iterations
  return(para.samples)
}

###########################################################
## 수정 알고리즘(이분산 가정)
###########################################################
## Random Walk Metropolis Algorithm ##
Metropolis_normal_mu_sigsq2 = function(nsim, nburn, delta.mat, dataList, initsList, option = 1){
  #-- initial values of mu and log.sigsq
  mu = initsList$mu
  log.sigsq = log(initsList$sigsq)
  theta.curr = c(mu, log.sigsq)
  p = length(theta.curr)
  delta = sqrt(diag(delta.mat))
  
  #-- Start iterations
  para.samples = matrix(0, nsim, p)
  for(iter in 1:(nsim + nburn)){
    z = rnorm(p, 0, 1)
    theta.prop = z * delta + theta.curr
    mu.curr = theta.curr[1]
    sigsq.curr = exp(theta.curr[2])
    mu.prop = theta.prop[1]
    sigsq.prop = exp(theta.prop[2])
    alpha = post.normal_mu_sigsq(c(mu.prop, sigsq.prop), dataList)/
      post.normal_mu_sigsq(c(mu.curr, sigsq.curr), dataList) * sigsq.prop/sigsq.curr
    if(runif(1) < alpha) {theta.next <- theta.prop} else {theta.next <- theta.curr}
    
    theta.curr = theta.next
    
    if(iter > nburn & option == 1) para.samples[iter-nburn, ] = c(theta.next[1], exp(theta.next[2]))
    if(iter > nburn & option == 2) para.samples[iter-nburn, ] = c(theta.next[1], theta.next[2])
    
  }
  
  #-- End iterations
  return(para.samples)
}

###########################################################
## 문제 풀이
###########################################################
## 1)
###  Multi Chain MCMC/ 등분산
nChains = 3; nsim = 2000; nburn = 500; p = 2
mcmc.samples = array(0, dim = c(nsim, p, nChains)) #array to save samples
delta = 1

### MCMC 수행
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq(nsim, nburn, delta, dataList, initsList, option = 2)
}

### 베이지안 사후 추론/ Posterior inference
mcmc.samples.combined = rbind(mcmc.samples[,,1], mcmc.samples[,,2], mcmc.samples[,,3])
para.hat = apply(mcmc.samples.combined, 2, mean); round(para.hat, 3)
para.var.hat = apply(mcmc.samples.combined, 2, var); round(para.var.hat,3)
delta.mat <- diag(para.var.hat * 2.4); round(delta.mat, 3)

## 4)
### 수정된 알고리즘으로 MCMC 수행(delta1 = sqrt(5.656), delta2 = sqrt(0.515))
mcmc.samples = array(0, dim = c(nsim, p, nChains))
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq2(nsim, nburn, delta.mat, dataList, initsList, option = 2)
}
mcmc.samples.combined = rbind(mcmc.samples[,,1], mcmc.samples[,,2], mcmc.samples[,,3])
para.hat = apply(mcmc.samples.combined, 2, mean); round(para.hat, 3)
para.var.hat = apply(mcmc.samples.combined, 2, var); round(para.var.hat,3)

### Rejection Rate
Metro.draws = mcmc(mcmc.samples.combined)
accept.rate = 1 - rejectionRate(Metro.draws)
accept.rate

## 5) delta값 조정
delta.mat <- diag(c(11.5, 1.05))
mcmc.samples = array(0, dim = c(nsim, p, nChains)) #array to save samples
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq2(nsim, nburn, delta.mat, dataList, initsList, option = 2)
}
mcmc.samples.combined = rbind(mcmc.samples[,,1], mcmc.samples[,,2], mcmc.samples[,,3])
Metro.draws = mcmc(mcmc.samples.combined)
accept.rate = 1 - rejectionRate(Metro.draws); accept.rate

## 6)
##############################
###  Multi Chain MCMC/ 등분산
##############################
nsim = 20000; nburn = 5000; mcmc.samples = array(0, dim = c(nsim, p, nChains)) #array to save samples
delta = 1
### MCMC 수행
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq(nsim, nburn, delta, dataList, initsList, option = 2)
}
### 베이지안 사후 추론/ Posterior inference
mcmc.samples.combined = rbind(mcmc.samples[,,1], mcmc.samples[,,2], mcmc.samples[,,3])
### Convergence diagnostics
mu.samples = mcmc.samples[,1,]
sigsq.samples = mcmc.samples[,2,]
par(mfrow = c(2,2))
### mu 경로그림
plot(mu.samples[1:2000,1], type = "l", xlab = "iteration", ylab = quote(mu))
lines(mu.samples[1:2000,2], col = 2)
lines(mu.samples[1:2000,3], col = 3)
### mu 사후밀도함수
plot(density(mu.samples[,1]), xlab = quote(mu), ylab = 'Posterior density', main = "")
lines(density(mu.samples[,2]), col = 2)
lines(density(mu.samples[,3]), col = 3)
### sigsq 경로그림
plot(sigsq.samples[1:2000,1], type = "l", xlab = "iteration", ylab = quote(log(sigma^2)))
lines(sigsq.samples[1:2000,2], col = 2)
lines(sigsq.samples[1:2000,3], col = 3)
### sigsq 사후밀도함수
plot(density(sigsq.samples[,1]), xlab = quote(log(sigma^2)), ylab = 'Posterior density', main = "")
lines(density(sigsq.samples[,2]), col = 2)
lines(density(sigsq.samples[,3]), col = 3)
### Gelman 상수
samples.1 = mcmc(mcmc.samples[,,1])
samples.2 = mcmc(mcmc.samples[,,2])
samples.3 = mcmc(mcmc.samples[,,3])
codaSamples = mcmc.list(list(samples.1, samples.2, samples.3)); gelman = gelman.diag(codaSamples); gelman
### Rejection Rate
Metro.draws = mcmc(mcmc.samples.combined)
accept.rate = 1 - rejectionRate(Metro.draws); accept.rate
### 베이지안 사후 추론/ Posterior inference
para.hat = apply(mcmc.samples.combined, 2, mean); round(para.hat, 3)
para.var.hat = apply(mcmc.samples.combined, 2, var); round(para.var.hat,3)
HPD = apply(mcmc.samples.combined, 2, function(x) quantile(x, c(0.025, 0.975)))
par(mfrow = c(1,2))
plot(density(mcmc.samples.combined[,1]), xlab = quote(mu), ylab = "", main = "")
abline(v = HPD[,1], lty = 2, col = 2)
plot(density(mcmc.samples.combined[,2]), xlab = quote(log(sigma^2)), ylab = "", main = "")
abline(v = HPD[,2], lty = 2, col = 2)
acf(mcmc.samples.combined)
##############################
###  Multi Chain MCMC/ 이분산
##############################
delta.mat <- diag(c(11.5, 1.05))
nsim = 20000; nburn = 5000; mcmc.samples = array(0, dim = c(nsim, p, nChains)) #array to save samples
### MCMC 수행
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq2(nsim, nburn, delta.mat, dataList, initsList, option = 2)
}
### 베이지안 사후 추론/ Posterior inference
mcmc.samples.combined = rbind(mcmc.samples[,,1], mcmc.samples[,,2], mcmc.samples[,,3])
### Convergence diagnostics
mu.samples = mcmc.samples[,1,]
sigsq.samples = mcmc.samples[,2,]
par(mfrow = c(2,2))
### mu 경로그림
plot(mu.samples[1:2000,1], type = "l", xlab = "iteration", ylab = quote(mu))
lines(mu.samples[1:2000,2], col = 2)
lines(mu.samples[1:2000,3], col = 3)
### mu 사후밀도함수
plot(density(mu.samples[,1]), xlab = quote(mu), ylab = 'Posterior density', main = "")
lines(density(mu.samples[,2]), col = 2)
lines(density(mu.samples[,3]), col = 3)
### sigsq 경로그림
plot(sigsq.samples[1:2000,1], type = "l", xlab = "iteration", ylab = quote(log(sigma^2)))
lines(sigsq.samples[1:2000,2], col = 2)
lines(sigsq.samples[1:2000,3], col = 3)
### sigsq 사후밀도함수
plot(density(sigsq.samples[,1]), xlab = quote(log(sigma^2)), ylab = 'Posterior density', main = "")
lines(density(sigsq.samples[,2]), col = 2)
lines(density(sigsq.samples[,3]), col = 3)
### Gelman 상수
samples.1 = mcmc(mcmc.samples[,,1])
samples.2 = mcmc(mcmc.samples[,,2])
samples.3 = mcmc(mcmc.samples[,,3])
codaSamples = mcmc.list(list(samples.1, samples.2, samples.3)); gelman = gelman.diag(codaSamples); gelman
### Rejection Rate
Metro.draws = mcmc(mcmc.samples.combined)
accept.rate = 1 - rejectionRate(Metro.draws); accept.rate
### 베이지안 사후 추론/ Posterior inference
para.hat = apply(mcmc.samples.combined, 2, mean); round(para.hat, 3)
para.var.hat = apply(mcmc.samples.combined, 2, var); round(para.var.hat,3)
HPD = apply(mcmc.samples.combined, 2, function(x) quantile(x, c(0.025, 0.975)))
par(mfrow = c(1,2))
plot(density(mcmc.samples.combined[,1]), xlab = quote(mu), ylab = "", main = "")
abline(v = HPD[,1], lty = 2, col = 2)
plot(density(mcmc.samples.combined[,2]), xlab = quote(log(sigma^2)), ylab = "", main = "")
abline(v = HPD[,2], lty = 2, col = 2)
