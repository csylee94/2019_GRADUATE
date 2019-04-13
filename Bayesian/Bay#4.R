library(rjags); library(coda)

modelString = "
model
{
  for(i in 1:n){
    #fixed node
    x[i] ~ dnorm(mu, invsigsq) #데이터의 분포
  }
  #random node
  mu ~ dnorm(mu0, invsigsq0)   #사전분포
  invsigsq ~ dgamma(a,b) 
  sigsq <- 1/invsigsq          #모수의 변환
  #fixed node
  mu0 <- 10                    #상수값 지정
  invsigsq0 <- 1/25
  a <- 0.5
  b <- 1
}
"
writeLines(modelString, "model_ex5_1.txt")
#입력 데이터
dataList = list(n = 10, x = c(10,13,15,11,9,18,20,17,23,21))
#초기치
initList = list(mu = 10, invsigsq = 1/25)
#랜덤한 초기치도 사용 가능
initList = function(x){
  resampledX=sample(x, replace = T)
  muInit = sum(resampledX)/length(resampledX)
  invsigsqInit = (1/sd(resampledX))^2*0.999+0.01
  return(list(mu = muInit, invsigsq = invsigsqInit))
}

jagsModel = jags.model(file = 'model_ex5_1.txt', data = dataList, inits = initList,
                       n.chains = 3, n.adapt = 500)
#burn-in
update(jagsModel, n.iter = 500)
#MCMC 표본 추출
codaSamples = coda.samples(jagsModel, variable.names = c("mu","sigsq"), n.iter = 5000)
par(mfrow = c(1,2))
coda::traceplot(codaSamples[,"mu"], main = "", ylab = quote('mu'))
acf(codaSamples[,"mu"][[1]], plot = T, main = "")
coda::traceplot(codaSamples[,"sigsq"], main = "", ylab = quote('sigma^2'))
acf(codaSamples[,"sigsq"][[1]], plot = T, main = "")

gelman = gelman.diag(codaSamples)
gelman.1 = as.matrix(gelman$psrf)
if(max(gelman.1) > 1.1) cat("Warning: Gelman Shrink Factor > 1.1",'\n')
gelman.2 = as.matrix(gelman$mpsrf)
if(max(gelman.2) > 1.1) cat("Warning: Gelman Multivariate Shrink Factor > 1.1",'\n')

#-- Check MCMC efficiency -- #
mcmcSamples.combined = mcmc(codaSamples[[1]])
n.chains = 3
if(n.chains > 1) for(ich in 2:n.chains){
  mcmcSamples.combined = rbind(mcmcSamples.combined, mcmc(codaSamples[[ich]]))
  }
ESS = effectiveSize(mcmcSamples.combined)
cat("Effective Sample size = ", ESS)
#사후 추론
MuSamples = as.matrix(codaSamples[,"mu"])
SigSamples = as.matrix(codaSamples[,"sigsq"])
#주변 사후 밀도함수
par(mfrow = c(1,2))
plot(density(MuSamples), main = "", xlab= bquote(mu), ylab = "posterior density")
plot(density(SigSamples), main = "", xlab= bquote(sigma^2), ylab = "posterior density")
AcceptRate = 1 - rejectionRate(codaSamples); AcceptRate

##-------EXAMPLE------##
dat = read.csv("immigrants.csv")
y = dat$wage
n = length(y)
X = cbind(rep(1,n), dat$sp, dat$lit)
p = ncol(X)

a = 1
b = 1
XtX = t(X) %*% X
XtX.inv = solve(XtX)
Xty = t(X) %*% y
beta.hat = as.vector(XtX.inv %*% Xty)
sigsq.hat = sum((y - X %*% beta.hat)^2)/(n-p)
beta0 = beta.hat
Sig0 = diag(diag(XtX.inv)) * sigsq.hat * 100
Sig0.inv = solve(Sig0)

modelString = "
model{
  for(i in 1:length(y)){
    y[i] ~ dnorm(inprod(X[i,], beta[]), invsigsq)
  }
  beta[1:length(beta0)] ~ dmnorm(beta0[], Sig0.inv[,])
  invsigsq ~ dgamma(a,b)
  sigsq = 1/invsigsq
}
"
writeLines(modelString, "model_reg.txt")

dataList = list(X = X, y = y, a = a, b = b, beta0 = beta0, Sig0.inv = Sig0.inv)
initsList = list(beta = beta.hat, invsigsq = 1/sigsq.hat)
nChains = 3

jagsModel = jags.model(file = "model_reg.txt", data = dataList, inits = initsList,
                       n.chains = nChains, n.adapt = 500)
update(jagsModel, n.iter = 1000)
codaSamples = coda.samples(jagsModel, variable.names = c('beta','sigsq'),
                           n.iter = 30000)
para.names = variable.names(codaSamples[[1]])
#경로그림 & 자기상관
par(mfrow = c(4,2))
for(i in 1:4){
  coda::traceplot(codaSamples[,i], main = "", ylab = para.names[i])
  acf(codaSamples[,i][[1]], plot = T, main = para.names[i])
}
#savePlot(file="Fig_5_7", type = c('bmp'), device = dev.cur())
MCMCSamples = as.matrix(codaSamples)
HPD = round(apply(MCMCSamples, 2, quantile, probs = c(0.025, 0.975)),4)
#사후밀도함수와 95% 사후구간
par(mfrow = c(2,2))
for(i in 1:4){
  plot(density(MCMCSamples[,i]), main = "", xlab = para.names[i], col = 'blue' )
  abline(v=HPD[,i], col = 2)
}
#savePlot(file="Fig_5_8", type = c('bmp'), device = dev.cur())
