bm <-"model{
 for (h in 1:H){
   Y[h]~dbinom(theta[h],n[h])
   theta[h]~dbeta(alpha,beta)
   res[h] <- (Y[h]-n[h]*theta[h])/sqrt(n[h]*theta[h]*(1-theta[h]))
 }
 alpha ~ dgamma(1,1)
 beta  ~ dgamma(1,1)
}"

writeLines(bm,'model.txt')
n <- c(143,187,323,122,164,405,239,482,195,177,581,301)
Y <- c(41,25,24,23,25,42,24,53,26,25,58,31)
H<-length(n)

data.h <- list(Y = Y, n = n,H=H)
library(rjags)

M1 <- jags.model('model.txt',data.h,n.adapt = 1000)
update(M1, 10000)
M2<-coda.samples(M1,variable.names = c('theta','alpha','beta','res'),n.iter=10000)

plot(M2)

summary(M2)
