library(rjags)
library(coda)
library(ggplot2)

hdl.data <- read.table("/home/elkip/Datasets/hdl-data.txt",sep="\t",header=T)
hdl.data$BMI = hdl.data$BMI5
hdl.data <- na.omit(hdl.data)

# Exercise 0: Model the linear trend with log HDL
hdl.data$log_hdl = log(hdl.data$HDL5)

model.hdl1 <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dnorm(mu[i],tau)
    mu[i] <- beta.0 + beta.sex*(sex[i]-1) + beta.bmi*(bmi[i]-mean.bmi) + beta.age*(age[i]-mean.age)
   }
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);
  beta.age ~ dnorm(0, 0.0001);

  mean.bmi <- mean(bmi[])  
  mean.age <- mean(age[])
   }"

# Formatted data for JAGS
hdl.dataf <- list(N = as.numeric(nrow(hdl.data)),
               hdl = hdl.data$log_hdl,
               bmi = hdl.data$BMI5,
               sex = hdl.data$SEX,
               age= hdl.data$AGE5)

jags.hdl1 <- jags.model(textConnection(model.hdl1), data=hdl.dataf, n.adapt=1500)
update(jags.hdl1, n.iter=1000)
test.hdl <- coda.samples(jags.hdl1, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)

summary(test.hdl)

# Exercise 1 Repeat the analysis of HDL using log-normal distribution
model.hdl2 <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dlnorm(mu[i],tau)
    mu[i] <- beta.0 + beta.sex*(sex[i]-1) + beta.bmi*(bmi[i]-mean.bmi) + beta.age*(age[i]-mean.age)
   }
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);
  beta.age ~ dnorm(0, 0.0001);

  mean.bmi <- mean(bmi[])  
  mean.age <- mean(age[])
   }"

# Formatted data for JAGS
hdl.dataf <- list(N = as.numeric(nrow(hdl.data)),
                  hdl = hdl.data$HDL5,
                  bmi = hdl.data$BMI5,
                  sex = hdl.data$SEX,
                  age= hdl.data$AGE5)

jags.hdl2 <- jags.model(textConnection(model.hdl2), data=hdl.dataf, n.adapt=1500)
update(jags.hdl2, n.iter=1000)
test.hdl2 <- coda.samples(jags.hdl2, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)
summary(test.hdl2)

# Part 2 is BMI associated with HDL after adjusting for sex
model.hdl3 <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dlnorm(mu[i],tau)
    mu[i] <- beta.0 + beta.sex*(sex[i]-1) + beta.bmi*(bmi[i]-mean.bmi)
   }
   
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);

  mean.bmi <- mean(bmi[])  
   }"

# Formatted data for JAGS
hdl.dataf <- list(N = as.numeric(nrow(hdl.data)),
                  hdl = hdl.data$HDL5,
                  bmi = hdl.data$BMI5,
                  sex = hdl.data$SEX)

jags.hdl3 <- jags.model(textConnection(model.hdl3), data=hdl.dataf, n.adapt=1500)
update(jags.hdl3, n.iter=1000)
test.hdl3 <- coda.samples(jags.hdl3, c('beta.0','beta.sex','beta.bmi'),  n.iter=1000)
summary(test.hdl3)

# Part 3 Diagnostic plots for above
model.hdl4 <- "
model  {  
## likelihood 
   for (i in 1:N)  {  
    hdl[i] ~ dlnorm(mu[i],tau)
    mu[i] <- beta.0 + beta.sex*(sex[i]-1) + beta.bmi*(bmi[i]-mean.bmi)
    ## Residuals
    r[i] <- hdl[i]-mu[i]
    sr[i] <- r[i]*sqrt(tau)
   }
   
 ##prior
  tau ~ dgamma(1,1);
  beta.0 ~ dnorm(0, 0.0001);
  beta.sex ~  dnorm(0, 0.0001);
  beta.bmi ~ dnorm(0, 0.0001);

  mean.bmi <- mean(bmi[])  
   }"

# Formatted data for JAGS
jags.hdl4 <- jags.model(textConnection(model.hdl4), data=hdl.dataf, n.adapt=1500)
update(jags.hdl4, n.iter=1000)
test.hdl4.res <-  coda.samples(jags.hdl4, c('r','sr'),  n.iter=1000)
out <- as.matrix(test.hdl4.res) 
hist(apply(out[,1:hdl.dataf$N],2,mean))
hist(apply(out[,3468:ncol(out)],2,mean))
qplot(hdl.data[,1], apply(out[, 3468:ncol(out)],2,median), xlab = "Observed Values, hdl[i]", ylab = "Standardized Residuals, Sr[i]")+theme_bw()
qplot(hdl.data[,5], apply(out[, 3468:ncol(out)],2,median),  xlab = "Observed Values, BMI[i]", ylab = "Standardized Residuals, Sr[i]")+theme_bw()+stat_smooth(method="loess",se=FALSE)
