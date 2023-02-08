library(rjags)

hdl.data <- read.table("/home/elkip/Datasets/hdl-data.txt",sep="\t",header=T)
hdl.data$BMI= hdl.data$BMI5
hdl.data <- na.omit(hdl.data)

## generate data in format for jags
hdl.jags <- list(N = nrow(new.hdl.data),
                 hdl=as.numeric(new.hdl.data[,1]),
                 age=as.numeric(new.hdl.data$AGE5),
                 gender=as.numeric(new.hdl.data$SEX),bmi=new.hdl.data$BMI)

# Exercise 0: Model the linear trend with log HDL
hdl.data$log_hdl = log(hdl.data$HDL5)

model.hdl <- "
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

hdl.dataf <- list(N = as.numeric(nrow(hdl.data)),
               hdl = hdl.data$log_hdl,
               bmi = hdl.data$BMI5,
               sex = hdl.data$SEX,
               age= hdl.data$AGE5)

jags.hdl1 <- jags.model(textConnection(model.hdl), data=hdl.dataf, n.adapt=1500)
update(jags.hdl1, n.iter=1000)
test.hdl <- coda.samples(jags.hdl1, c('beta.0','beta.age','beta.sex','beta.bmi'),  n.iter=1000)

summary(test.hdl)
