# Lecture 5: Capture Recapture methods
# Code using the log linear model

#############################
# Example 1: Simulated Data #
#############################
# create the data with indicator variables
sim1 <- as.data.frame(cbind(c(1,0,1),c(0,1,1),c(110,210,40)))
names(sim1) <- c("group1","group2","count")

# fit the log linear model
fit1 <- glm(count ~ group1 + group2,data=sim1,family=poisson)

# look at the parameter estimates and other summaries of the model fit
summary(fit1)

# get the components of the model that we can ask for
names(fit1)

# estimate of the number of cases not captured by either
# this is the exponential of the intercept
# coefficients is a piece of the model fit and is a vector. 
# The intercept is the first component of coefficients. beta_1 is the second, etc.
exp(fit1$coefficients[1])

###################################
# Example 2: HIV data from France #
###################################
# create the data
hiv <- as.data.frame(cbind(c(1,0,1),c(0,1,1),c(10288,1835,9337)))
names(hiv) <- c("DO","FHDH","count")

# fit the log linear model
fit.hiv <- glm(count ~ DO + FHDH,data=hiv,family=poisson)

summary(fit.hiv)

names(fit.hiv)
# estimate of uncaptured
exp(fit.hiv$coefficients[1])

# estimate of total population
tot.pop.hiv <- exp(fit.hiv$coefficients[1])+sum(hiv$count)

############################
# Example 3: Four sources  #
#   From Bruno et al(1994) #
############################
s1 <- c(1,0,0,0,1,1,1,0,0,0,1,1,1,0,1)
s2 <- c(0,1,0,0,1,0,0,1,1,0,1,1,0,1,1)
s3 <- c(0,0,1,0,0,1,0,1,0,1,1,0,1,1,1)
s4 <- c(0,0,0,1,0,0,1,0,1,1,0,1,1,1,1)

diab <- c(709,74,182,10,104,650,12,20,7,8,157,18,46,14,58)

dat.diab <- as.data.frame(cbind(s1,s2,s3,s4,diab))

fit.diab <- glm(diab~s1+s2+s3+s4+s1*s2+s1*s3+s2*s3+s2*s4+s3*s4,
                data=dat.diab,family=poisson)

fit.diab2 <- glm(diab~s1+s2+s3+s4+s1*s2+s1*s3+s2*s3+s2*s4+s3*s4+s1*s4,data=dat.diab,family=poisson)

exp(fit.diab$coefficients[1])

tot.diab <- exp(fit.diab$coefficients[1]) + sum(dat.diab[,5])

# What if we do not adjust for source dependence
fit.diab3 <- glm(diab~s1+s2+s3+s4,data=dat.diab,family=poisson)
tot.diab3 <- exp(fit.diab3$coefficients[1]) + sum(dat.diab[,5])

####################################
# Example 4: TB incidence in Egypt #
#  from Bassili et al (2010)       #
####################################
# Egypt example
ntb <-     c(1,1,1,1,0,0,0)
pub <-     c(1,1,0,0,0,1,1)
private <- c(1,0,1,0,1,0,1)
number <- c(1,76,40,247,41,5,0)
dat <- data.frame(ntb,pub,private,number)

fit1 <- glm(number~ntb+pub+private,data=dat,family="poisson")
fit2 <- glm(number ~ntb+private+pub+ntb*pub,data=dat,family="poisson")
fit3 <- glm(number ~ntb+private+pub+ntb*private,data=dat,family="poisson")
fit4 <- glm(number ~ntb+private+pub+ntb*pub+pub*private,data=dat,family="poisson")

exp(fit1$coefficients[1])+sum(dat$number)
exp(fit2$coefficients[1])+sum(dat$number)
exp(fit3$coefficients[1])+sum(dat$number)
exp(fit4$coefficients[1])+sum(dat$number)

