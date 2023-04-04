###  ##soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 8.R                                                                                
###                           (.\BS853\Class 8)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
###                           - Lecture: Generalized Linear Models for count data                                                                        
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. Testing in Generalized Linear Models                                                    
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                                      
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified 03/27/2021                                                                                                            
#####eoh########################################################################/                                                         
setwd('/Users/doros/Documents/BS853/Class 8')
library(contrast)
library(pscl)

claims <- read.csv('Claims.csv');
claims$logN <- log(claims$N)
claims$agec <- claims$age                  # keep a continuois Age
claims$age <- factor(claims$age)           # change age in factor
claims$district <- factor(claims$district) # change district in factor
claims$car <- factor(claims$car)           # change car in factor

cat( '\n Claims data \n Only intercept model(1) \n')
M1 <- glm( C ~ 1, data=claims, family=poisson(), offset=logN)


cat( '\n Claims data \n Only district effect model(2) \n')
M2 <- glm( C ~ district, data=claims, family=poisson(), offset=logN)

cat( '\n Claims data \n District and Car main effects model(3) \n')
M3 <- glm( C ~ district + car, data=claims, family=poisson(), offset=logN)

cat( '\n Claims data \n All main effects model(4) \n')
M4 <- glm( C ~ district + car + age, data=claims, family=poisson(), offset=logN)
 contrast(M4, list(car=levels(claims$car),age=levels(claims$age),district='1'),list(car=levels(claims$car),age=levels(claims$age),district='4'),type='average') 
  
cat( "\n Claims data \n All main effects model(4') - Check coefficient for offset=1 \n")
M41 <- glm( C ~ district + car + age + logN, data=claims, family=poisson())


# We expect the effect of car to vary by age #
cat( '\n Claims data \n Effect of Age and Car independent of District(5) \n')
M5 <- glm( C ~ district + car * age, data=claims, family=poisson(), offset=logN)
anova(M5,M4)
 
cat( '\n Claims data \n District and age independent given Car (6) \n')
M6 <- glm( C ~ district * car + age, data=claims, family=poisson(), offset=logN)
 

cat( '\n Claims data \n All 2-way interactions(7) \n')
M7 <- glm( C ~ district * car + age * car + district * age, data=claims, family=poisson(), offset=logN)


cat( '\n Claims data \n Check normality of the Chi-Squared residuals (Model 4) \n')

 claims$phat <- predict(M4,type='response')                            # Predicted values
 claims$Residuals <- (claims$C-claims$phat)/sqrt(claims$phat)          # Pearson residuals
 hist(claims$Residuals,ylab='Density', xlab='Pearson Residuals', main='', freq=TRUE)
 qqnorm(claims$Residuals)                                               # Normal quintile plot
 shapiro.test(claims$Residuals)                                         # Test for normality

###############################################################################################################
absences <- read.csv('Absences.csv');
absences$C<-factor(absences$C)
absences$S<-factor(absences$S)
absences$A<-factor(absences$A)
absences$L<-factor(absences$L)

cat('\n Means and Variances for each cell  \n')
f<-function(x) c(N=length(x),Mean=mean(x),Var=var(x))
ll<-tapply(absences$days,list(absences$C, absences$S, absences$A, absences$L), f) 
do.call('rbind',ll)

#Model 1#
cat( '\n  Model 1 - All 4 way interaction model - Poisson Regression  \n')
M1 <- glm( days ~ C*S*A*L, family=poisson(), data=absences)  


M11<-glm.nb( days ~ C*S*A*L, data=absences)  

odTest(M11, alpha=0.05)

# Model 2 # 
cat( '\n  Model 1 - Poisson Regression with Overdispersion (1) \n')
M2 <- glm(days ~ C*S*A*L, family=quasipoisson(),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M2)$dispersion    # Get an estimate of the dispersion parameter

coef(M2)       # Coefficient estimates
confint(M2)    # Confidence Intervals



cat( '\n  Model 1 - Poisson Regression with Overdispersion (2) \n')
options(contrasts = c("contr.SAS", "contr.poly")) ;options()$contrast
source('quasi1.R')
M21 <- glm(days ~ (C + S + A + L)^4, family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M21)$dispersion  
coef(M21);
confint(M21)

# Model 1' # 
cat( '\n  Model 1 - Poisson Regression with Overdispersion (Model 1)  \n')
# Model 2' # 
cat( '\n  All 3 way interaction model - Poisson Regression with Overdispersion (Model 2)  \n')
M22 <- glm(days ~ C*S*A + C*S*L + C*A*L + S*A*L, family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M22)$dispersion  
coef(M22);
confint(M22)


# Model 3' # 
cat( '\n  All 2 way interaction model - Poisson Regression with Overdispersion (Model 3)  \n')
M23 <- glm(days ~ (C + S + A + L)^2, family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M23)$dispersion  
coef(M23);
confint(M23)

# Model 4' # 
cat( '\n  Poisson Regression with Overdispersion (Model 4)  \n')
M24 <- glm(days ~ C*A*L + C*S + A*S + L*S, family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M24)$dispersion  
coef(M24);
confint(M24)

# Model 5' # 
cat( '\n  Poisson Regression with Overdispersion (Model 5)  \n')
M25 <- glm(days ~ C*A*L + C*S*L + A*S , family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M25)$dispersion  
coef(M25);
confint(M25) 


# Model 6' # 
cat( '\n  Poisson Regression with Overdispersion (Model 6)  \n')
M26 <- glm(days ~ C*A*L + C*S*L + C*A*S , family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M26)$dispersion  
coef(M26);
confint(M26) 


# Model 7' #
cat( '\n  Poisson Regression with Overdispersion (Model 7)  \n')
M27 <- glm(days ~ C*A*L + C*S*L + L*A*S , family=quasi1(link='log',variance='cmu',const=sqrt(9.51)),data=absences)  # Nothe that the family is quasipoisson instead of poisson
summary(M27)$dispersion  
coef(M27);
confint(M27)  

###################################################################################
# Rates of initial glaucoma treatment among elderly New Jersey Medicaid enrollees
# during the 1980
###################################################################################

incidence <- read.csv('Incidence.csv');
incidence$rate <- log(incidence$Cases/incidence$Person_Years);
incidence$logT <- log(incidence$Person_Years);


# Construct plots to study the relationship of the outcomes with age #

cat( '\n Glaucoma Treatment \n')
plot(as.numeric(incidence$Age), incidence$rate,type='n',xlab='AGE', ylab='Log Incidence',axes=FALSE)
  axis(2);
  points(as.numeric(incidence$Age)[incidence$race=='White'], incidence$rate[incidence$race=='White'],pch=1,cex=2)
  points(as.numeric(incidence$Age)[incidence$race=='Black'], incidence$rate[incidence$race=='Black'],pch=19,cex=2)
  axis(1,at=1:length(levels(incidence$Age)),labels=levels(incidence$Age))
  legend('topleft',pch=c(1,19),pt.cex=2,legend=c('Caucasian','African-American'),ncol=2)

cat( '\n Glaucoma Treatment \n -only main effects model \n')
M1 <- glm(Cases ~ Age + race, family=poisson(), offset=logT, data=incidence);


cat( '\n Glaucoma Treatment \n -saturated model  \n')
M2 <- glm(Cases ~ Age * race, family=poisson(), offset=logT, data=incidence);
contrast(M2, list(Age=levels(incidence$Age),race='Black') , list(Age=levels(incidence$Age),race='White') )


cat( '\n Glaucoma Treatment \n  - Negative Binomial Regression \n')
M2 <- glm.nb(Cases ~ Age + race + offset(logT), data=incidence);  # Note in glm.nb the offset is integrated using the offset() function 


