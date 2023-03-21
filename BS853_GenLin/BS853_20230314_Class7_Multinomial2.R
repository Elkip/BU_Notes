######soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 87.R                                                                                
###                           (.\BS853\Class 7)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)   
###                           - Lecture: Multinomial logit Models for ordinal data                                                                        
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. Multinomial logit Models                                                    
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                            
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified 03/08/2021                                                                                                            
######eoh########################################################################                                                         
                                                       
setwd('/Users/doros/Documents/BS853/Class 7')
library(contrast)
library(VGAM)
                                                


### Satisfaction with THR and hospital volume and year of surgery ###
### 1='High' 0='Low';  

thr <- read.csv('THR.csv'); 
thr$highvol <-  factor(thr$highvol)
thr$year <-  factor(thr$year)

### Only main effects model ###
cat('\nOnly main effects model \n');

M<-vglm(satisf~highvol+year,data=thr,cumulative(parallel=TRUE, reverse=TRUE),weights=count) # Cumulative Proportional odds assumed; options 'parallel = TRUE'
summary(M)
M2<-vglm(satisf~highvol+year,data=thr,cumulative(parallel=FALSE, reverse=TRUE),weights=count) # Cumulative Proportional odds not assumed; options 'parallel = FALSE'
summary(M2)

# Test for cumulative proportional ODDS by comparing the two goodness of fit statistics
pchisq(deviance(M)-deviance(M2), df=df.residual(M)-df.residual(M2),lower.tail=FALSE)

cat('\n Adjacent Logits models for THR \n');
M<-vglm(satisf~highvol+year,data=thr,acat(parallel=TRUE, reverse=TRUE),weights=count) # Adjacent Proportional odds assumed; options 'parallel = TRUE'
summary(M)
M2<-vglm(satisf~highvol+year,data=thr,acat(parallel=FALSE, reverse=TRUE),weights=count) # Adjacent Proportional odds not assumed; options 'parallel = FALSE'
summary(M2)

# Test for Adjacent proportional ODDS by comparing the two goodness of fit statistics
pchisq(deviance(M)-deviance(M2), df=df.residual(M)-df.residual(M2),lower.tail=FALSE)


cat('\n Adjacent Logits models for THR - with coefficents dependent on the level of satisfcation\n')
M3<-vglm(satisf~highvol+year,data=thr,acat(parallel=FALSE~year, reverse=TRUE),weights=count) # Adjacent Proportional odds not assumed
summary(M3)

# Test for Adjacent proportional ODDS w.r.t volume 
pchisq(deviance(M3)-deviance(M2), df=df.residual(M3)-df.residual(M2),lower.tail=FALSE)


### number of hip fractures by race age and BMI ###
### n=0 then no hip fractures, n=1 then 1 hip fractures, n=2 then more than 1 hip fractures ###
nhf <- read.csv('NHF.csv');names(nhf)
nhf$bmi <- factor(nhf$bmi)
### continuation ratio logits models###
cat('\n Continuation-ratio Logits models for NHF \n ');
M<-vglm(n~age + bmi*race, data=nhf, cratio(parallel=TRUE, reverse=TRUE), weights=count) # Adjacent Proportional odds assumed
summary(M)
M2<-vglm(n~age + bmi*race, data=nhf, cratio(parallel=FALSE, reverse=TRUE), weights=count) # Adjacent Proportional odds not assumed; options 'parallel = FALSE'
summary(M2)
# Test for continuation ratio proportional ODDS 
pchisq(deviance(M)-deviance(M2), df=df.residual(M)-df.residual(M2),lower.tail=FALSE)


### let the coefficients depend on the number of hip fractures ###
cat('\n Coefficients dependent on the number of HF \n');
M3<-vglm(n~age + bmi+ race + bmi:race, data=nhf, cratio(parallel=FALSE~age+bmi+race, reverse=TRUE), weights=count) # Adjacent Proportional odds not assumed
                                                                                              # Varying coefficents only for the main effects of age, bmi, and race; option parallel=FALSE~age+bmi+race
                                                                                              # Constant coefficients for bmi:race
summary(M3)

####################################################################################
###                Overdispersion in binomial models                             ###
####################################################################################
aggregate <-read.csv('aggregate.csv');names(aggregate)
aggregate$no <- aggregate$total - aggregate$yes
aggregate$cultivar <- factor(aggregate$cultivar) 
aggregate$soil <- factor(aggregate$soil) 

### no scale - aggregated data'###

cat('\n No Scale \n')
M <- glm(cbind(yes,no) ~ cultivar*soil, data=aggregate, family=binomial()) 

###  scale - aggregated data'###
### Model 2 ###
cat('\n Scale parameter estimated by the scaled dispersion \n');
M2 <- glm(cbind(yes,no) ~ cultivar*soil, data=aggregate, family=quasibinomial()) 
summary(M);summary(M2)