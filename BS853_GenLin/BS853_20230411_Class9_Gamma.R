######soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 9.R                                                                                
###                           (.\BS853\Class 9)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)   
###                           - Lecture: Gamma Regression                                                                        
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. Gamma Regression                                                      
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                            
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified 04/02/2021                                                                                                            
######eoh########################################################################                                                         
                                                       
setwd('/Users/doros/Documents/BS853/Class 9')
library(Design)
library(MASS)
library(contrast)
library(VGAM)
                

##########################################################
#### Generate Gamma density ####
##########################################################

nu <- c(0.5, 1, 2, 10);
s <- nu/7
 x<-seq(0.1,15, by=0.1);
f1<-dgamma(x,shape=nu[1],rate=s[1]) 
f2<-dgamma(x,shape=nu[2],rate=s[2]) 
f3<-dgamma(x,shape=nu[3],rate=s[3]) 

plot(x,f1,ylim=c(0,max(c(f1,f2,f3))),xlab='x',ylab='Gamma Density',type='n')
lines(x,f1,lty=1,col=1);lines(x,f2,lty=2,col=2);lines(x,f3,lty=3,col=3)

#######################################################
##              Car insurance claims                 ##
#######################################################
claims <- read.csv('claims.csv');
claims$cost<-claims$cost+1;
claims$lcost <- log(claims$cost+1)
claims$ageofcarc <- claims$ageofcar # continuous age of the car


cat('\n Coefficient of variation \n');
cv <- function(x) c(Mean=mean(x,na.rm=TRUE),CV=sd(x,na.rm=TRUE)/mean(x,na.rm=TRUE)*100)
ll<-lapply(claims[c('policyholderage','cargroup','ageofcar')],function(x) do.call('rbind',tapply(claims$cost,x,cv)));


###################################################################
#### Model Selection - Inverse Link                            ####
###################################################################

claims$policyholderage <- factor(claims$policyholderage) 
claims$cargroup <- factor(claims$cargroup) 
claims$ageofcar <- factor(claims$ageofcar) 
 
cat('\n Gamma regression - Saturated model \n\n Inverse Link');
M<-glmD(cost~policyholderage*cargroup*ageofcar, data=claims, weights=number, family=Gamma(link='inverse'))	

cat('\n Gamma regression -  Intercept only model \n\n Inverse Link');
M0<-glmD(cost~1, data=claims, weights=number, family=Gamma(link='inverse'))


cat('\n Gamma regression -  Main effects model \n\n Inverse Link');
M1<-glmD(cost~policyholderage + cargroup + ageofcar, data=claims, weights=number, family=Gamma(link='inverse'))


cat('\n Gamma regression -  all two way interactions \n\n Inverse Link');
M2<-glmD(cost~(policyholderage + cargroup + ageofcar)^2, data=claims, weights=number, family=Gamma(link='inverse'))

cat('\n Gamma regression -  Joint independence of policyholder and cargroup from ageofcar \n\n Inverse Link');
M3<-glmD(cost~policyholderage*cargroup + ageofcar, data=claims, weights=number, family=Gamma(link='inverse'))


cat('\n Gamma regression -   Quadratic Age of car effect\n\n Inverse Link');
M4<-glm(cost~policyholderage*cargroup + ageofcarc+I(ageofcarc*ageofcarc), data=claims, weights=number, family=Gamma(link='inverse'))
Pg1<-M4$fitted.values
###################################################################
#### Model Selection - Log Link                            ####
###################################################################

	

cat('\n Gamma regression -  Intercept only model \n\n Log Link');
M0<-glmD(cost~1, data=claims, weights=number, family=Gamma(link='log'))


cat('\n Gamma regression -  Main effects model \n\n Log Link');
M1<-glmD(cost~policyholderage + cargroup + ageofcar, data=claims, weights=number, family=Gamma(link='log'))


cat('\n Gamma regression -  all two way interactions \n\n Log Link');
M2<-glmD(cost~(policyholderage + cargroup + ageofcar)^2, data=claims, weights=number, family=Gamma(link='log'))

cat('\n Gamma regression -  Joint independence of policyholder and cargroup from ageofcar \n\n Log Link');
M3<-glmD(cost~policyholderage*cargroup + ageofcar, data=claims, weights=number, family=Gamma(link='log'))


cat('\n Gamma regression -   Quadratic Age of car effect\n\n Log Link');
M4<-glm(cost~policyholderage*cargroup + ageofcarc+I(ageofcarc*ageofcarc), data=claims, weights=number, family=Gamma(link='log'))
Pg2<-M4$fitted.values

###################################################################
#### Model Selection - Log normal                           ####
###################################################################

	

cat('\n Log-Normal regression -  Intercept only model \n');
M0<-glmD(lcost~1, data=claims, weights=number)


cat('\n Log-Normal regression -  Main effects model \n');
M1<-glmD(lcost~policyholderage + cargroup + ageofcar, data=claims, weights=number,)


cat('\n Log-Normal regression -  all two way interactions \n');
M2<-glmD(lcost~(policyholderage + cargroup + ageofcar)^2, data=claims, weights=number)

cat('\n Log-Normal regression -  Joint independence of policyholder and cargroup from ageofcar \n');
M3<-glmD(lcost~policyholderage*cargroup + ageofcar, data=claims, weights=number, family=Gamma(link='log'))


cat('\n Log-Normal regression -   Quadratic Age of car effect\n');
M4<-glm(lcost~policyholderage*cargroup + ageofcarc+I(ageofcarc*ageofcarc), data=claims, weights=number);
Pr<-exp(M4$fitted.values)-1;

### Predicted vs. Observed plot
plot(claims$cost,Pr,xlab='Observed', ylab='Predicted')
points(claims$cost,Pg1,pch=19,col='red')
points(claims$cost,Pg2,pch=17,col='blue')
legend('topleft',pch=c(1,19,17),col=c('black','red','blue'),legend=c('Log-Normal Model','Gamma - Inverse Link','Gamma - Log Link'),bty='n')
