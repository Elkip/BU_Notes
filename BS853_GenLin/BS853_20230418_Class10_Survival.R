######soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 10.R                                                                                
###                           (.\BS853\Class 10)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)   
###                           - Lecture: Multinomial logit Models for ordinal data                                                                        
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. Time to event data analysis                                                    
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                            
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified 04/12/2021                                                                                                            
######eoh########################################################################                                                         
                                                       
setwd('/Users/doros/Documents/BS853/Class 10')
library(MASS)
library(contrast)
library(VGAM)
                                                
#Comparing survival using CMH test and 3 time strata#
RCMH<-read.csv('RCMH.csv'); 
RCMH$group<-factor(RCMH$group)
RCMH$time<-factor(RCMH$time)
RCMH$event<-factor(RCMH$event,levels=c('yes','no'))
# Frequencies specified as an array.
l<-sapply(RCMH, function(x) length(levels(x)))
###group event  time count 
###    2     2     3     0  
Tc <- array(0, l[-4], lapply(RCMH[, -4], levels))
Tc[data.matrix(RCMH[,-4])] <- RCMH$count 
ftable(Tc) 
  #Comparing survival using CMH test and 3 time strata#
cat('\nComparing survival using CMH test and 3 time strata\n\n');
mantelhaen.test(Tc,correct=FALSE)
  


#Comparing Survival with Log-Rank test #
ratc <- read.csv('ratc.csv')

cat('\n Testing the difference Using Log-Rank test\n'); 
survdiff(Surv(day, 1-censor) ~ group,data=ratc)


### Angina Pectoris in Framingham study ###
anginaf <- read.csv('anginaf.csv');names(anginaf);table(anginaf$sex)

# Create individual data
 anginaf$index<-1:nrow(anginaf)
 anginaf<-anginaf[anginaf$freq>0,] # eliminate 0 frequency
 M<-matrix(0,nrow=sum(anginaf$freq),ncol=nrow(anginaf)) # Initiate a matrix to multiply data
 cFreq<-c(0,cumsum(anginaf$freq))   # Construct frequency of each observation
 for (i in 1:(length(cFreq)-1)) {  # modify the Matrix
 	M[(cFreq[i]+1):cFreq[i+1],i]<-1
 	}
 Ind.anginaf<-data.frame(M%*%data.matrix(anginaf));dim(Ind.anginaf)  # get individual data
table(Ind.anginaf$index); table(Ind.anginaf$sex)


 cat('\n Testing the difference Using Log-Rank test\n'); 
 survdiff(Surv(years, censor) ~ sex,data=Ind.anginaf);table(Ind.anginaf$sex)

fit<-survfit(Surv(years, censor) ~ sex,data=Ind.anginaf);
summary(fit)
cat('\n Angina Pectoris in Framingham study \n');
   postscript("survm.eps", width = 9, height = 7, pointsize = 12)
par(mar=c(10,8,5,5))
a=plot(fit, mark.time=TRUE, mark=c(16),
        xlab='Years', ylab='Survival, %',lty=c(1),lwd=c(2),cex=.1,col=c(1),axes=FALSE,conf.int=FALSE)
axis(1)
axis(2, at=seq(0, 1, .2), labels=paste(seq(0, 100, 20),'%',sep=''),las=1)
dev.off()
#Relapse times after treatment for acute leukemia from Gehan (Biometrika 1965)#
### relapse times in weeks (tweeks); 
### treatment group (1=placebo, 2=6-mercaptopurine (6-mp)); 
### censoring variable (1=censored obsn, 0=uncensored); 
### negative treatment times indicate censored observations; 

leuktrt <- read.csv('leuktrt.csv'); 
leuktrt$logwk<-log(leuktrt$tweeks)
leuktrt$group<-factor(leuktrt$group)

fit<-survfit(Surv(tweeks, 1-censored) ~ group,data=leuktrt);
cat('\n Estimate and Compare time to relapse curves using Log-Rank test \n');
 survdiff(Surv(tweeks, 1-censored) ~ group,data=leuktrt)
 summary(fit)

postscript("LeukTRT.eps", width = 9, height = 7, pointsize = 12)
par(mar=c(10,8,5,5))
a=plot(fit, mark.time=TRUE, mark=c(16),
        xlab='Weeks', ylab='Survival, %',lty=c(1),lwd=c(2),cex=.1,col=c(1),axes=FALSE,conf.int=FALSE)
axis(1)
axis(2, at=seq(0, 1, .2), labels=paste(seq(0, 100, 20),'%',sep=''),las=1)
dev.off()



cat('\n Compare time to relapse by assuming an Exponential distribution - psm() function \n');

psm(Surv(tweeks,1-censored) ~ group, dist = 'exponential',data=leuktrt) ;


glm(relapse~group,family=poisson(), data=leuktrt,offset=logwk); 
  
### recidivism in 432 male prisoners in the year following their release ###
prison <- read.csv('prison.csv')


cat('\n Predict the hazard of reicarceration with proportional hazard regression \n');
coxph(Surv(week, arrest)~mar + paro + race + fin + age + prio + educ, data=prison);

