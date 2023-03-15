 #####soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 7.R                                                                                
###                           (/Userrs/Documents/Projects/BS853/Class 7)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
###                           - Lecture: Generalized Linear Models for Multinomial data                                                                         
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. Generalized Linear Models for Multinomial data                                                    
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R/Version 2.12.2                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                                      
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified 03/6/2021                                                                                                             
######eoh########################################################################                                                         
setwd('/Users/doros/Documents/BS853/Class 6')
library(contrast)
library(multcomp)
library(VGAM)


### Contraceptive data 1=Sterilization,2=Other, 3=None###
contra <- read.csv('contraceptive.csv'); 

contra$cage=contra$age-4;
contra$agesq=contra$cage^2;
contra$elsn=log((contra$count/contra$total)/(contra$ref/contra$total));

setEPS(paper='special')
postscript('contraceptive.eps',width=7,height=5)  
plot(contra$age[contra$method %in% c(1,2)], contra$elsn[contra$method %in% c(1,2)],xlab='AGE',ylab='Log Generalized Logit (None as reference)',type='n'); 
lines(contra$age[contra$method %in% c(1)], contra$elsn[contra$method %in% c(1)],cex=1.4,type='b'); 
lines(contra$age[contra$method %in% c(2)], contra$elsn[contra$method %in% c(2)],cex=1.4,type='b',pch=19); 
legend('bottomright',legend=c('Sterilization', 'Other'), title='Method',pch=c(1,19),pt.cex=1.4)
dev.off()



contra$method <- factor(contra$method)
contra$age<- factor(contra$age,levels=c(7,1:6))



###Model 1###
cat('\n Contraceptive data \n Using MLogit - AGE categorical \n');

M<-vglm(method ~ age, multinomial, weight=count, contra)
summary(M)


###Using LogLinear models###
cat('\n Contraceptive data \n Using Loglinear models - AGE categorical';

M2<- glm(count ~ method*age, family=poisson(), data=contra);


###Model 2 ###
cat('\n Contraceptive data \n Using MLogit - - continuous age linear effect \n');
M<-vglm(method~cage,data=contra,multinomial,weights=count)
summary(M)


###Model 3###
cat('\n Contraceptive data \n Using MLogit - continuous age linear and quadratic effects\n');
M<-vglm(method~cage+agesq,data=contra,multinomial,weights=count)
summary(M)


###Using LogLinear Models###
cat('\n Contraceptive data \n Using LogLinear models - continuous age linear and quadratic effects\n');
M2<- glm(count ~ method+age+method:cage + method:agesq, family=poisson(), data=contra);




###Schoolchildren learning style preference and School program###
### styles 1=Self,2=Team,3=Class###
school <- read.csv('school.csv')
school$style <- as.factor(school$style)
school$school <- as.factor(school$school)
###Model 1###
cat('\n Schoolchildren learning style preference and School program \n');
M<-vglm(style~school+program,data=school,multinomial,weights=count)
summary(M)


#contrast 'reg vs. after' program -1 1/estimate=exp;



###Consider data on association of cumulative incidence of 
### pneumoconiosis and years working at a coalmine.###

ph <- read.csv('ph.csv')
ph$age<-factor(ph$age)


cat('\n Cumulative Logit models \n');
cum.logit<- lrm(type ~ age, data=ph, na.action=na.pass,weight=count)
print(cum.logit)
#      estimate 'LogOR12' age -1 1 / exp;      


### Metal Impairement data - Agresti, 1990###

mimpair <- read.csv("impair.csv")
mimpair$SES <-factor(mimpair$SES )
cat('\n Cumulative Logit models\n');
cum.logit<- lrm(Level ~ SES+Events, data=mimpair, na.action=na.pass)
print(cum.logit)
     


###satisfaction with THR and hospital volume and year of surgery.###
# vol 1='High' 0='Low';  

thr <- read.csv('THR.csv'); 
thr$highvol <-  factor(thr$highvol)
thr$year <-  factor(thr$year)

thr$cuml1 <- ifelse(thr$satisf==1,log(thr$count/(thr$total123-thr$count)),NA)
thr$cuml2 <- ifelse(thr$satisf==2 ,log(thr$total12/(thr$total123-thr$total12)),NA);

### Only main effects model ###
cat('\nOnly main effects model \n');
cum.logit<- lrm(satisf~highvol+year, data=thr, na.action=na.pass,weight=count)
print(cum.logit)


### Only volume main effect in the model ###
cat('\nOnly volume effect model \n');
cum.logit<- lrm(satisf~highvol+year, data=thr, na.action=na.pass,weight=count)
print(cum.logit)

#output out=bb(where=(_level_ in (1,2) and satisf in (1,2) and satisf=_level_)) xbeta=pred;

