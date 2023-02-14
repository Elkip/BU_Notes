# ###soh########################################################################                                                        
#   Boston University - Biostatistics Department                                                                                         
#   PROGRAM NAME          : script - Lecture 4.R                                                                               
#                           (/Users/doros/Documents/Class 4)                                        
#   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
#                           - Lecture: Generalized Linear Models                                                                         
#                                                                                                                                        
#   DESCRIPTION           : 1. Read data                                                                                                 
#                           2. Logistic regression models                                                    
#                                                                                                                                        
#   SOFTWARE/VERSION#     : R/Version 2.12.2                                                                                              
#   INFRASTRUCTURE        : Mac OS                                                                                                      
#   INPUT                 :                                                                                                              
#   OUTPUT                :                                                                                                              
#  -----------------------------------------------------------------------------                                                         
#   Author : Gheorghe Doros                                                                                                              
#   Last modified 02/10/2019                                                                                                             
# ##eoh##########################################################################                                                         
                                                
setwd('/Users/doros/Documents/Class 4')
library(MASS)
library(contrast)

CHD <- read.csv('CHD.csv')
CHD$P<-(CHD$CHD+0.5)/CHD$Total
CHD$LogODDS<-log(CHD$P/(1-CHD$P))
CHD$Chol1<-factor(CHD$Chol,levels=c('<200','200 - 219','220 - 259','>=260')) # order the levels

lch<-c('<200','200 - 219','220 - 259','>=260')
tiff('EDCS.tif')
plot(as.numeric(CHD$SBP),CHD$LogODDS,axes=FALSE,ylab='Log ODDS of CHD',xlab=' SBP Levels ',type='n')
axis(1,at=1:length(levels(CHD$SBP)),labels=levels(CHD$SBP))
axis(2)

for (l in 1:length(lch))
{lines(as.numeric(CHD$SBP)[CHD$Chol1==lch[l]],CHD$LogODDS[CHD$Chol1==lch[l]],col=l,lwd=2,lty=l,type='b')}
legend('topleft',lty=1:4,col=1:4,pch=1,legend=lch,bty='n',title='Cholesterol Levels',lwd=2,seg.len=3)
dev.off()

### Fit different Models using glm ###
cat('\n Only Intercept \n');
M1 <- glm(cbind(CHD,Total-CHD)~1 , data=CHD,family=binomial())


cat('\n Only Chol Effect \n');
M2 <- glm(cbind(CHD,Total-CHD)~Chol1 , data=CHD,family=binomial())

cat('\n Only SBP Effect \n');
M3 <- glm(cbind(CHD,Total-CHD)~SBP , data=CHD,family=binomial())


cat('\n Only main Effects \n');
M4 <- glm(cbind(CHD,Total-CHD)~ Chol1 + SBP , data=CHD,family=binomial())


cat('\n Only main Effects - Continuous predictors \n');
M5 <- glm(cbind(CHD,Total-CHD)~ unclass(Chol1) + unclass(SBP) , data=CHD,family=binomial())


cat('\n Saturated Model \n');
M6<- glm(cbind(CHD,Total-CHD)~ Chol1 * SBP , data=CHD,family=binomial())

anova(M6,M4,M5,M3,M1)
anova(M6,M4,M5,M2,M1)

CHD2<-read.csv('CHD2.csv')
CHD2$chol <- factor(CHD2$chol)
CHD2$sbp  <- factor(CHD2$sbp)
CHD2$chd <- factor(CHD2$chd)

# Frequencies specified as an array.
l <- sapply(CHD2, function(x) length(levels(x)))

### chol   sbp   chd count 
###    4     4     2     0 
T <- array(0, l[-4], lapply(CHD2[, -4], levels))
T[data.matrix(CHD2[,-4])] <- CHD2$count  
library(MASS)


cat('\n Logistic regression models as Loglinear models \n');
cat('\n Model 1 \n');
LM1<-glm(count ~ chd + sbp*chol, family=poisson(),data=CHD2);LM11 <- loglm(~chd+sbp*chol,T)

cat('\n Model 2 \n');
LM2<-glm(count ~ chd*sbp + sbp*chol, family=poisson(),data=CHD2);


cat('\n Model 3 \n');
LM3<-glm(count ~ chd*chol + sbp*chol, family=poisson(),data=CHD2);


cat('\n Model 4 \n');
LM1<-glm(count ~ chd*chol + chd*sbp + sbp*chol, family=poisson(),data=CHD2);



##################################################################
### Admission Data                                             ###
##################################################################
### Ignoring Department ###

overall<-read.csv('AdmissionPulled.csv');names(overall)

### Saturated model: with class statement###;  
cat('\n Differential admission by Gender \n');
Mp<- glm(cbind(yes,total-yes)~ sex , data=overall,family=binomial())

a<-contrast(Mp,list(sex='F'),list(sex='M')) #Gender (F-M) 
print(a,X=TRUE)


### By Department ###

one <- read.csv('Admission.csv');names(one)

lch<-c('F','M')
tiff('Admit.tif',width=5,height=4,units='in',res=100)
plot(one$Department,one$logitp,axes=FALSE,ylab='Log Rate of Admission',xlab=' Department ',type='n')
axis(1,at=1:length(unique(one$Department)),labels=unique(one$Department))
axis(2)

for (l in 1:length(lch))
{lines(one$Department[one$Sex==lch[l]],one$logitp[one$Sex==lch[l]],col=l,lwd=2,lty=l,type='b')}
legend('bottomleft',lty=1:2,col=1:2,pch=1,legend=lch,bty='n',title='Gender',lwd=2,seg.len=3)
dev.off()

one$Department=factor(one$Department)
cat('\n Saturated Model \n');
M1<-glm(cbind(yes,no) ~ Department*Sex , data=one,family=binomial())


cat('\n Only main effects Model \n');
M2<-glm(cbind(yes,no) ~ Department + Sex , data=one,family=binomial())


cat('\n Only Sex Model \n');
M3<-glm(cbind(yes,no) ~ Sex , data=one,family=binomial())



cat('\n Only Department effects Model \n');
M3<-glm(cbind(yes,no) ~ Department , data=one,family=binomial())

contrast(M1,list(Department='1',Sex='F'),list(Department='1',Sex='M')) # Gender (F-M) in Department 1
contrast(M1,list(Department='2',Sex='F'),list(Department='2',Sex='M')) # Gender (F-M) in Department 2
contrast(M1,list(Department='3',Sex='F'),list(Department='3',Sex='M')) # Gender (F-M) in Department 3
contrast(M1,list(Department='4',Sex='F'),list(Department='4',Sex='M')) # Gender (F-M) in Department 4
contrast(M1,list(Department='5',Sex='F'),list(Department='5',Sex='M')) # Gender (F-M) in Department 5
contrast(M1,list(Department='6',Sex='F'),list(Department='6',Sex='M')) # Gender (F-M) in Department 6

############################################  Modeling Trends in Proportions ################################################
refcanc<-read.csv('refcanc.csv');names(refcanc)

### only intercept ###
cat('\n Only Intercept model \n');
M1<-glm(cbind(cases,ref) ~ 1 , data=refcanc, family=binomial())



### Saturated model ###
cat('\n Saturated model \n');
M2<-glm(cbind(cases,ref) ~ factor(age) , data=refcanc, family=binomial())


cat('\n Linear trend model \n');
M3<-glm(cbind(cases,ref) ~ age , data=refcanc, family=binomial())

anova(M2,M3,M1)