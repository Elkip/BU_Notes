# #soh########################################################################
#   Boston University - Biostatistics Department
#   PROGRAM NAME          : script - Lecture 3.R
#                          (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 3)
#   PROJECT NAME          : Generalized Linear Models (BS853) 
#                           - Lecture: LogLinear Models in SxR and three way tables
#
#   DESCRIPTION           : 1. Read data
#                           2. Loglinear Models for SxR tables and sets of SxR tables
#                                                        
#   SOFTWARE/VERSION#     : R/Version 2.12.2                                                                                              
#   INFRASTRUCTURE        : Mac OS          
#   INPUT                 : 
#   OUTPUT                : 
#  -----------------------------------------------------------------------------
#   Author : Gheorghe Doros 
#   Last modified 01/27/2019               
# ##eoh########################################################################/  
setwd('/Users/Doros/Documents/Class 3') 
library(MASS)
#Read the data from a '.csv' file using read.csv() function 

####################  
DataC<-read.csv('DataC.csv')
DataC$clinic<-factor(DataC$clinic)
# Frequencies specified as an array.
l<-sapply(DataC, function(x) length(levels(x)))
###lessca  death clinic  count 
###     2      2      2      0
Tc <- array(0, l[-4], lapply(DataC[, -4], levels))
Tc[data.matrix(DataC[,-4])] <- DataC$count 
ftable(Tc) 
mantelhaen.test(Tc,correct=FALSE)



##########################################
### Clinic, Care and Infant mortatlity ###
##########################################

cat('\n\n\nClinic, Care and Infant mortatlity\n\n\n')
infcare <- read.csv('infcare.csv');names(infcare)
infcare$Clinic<-factor(infcare$Clinic)
infcare$Care<-factor(infcare$Care)
infcare$Death <-factor(infcare$Death)

### Construct the table

# Frequencies specified as an array.
l<-sapply(infcare, function(x) length(levels(x)))
#Clinic   Care  Death  Count 
#     2      2      2      0 
T <- array(0, l[-4], lapply(infcare[, -4], levels))
T[data.matrix(infcare[,-4])] <- infcare$Count
ftable(T)

cat('\n Model 1 saturated model\n');

Ms<-loglm(~1:2:3,T) # Saturated Model

cat('\n Model 2 - all two way interactions\n');

M2i <- loglm(~1:2+1:3+2:3,T)

cat('\nModel 3 - conditional independence of Care and Clinic\n');
Mci1<-loglm(~1:3+2:3,T)

cat('\nModel 4 - conditional independence of Survival and Clinic\n');
Mci2<-loglm(~1:2+2:3,T) 

cat('\nModel 5 - conditional independence of Survival and Care\n');
Mci3<-loglm(~1:2+1:3,T) 

cat('\nModel 6 - joint independence of (Care and Survival) from Clinic\n');
Mji1<-loglm(~1+2:3,T) 

cat('\nModel 7 - Mutual independence of Care, Clinic, and Survival\n');
Mmi1<-loglm(~1+2+3,T) 

anova(Ms,M2i,Mci1,Mji1,Mmi1) # Comparing nested models
anova(Ms,M2i,Mci2,Mji1,Mmi1)
anova(Ms,M2i,Mci3,Mmi1)


###################################################################################################
# Association among residence, income and satisfaction with total hip replacement (THR)           #
###################################################################################################

cat('\nAssociation among residence, income and satisfaction with total hip replacement (THR)\n');
THR<-read.csv('THR.csv')
#Make variables factors
THR$Rural <-factor(THR$Rural)
THR$Income <-factor(THR$Income)
THR$Satisf <-factor(THR$Satisf)
### Construct the table

# Frequencies specified as an array.
l<-sapply(THR, function(x) length(levels(x)))
# Rural Income Satisf  Count 
#     2      2      2      0 
T <- array(0, l[-4], lapply(THR[, -4], levels))
T[data.matrix(THR[,-4])] <- THR$Count
ftable(T)


cat('\nModel 1 saturated model\n')
Ms<-loglm(~1:2:3,T)
cat('\nModel 2 - all two way interactions\n')
M2i<-loglm(~1:2+1:3+2:3,T)

cat('\nModel 3 - conditional independence of Income and Rural\n')
Mci1<-loglm(~1:3+2:3,T)

cat('\nModel 4 - conditional independence of Satisfaction and Rural\n')
Mci2<-loglm(~1:2+2:3,T)

cat('\nModel 5 - conditional independence of Satisfaction and Income\n')
Mci3<-loglm(~1:2+1:3,T)

cat('\nModel 6 - joint independence of (Income and Satisfaction) from Rural\n')
Mji1<-loglm(~1+2:3,T)

cat('\nModel 7 - joint independence of (Income and Rural) from Satisfaction \n')
Mji2<-loglm(~1:2+3,T)

cat('\nModel 8 - joint independence of (Rural and Satisfaction) from Income \n')
Mji3<-loglm(~1:3+2,T)

cat('\nModel 9 - Mutual independence of Income, Rural, and Satisfaction\n')
Mmi1<-loglm(~1+2+3,T)

#Compare nested models
anova(Ms,M2i,Mci1,Mji1,Mmi1)
anova(Ms,M2i,Mci1,Mji3,Mmi1)
anova(Ms,M2i,Mci2,Mji1,Mmi1)
anova(Ms,M2i,Mci2,Mji2,Mmi1)
anova(Ms,M2i,Mci3,Mji2,Mmi1)
anova(Ms,M2i,Mci3,Mji3,Mmi1)