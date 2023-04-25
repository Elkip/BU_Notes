######soh########################################################################                                                        
###   Boston University - Biostatistics Department                                                                                         
###   PROGRAM NAME          : script - Lecture 11.R                                                                                
###                           (.\BS853\Class 11)                                        
###   PROJECT NAME          : Generalized Linear Models (BS853)   
###                           - Lecture: General Estimating Equations for correlated data                                                                        
###                                                                                                                                        
###   DESCRIPTION           : 1. Read data                                                                                                 
###                           2. GEE                                                      
###                                                                                                                                        
###   SOFTWARE/VERSION#     : R                                                                                              
###   INFRASTRUCTURE        : Mac OS                                                                                            
###   INPUT                 :                                                                                                              
###   OUTPUT                :                                                                                                              
###  -----------------------------------------------------------------------------                                                         
###   Author : Gheorghe Doros                                                                                                              
###   Last modified                                                                                                           
######eoh########################################################################                                                         
                                                       
setwd('/Users/doros/Documents/BS853/Class 11')
library(MASS)
library(contrast)
library(VGAM)
library(geepack)
source('QIC.pois.geese.R') # Try this for QIC function
                


#######################################################
##              Car insurance claims                 ##
#######################################################
seizures <- read.csv('seizures.csv');
names(seizures)
seizures$TIMEc<-seizures$TIME 
seizures$TIME <- factor(seizures$TIME)
seizures$TREATMENT <- factor(seizures$TREATMENT)
seizures$logresponse <- log(seizures$RESPONSE + 1);
seizures$logbase <- log(seizures$BASELINE + 1);

seizures <- seizures[order(seizures$ID, seizures$TIME),];

options(contrasts = c("contr.SAS", "contr.poly")) 

###################################
#### Naive analysis **##
###################################
cat('\n Naive analyses treating time as categorical \n');

M0 <- glm(RESPONSE ~ TIME*TREATMENT + BASELINE + AGE, 
           data = seizures, family = quasipoisson())


cat( '\n Naive analyses treating time as continuous \n');
M1 <- glm(RESPONSE ~ TIMEc*TREATMENT + BASELINE + AGE, 
           data = seizures, family = quasipoisson())



################################################
#### GEE #####
################################################

#### Estimating the corr structure **##

wideS <- reshape(seizures[c('ID','TIME','logresponse')], timevar = "TIME", idvar = "ID", direction = "wide")

cat(' \n Estimated correlation \n');

cor(wideS[,c(2:5)])



options ls=97 ps=60 pageno=1 nodate;
cat('\n A GEE model with a time dependent treatment effect \n  Exchangeable correlation structure \n');
fm <- geese(RESPONSE ~ TIME*TREATMENT + BASELINE + AGE, id = ID,
           data = seizures, family = poisson, corstr = "exch")
summary(fm)

####  Model Selection using QIC ##
# Not available in package gee

###### Final Model ######
cat( '\n A GEE model with a time independent treatment effect\n Exchangeable correlation structure \n');
final <- geese(RESPONSE ~ TIME + TREATMENT + BASELINE + AGE, id = ID,
           data = seizures, family = poisson, corstr = "exch")
summary(final)
  

cat( '\n A GEE model with a time independent treatment effect and LOG baseline \n Exchangeable correlation structure \n');
final2 <- geese(RESPONSE ~ TIME + TREATMENT + logbase + AGE, id = ID,
           data = seizures, family = poisson, corstr = "exch")
summary(final2)
 