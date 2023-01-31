##soh########################################################################
#   Boston University - Biostatistics Department
#   PROGRAM NAME          : script - Lecture 2.R
#                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 2)
#   PROJECT NAME          : Generalized Linear Models (BS853) 
#                           - Lecture: Generalized Linear Models
#
#   DESCRIPTION           : 1. Read data
#                           2. Fit logLinear models for 2x2 tables
#                                                        
#   SOFTWARE/VERSION#     : R/Version 2.12.2
#   INFRASTRUCTURE        : MAC OS 
#   INPUT                 : 
#   OUTPUT                : 
#  -----------------------------------------------------------------------------
#   Author : Gheorghe Doros 
#   Last modified 01/21/2021               
##eoh########################################################################
setwd('/Users/Doros/Documents/Class 2') 
library(MASS)
#Read the data from a '.csv' file using read.csv() function 

Data <- read.csv("Lesscare.csv")

#Normal distribution for the response (CD4 count) 

 LL1 <-  glm(count ~ lessca*death, family=poisson(),data=Data)  # Object N1 contains all components of the model

  
  LL1$family # What generalized linear model do we fit
   #Family: poisson 
   #Link function: log  

  LL1$contrasts  # Treatment constrats - as in SAS
  S1<- summary(LL1) # Summary of the output
  
  S1$coefficients  # Get the coefficients  - Note they are different from SAS as sas uses the last category as reference while R the first
#                 Estimate Std. Error    z value     Pr(>|z|)
#(Intercept)     5.7557422 0.05625440 102.316312 0.000000e+00
#lesscay         0.1658362 0.07645601   2.169041 3.007959e-02
#deathy         -3.9639827 0.41210584  -9.618846 6.657360e-22
#lesscay:deathy  1.0381366 0.47171197   2.200785 2.775125e-02

### Change the contrats so that the output crrespond
options(contrasts = c("contr.SAS", "contr.poly")) 
  LL1 <-  glm(count ~ lessca*death, family=poisson(),data=Data)  # Object N1 contains all components of the model
  LL1$family # What generalized linear model do we fit
  LL1$contrasts  # Treatment constrats - as in SAS
  S1<- summary(LL1) # Summary of the output
  S1$coefficients  # Get the coefficients
#                Estimate Std. Error   z value     Pr(>|z|)
#(Intercept)     2.995732  0.2236068 13.397322 6.268444e-41
#lesscan        -1.203973  0.4654747 -2.586549 9.694252e-03
#deathn          2.925846  0.2295233 12.747488 3.219516e-37
#lesscan:deathn  1.038137  0.4717120  2.200785 2.775125e-02

# Frequencies specified as an array.
l<-sapply(Data, function(x) length(levels(x)))
###lessca  death  count 
###     2      2      0
T <- array(0, l[-3], lapply(Data[, -3], levels))
T[data.matrix(Data[,-3])] <- Data$count  

LNf <- loglm(~lessca*death,T);LNf0<-loglm(~1:2,T)

#Normal distribution for the response (CD4 count) 

 LL2 <-  glm(count ~ lessca + death, family=poisson(),data=Data)  # Object N1 contains all components of the model
  S2<- summary(LL2) # Summary of the output
 
  S2$coefficients  # Get the coefficients
#              Estimate Std. Error   z value     Pr(>|z|)
#(Intercept)  2.6596236 0.19901495 13.363939 9.822600e-41
#lesscan     -0.1992581 0.07516726 -2.650863 8.028651e-03
#deathn       3.2771447 0.19978089 16.403695 1.799518e-60

LNi <- loglm(~lessca+death,T);LNi0<-loglm(~1+2,T)

##########  Chi-Squared test
chisq.test(T,correct=FALSE)

##############################################################
# Loglinear models in SxR tables - Analysis of melanoma data #
###############################################################

cat('\nLoglinear models in SxR tables - Analysis of melanoma data\n') 
melanoma <- read.csv('melanoma.csv')
melanoma$type<-factor(melanoma$type)

cat('\n Using Loglm \n');

### Construct the table

# Frequencies specified as an array.
l<-sapply(melanoma, function(x) length(levels(x)))
# type  site count 
#    4     3     0 
T <- array(0, l[-3], lapply(melanoma[, -3], levels))
T[data.matrix(melanoma[,-3])] <- melanoma$count

Ms <- loglm(~type:site, T) #Saturated Model
Mi <- loglm(~type+site, T) #Independence Model

### Compare the two models
anova(Ms,Mi)


cat( '\n\n\n Test independence by using PROC FREQ \n';
##########  Chi-Squared test
chisq.test(T,correct=FALSE)



