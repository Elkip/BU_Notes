# ###soh########################################################################                                                        
#   Boston University - Biostatistics Department                                                                                         
#   PROGRAM NAME          : script - Lecture 5.R                                                                               
#                           (/Users/doros/Documents/Class 5)                                        
#   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
#                           - Lecture: Generalized Linear Models                                                                         
#                                                                                                                                        
#   DESCRIPTION           : 1. Read data                                                                                                 
#                           2. Testing in Generalized Linear Models                                                      
#                                                                                                                                        
#   SOFTWARE/VERSION#     : R/Version 2.12.2                                                                                              
#   INFRASTRUCTURE        : Mac OS                                                                                                      
#   INPUT                 :                                                                                                              
#   OUTPUT                :                                                                                                              
#  -----------------------------------------------------------------------------                                                         
#   Author : Gheorghe Doros                                                                                                              
#                                                                                                              
# ##eoh##########################################################################                                                         
                                                
setwd('/Users/doros/Documents/Class 5')
library(MASS)
library(contrast)
library(multcomp)

### We are interested in the ODDS Ratio for remission comparing the first labeling index group ('8-12') 
### to the last labeling index group ('34-38') 
remission <- read.csv('remission.csv')


cat('\n Default order - Alphanumeric ordering \n GLM Coding \n');

M <- glm(cbind(remiss,total-remiss) ~ LLI, family=binomial(), data=remission)
contrast(M,list(LLI='8-12'), list(LLI='34-38'), fun=exp)

# Levels order
levels(remission$LLI)

# Change order
remission$LLI <- factor(remission$LLI, levels = c("8-12", "14-18", "20-24", "26-32", "34-38"))

M1 <- glm(cbind(remiss,total-remiss) ~ LLI, family=binomial(), data=remission)
contrast(M1,list(LLI='8-12'), list(LLI='34-38'), fun=exp)

### Change the contrats so that the output crrespond
options(contrasts = c("contr.SAS", "contr.poly"))  # SAS Contrast
options()$contrasts
M2 <- glm(cbind(remiss,total-remiss) ~ LLI, family=binomial(), data=remission); contrast(M2,list(LLI='8-12'), list(LLI='34-38'), fun=exp)
options(contrasts = c("contr.treatment", "contr.poly"))  # Default contrast
options()$contrasts
M3 <- glm(cbind(remiss,total-remiss) ~ LLI, family=binomial(), data=remission); contrast(M3, list(LLI='8-12'), list(LLI='34-38'), fun=exp)
options(contrasts = c("contr.sum", "contr.poly"))    # Reference contrast
options()$contrasts
M4 <- glm(cbind(remiss,total-remiss) ~ LLI, family=binomial(), data=remission); contrast(M4, list(LLI='8-12'), list(LLI='34-38'), fun=exp)


cat('\nTwo contrasts for multiple hypotheses\n')
options(contrasts = c("contr.SAS", "contr.poly")) 
options()$contrasts

contrast(M4, list(LLI=c('8-12','8-12')), list(LLI=c('34-38','14-18')))

Ctrst<- rbind(c(1, 0, 0, 0, -1), c(1, -1, 0, 0, 0))
Cm<-glht(M2,linfct=mcp(LLI=Ctrst))
summary(Cm)                         # Take the min p-value as an overall p-value


###################################################################
### CHD in Framingham Study###

chd <- read.csv('CHD.csv');  
cat( '\n Contrasting <200 vs. 220-259 with CONTRAST \n')
M1 <- glm(cbind(CHD,Total-CHD) ~ Chol + SBP, family=binomial(), data=chd)
contrast(M1, list(Chol='<200', SBP=levels(chd$SBP)), list(Chol='220 - 259', SBP=levels(chd$SBP)),type='average')
contrast(M1, list(Chol='<200', SBP='127 - 146'), list(Chol='220 - 259', SBP='127 - 146'))
contrast(M1, list(Chol='<200', SBP=c('<127','147 - 166')), list(Chol='<200',  SBP='127 - 146'),type='average')


##################################################################
### Admission Data                                             ###
##################################################################

one <- read.csv('gradAdmission.csv');names(one)

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


#######################################################
##### Treatment for Urinary Tract Infection (UTI) #####
#######################################################
 UTI <- read.csv('UTI.csv');names(UTI)
 PM<-glm(count ~ diag*treat + diag*resp + treat*resp, family=poisson(), data=UTI)

a1<-c(0,  0, 0, 0, 0, 0, 0,  0, 1,  0)
a2<-c(0,  0, 0, 0, 0, 0, 0,  0, 1, -1)
a3<-c(0,  0, 0, 0, 0, 0, 0,  0, 0,  1)
Cont<-rbind(a1,a2,a3)
rownames(Cont='trt A vs C','trt A vs B','trt B vs C')
# Individual tests
Res1<-summary(glht(PM,linfct=t(a1)))
Res2<-summary(glht(PM,linfct=t(a2)))
Res3<-summary(glht(PM,linfct=t(a3)))
# All tests
Res<-glht(PM,linfct=Cont)
summary(Res)

####################################################################################
#####  Breathlessness and wheeze in coal miners Ashford and Sowden (1970)      #####
####################################################################################

### read in data to use for plots ###
coalminr <- read.csv('coalminr.csv')
coalminr$cage <- coalminr$age - 5;   
coalminr$sqage <- coalminr$cage^2;
coalminr$brthlss1 <- 0+(coalminr$brthlss=='yes');
coalminr$wheeze1 <- 2-coalminr$wheeze
coalminr$wheeze <- factor(coalminr$wheeze)
coalminr$age <- factor(coalminr$age)

cat('\n CMH test for conditional independence \n');

l<-sapply(coalminr[c('wheeze','brthlss', 'age', 'count')], function(x) length(levels(x)))
Tc <- array(0, l[-4], lapply(coalminr[c('wheeze','brthlss', 'age', 'count')][, -4], levels))
Tc[data.matrix(coalminr[c('wheeze','brthlss', 'age', 'count')][,-4])] <- coalminr$count 
ftable(Tc) 
mantelhaen.test(Tc,correct=FALSE)


###Create data sets for studying the relation of wheeze and breathlessmess with age###
# Construct the wheeze data
wheeze <- data.frame(tapply(coalminr$count,list(coalminr$age,coalminr$wheeze ) , sum))
names(wheeze) <- c('yes','no')
wheeze$age <- as.numeric(as.character(rownames(wheeze)))
wheeze$cage <- wheeze$age - 5;   
wheeze$sqage <- wheeze$cage^2;
# Construct the breathlessness data
breathlessness <- data.frame(tapply(coalminr$count,list(coalminr$age,coalminr$brthlss) , sum))
breathlessness$age <- as.numeric(as.character(rownames(wheeze)))
breathlessness$cage <- breathlessness$age - 5;   
breathlessness$sqage <- breathlessness$cage^2;



###Model 1###
cat('\n Predicting Breathlessness rates with age \n Only linear effect of age in the model \n');
M1 <- glm(cbind(yes,no) ~ cage, family=binomial(), data=breathlessness);

###Model 2###
cat('\n Predicting Breathlessness rates with age \n Linear and quadratic effects of age in the model \n');
M2 <- glm(cbind(yes,no) ~ cage + sqage, family=binomial(), data=breathlessness);

###Model 3###
cat('\n Predicting Wheeze rates with age \n Only linear effect of age in the model \n');
M3 <- glm(cbind(yes,no) ~ cage, family=binomial(), data=wheeze);

cat('\n Predicting Breathlessness rates with age \n Linear and quadratic effects of age in the model \n Use estimate to compare age groups \n');
Agec <- glht(M2,linfct=t(c(0,-4, 16)))
summary(Agec)

###############################################################
##### Use Loglinear models to analyse the data            ##### 
###############################################################
cat('\n All 2 way interaction model \n');
M4 <- glm(count ~ age*brthlss1 + age*wheeze1 + brthlss1*wheeze1, family=poisson(), data=coalminr);


cat('\n Allowing OR between Wheeze and Breathlessness to vary with age \n');
M5 <- glm(count ~ age*brthlss1 + age*wheeze1 + brthlss1*wheeze1+I(cage*brthlss1*wheeze1), family=poisson(), data=coalminr);
