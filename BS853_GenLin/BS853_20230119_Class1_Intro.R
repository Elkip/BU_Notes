##soh########################################################################
#   Boston University - Biostatistics Department
#   PROGRAM NAME          : script - Lecture 1.R
#                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 1)
#   PROJECT NAME          : Generalized Linear Models (BS853) 
#                           - Lecture: Generalized Linear Models
#
#   DESCRIPTION           : 1. Read data
#                           2. Fit Generalized Linear Models
#                                                        
#   SOFTWARE/VERSION#     : R/Version 3.0.2
#   INFRASTRUCTURE        : MAC OS 
#   INPUT                 : 
#   OUTPUT                : 
#  -----------------------------------------------------------------------------
#   Author : Gheorghe Doros 
#   Last modified 01/12/2020              
##eoh########################################################################
setwd('~/Documents/BS853/Class 1') 
Data<-read.csv('Imigration.csv')

M<-lm(Inc~En+Lit+US5,data=Data)
SumM<-summary(M)
# Density plot of residuals
plot(density(SumM$residuals))

### Normal Density Plot 
setEPS(paper='special')
### Plot a normal density
postscript('NormalDensity.eps',width=7,height=5,pointsize=12)
x<-seq(-3.1,3.1,by=0.001)
plot(x,dnorm(x),type='l',axes=FALSE,ylab='',xlab='',ylim=c(0,dnorm(0)+.3))
axis(1,at=c(-3,-2,-1,0,1,2,3),labels=c(expression(mu-3*sigma),expression(mu-2*sigma),expression(mu-1*sigma),expression(mu),
expression(mu+1*sigma),expression(mu+2*sigma),expression(mu+3*sigma)))
mtext(expression(paste('Density - f(y|' , mu,",",sigma,')           ',)),2)

lines(c(-1,-1),c(0,dnorm(0)+.07));lines(c(1,1),c(0,dnorm(0)+.07))
text(0,dnorm(0)+.03,'68% within',pos=3,cex=.8)
text(0,dnorm(0),'1 standard deviation',pos=3,cex=.8)
arrows(-.8,dnorm(0)+.04,-1,dnorm(0)+.04,length=.1)
arrows(.8,dnorm(0)+.04,1,dnorm(0)+.04,length=.1)#box()

lines(c(-2,-2),c(0,dnorm(0)+.15));lines(c(2,2),c(0,dnorm(0)+.15))
text(0,dnorm(0)+.12,'95% within',pos=3,cex=.8)
text(0,dnorm(0)+.09,'2 standard deviation',pos=3,cex=.8)
arrows(-.8,dnorm(0)+.13,-2,dnorm(0)+.13,length=.1)
arrows(.8,dnorm(0)+.13,2,dnorm(0)+.13,length=.1)#box()

lines(c(-3,-3),c(0,dnorm(0)+.25));lines(c(3,3),c(0,dnorm(0)+.25))
text(0,dnorm(0)+.22,'99.7% within',pos=3,cex=.8)
text(0,dnorm(0)+.19,'3 standard deviation',pos=3,cex=.8)
arrows(-.8,dnorm(0)+.23,-3,dnorm(0)+.23,length=.1)
arrows(.8,dnorm(0)+.23,3,dnorm(0)+.23,length=.1)#box()

dev.off()
### Plot a bernoulli and Binomial probability mass function
postscript('BinomialDensity.eps',width=8,height=5,pointsize=12)
par(mfrow=c(1,2))
x<- 0:1;
db<-dbinom(x,1,0.2)
plot(x,db,type='h',lwd=7,col='blue',xlab='',ylab='Density - Bernoulli(0.2)',axes=F,xlim=c(-0.2,1.2))
axis(1,at=c(0,1),label=c(0,1))
axis(2)
x<- 0:10;
db<-dbinom(x,10,0.2)
plot(x,db,type='h',lwd=7,col='blue',xlab='',ylab='Density - Binomial(10, 0.2)', axes=F,xlim=c(-0.2,10.4))
axis(1,at=0:10,label=0:10)
axis(1,at=10,label=10)
axis(2)
dev.off()


setEPS(paper='special')
tiff( "LR.tif", width=7, height = 5, pointsize = 12,unit='in',res=200 )


R<-function(theta) matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2) #Rotation matrix


D<-1
x0<-seq(-2*D,2*D,length=1000)
x1<-x0;y1<-dnorm(x0,sd=D);D1<-cbind(x1,y1)%*%R(pi/2);
x2<-x0;y2<-x0*0;D2<-cbind(x2,y2)%*%R(pi/2)
x2<-c(0,0);y2<-c(0,dnorm(0,sd=D));D2<-cbind(x2,y2)%*%R(pi/2)


xx<-seq(0,4,length=10); yy<- -4 + xx*9/4

L<-1000
x<-seq(0,4,length=L)
y<- -4 + x*9/4
yn<-y+rcauchy(L)/2
 #X11(width =9, height = 7, pointsize = 12)         
plot(c(0,4),c(-4,4),type='n',axes=FALSE, ylab='Outcome (Y)',xlab='Predictor(X)',ylim=c(-4,5))
lines(x,y,type='l',lty=2,lwd=2,cex=1,col='blue')
grid(nx=10,ny=0)
points(x, yn,pch=1,cex=.2,col='red')

par(xpd=T)

axis(1,at=seq(0,4,length=5),labels=rep('',5))
axis(2,at=seq(-4,5,length=5),labels=rep('',5))
for (i in 1:10){
dx1<-D1[,1]+xx[i];dy1<-D1[,2]+yy[i]
dx2<-D2[,1]+xx[i];dy2<-D2[,2]+yy[i]

lines(dx1,dy1,lwd=2,col='darkgreen')
lines(dx2,dy2,lwd=2,col='darkred')
}
points(xx, yy,pch=22,cex=1,col='darkgreen',bg='orange')

text(3,-3,expression(Y(X,beta,sigma)%~%N(X*beta,sigma^2)))


dev.off()


#Read the data from the console - Alternatively the data can be read from other external file, e.g. from a '.csv' file using read.csv() function 
cat("1 396 1 568  1 1212 1 171 1 554  1 1104 1 257 1 435 1 295  1 397
   1 288 1 1004 1 431  1 795 1 1621 1 1378 1 902 1 958 1 1283 1 2415
   2 375 2 375  2 752  2 208 2 151  2 116  2 736 2 192 2 315  2 1252 
   2 675 2 700  2 440  2 771 2 688  2 426  2 410 2 979 2 377  2 503", file="data.txt")
#Read the data as a vector
Data1 <- scan("Data.txt")
# Transform the data
Data2<-matrix(Data1,ncol=2,byrow=TRUE)

Data<-data.frame(Data2)
names(Data)<-c('disease','cd4')
Data$disease<-factor(Data$disease)

#Normal distribution for the response (CD4 count) 
 N1 <-  glm(cd4 ~ disease, data=Data)  # Object N1 contains all components of the model

 names(N1) # get the names of the objcts
 #[1] "coefficients"      "residuals"         "fitted.values"     "effects"           "R"                 "rank"              "qr"                "family"           
# [9] "linear.predictors" "deviance"          "aic"               "null.deviance"     "iter"              "weights"           "prior.weights"     "df.residual"      
#[17] "df.null"           "y"                 "converged"         "boundary"          "model"             "call"              "formula"           "terms"            
#[25] "data"              "offset"            "control"           "method"            "contrasts"         "xlevels"    
  
  N1$family # What generalized linear model do we fit
  #Family: gaussian 
  #Link function: identity 

  N1$contrasts  # Treatment constrats - as in SAS
  S1<- summary(N1) # Summary of the output
  
  names(S1)   # Get the components of S1
#[1] "call"           "terms"          "family"         "deviance"       "aic"            "contrasts"      "df.residual"    "null.deviance"  "df.null"        "iter"          
#[11] "deviance.resid" "coefficients"   "aliased"        "dispersion"     "df"             "cov.unscaled"   "cov.scaled" 
  
  S1$coefficients  # Get the coefficients
#            Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)   823.20   100.8259  8.164567 6.937410e-10
#disease2     -301.15   142.5894 -2.112009 4.131601e-02 

### Construct the lo(CD4 count)
Data$LogCD4 <- log(Data$cd4)
 
 # Lognormal Model for the CD4 counts
  LN1 <- glm(LogCD4~disease, data=Data)

  SN1<-summary(LN1)
  SN1$coefficients   # Get coefficients stats

  dev.res <- SN1$deviance.resid # get the devoaiance residuals
  qqnorm(dev.res)               # Normal q-q plot of the residuals


### Normal Disribution with LOG link  
  N2 <- glm(cd4 ~ disease, family=gaussian(link=log), data = Data)
  ### Other options
   #binomial(link = "logit")
   #gaussian(link = "identity")
   #Gamma(link = "inverse")
   #inverse.gaussian(link = "1/mu^2")
   #poisson(link = "log")
   #quasi(link = "identity", variance = "constant")    # Specify your own
   #quasibinomial(link = "logit")          # Overdispered and Underdispersed binomial
   #quasipoisson(link = "log")             # Overdispered and Underdispersed poisson

  
  N2$family         # what generalized linear model is fit
  #Family: gaussian 
  #Link function: log 
  SN2 <- summary(N2)
  dev.res2 <- SN2$deviance.resid # get the devoaiance residuals
  qqnorm(dev.res2)               # Normal q-q plot of the residuals 



#####  Poisson distribution  
 P1 <- glm(cd4 ~ disease, family=poisson(link = log), data=Data)
 
  P1$family         # what generalized linear model is fit
  #Family: poisson 
  #Link function: log 
  SP1 <- summary(P1)
  dev.res3 <- SP1$deviance.resid # get the deviance residuals
  qqnorm(dev.res3)               # Normal q-q plot of the residuals 


###  Negative Binomial distribution
library(MASS)                              ### Need to upload a new library to fit the negative binomial model
 NB1 <- glm.nb(cd4 ~ disease, data=Data)
 
 SNB1<-summary(NB1)
 
  SNB1$family         # what generalized linear model is fit
   #Family: Negative Binomial(2.6962) 
   #Link function: log 
  dev.res4 <- SNB1$deviance.resid # get the devoaiance residuals
  qqnorm(dev.res4)               # Normal q-q plot of the residuals 



