library("JM")
library("lattice")
head(aids)
head(aids.id)
#LME fit
lmeFit.aids<-lme(CD4~obstime+obstime:drug,
                 random=~ obstime|patient,data=aids)
summary(lmeFit.aids)
#Cox fit
coxFit.aids<-coxph(Surv(Time,death)~drug,
                   data=aids.id,x=TRUE)
summary(coxFit.aids)
#JM fit
jointFit.aids<-jointModel(lmeFit.aids,coxFit.aids,
                          timeVar='obstime',
                          method='piecewise-PH-aGH')
summary(jointFit.aids)

#Alternative JM fits
#Piecewise-constant baseline risk function (much slower)
jointFit.aids1<-jointModel(lmeFit.aids,coxFit.aids,
                          timeVar='obstime',
                          method='piecewise-PH-GH')
summary(jointFit.aids1)

#Spline
jointFit.aids2<-jointModel(lmeFit.aids,coxFit.aids,
                           timeVar='obstime',
                           method='spline-PH-GH')
summary(jointFit.aids2)


#Spline model for longitudinal outcome

lmeFit.av <- lme(CD4 ~ ns(obstime, 3) * drug ,
                 data = aids, random = list(patient = pdDiag(form = ~ ns(obstime, 3))))
summary(lmeFit.av)
coxFit.av<-coxph(Surv(Time,death)~drug,
                   data=aids.id,x=TRUE)
jointFit.av<-jointModel(lmeFit.av,coxFit.av,
                          timeVar='obstime',
                          method='piecewise-PH-aGH',verbose=TRUE)
summary(jointFit.av)



# Marginal Wald tests for the regression coefficients of the
# longitudinal submodel
anova(jointFit.aids, process = "Longitudinal")
# Multivariate Wald test for simultaneously testing all
# regression coefficients of the survival submodel
anova(jointFit.aids, process = "Event", L = diag(2))
# Likelihood ratio test for the treatment effect in the survival
# submodel
lmeFit.aids <- lme(CD4 ~ obstime + obstime:drug,
                   random = ~ obstime | patient, data = aids)

coxFit2.aids <- coxph(Surv(Time, death) ~ 1, data = aids.id, x = TRUE)

jointFit2.aids <- jointModel(lmeFit.aids, coxFit2.aids,
                             timeVar = "obstime", method = "piecewise-PH-aGH")

anova(jointFit2.aids, jointFit.aids)
