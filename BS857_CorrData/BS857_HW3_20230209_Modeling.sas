libname HW3 'Z:\';

data dental;
set HW3.dental;
y=y8; time=8; output;
y=y10; time=10; output;
y=y12; time=12; output;
y=y14; time=14; output;
drop y8 y10 y12 y14;
run;

title '1. LSMeans Height';
proc glimmix data=dental;
class id gender time;
model y = time gender time*gender;
lsmeans time*gender/plots=(meanplot(join sliceby=gender));
run;

title '2a. Unstructured Covariance';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=un subject=id r rcorr;
run;

title '2b. Compound Symmetry';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=cs subject=id r rcorr;
run;

*Compound - Hetero Compound;
data LR;
LR=469.9-462.5;
pvalue=1-probchi(LR,3);
run;
proc print data=LR;
run;

*Unstructured vs Compound;
data LR;
LR=469.9-458.4;
pvalue=1-probchi(LR,8);
run;
proc print data=LR;
run;

title '2c. Heterogeneous Compound Symmetry';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=csh subject=id r rcorr;
run;
*Unstructured - Hetero compound;
data LR;
LR=462.5-458.4;
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;

title '2d. Autoregressive';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=ar(1) subject=id r rcorr;
run;

*Autoregressive - Hetero Autoregressive;
data LR;
LR=471.6-462.4;
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;

*Autoreg - Unstructured;
data LR;
LR=2*(471.6-458.4);
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;

title '2e. Heterogeneous Autoregressive';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=arh(1) subject=id r rcorr;
run;

*Hetero Autoreg - Unstructured;
data LR;
LR=2*(462.4-458.4);
pvalue=1-probchi(LR,5);
run;
proc print data=LR;
run;



