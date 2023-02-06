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

title '2c. Heterogeneous Compound Symmetry';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=csh subject=id r rcorr;
run;

title '2d. Autoregressive';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=ar(1) subject=id r rcorr;
run;

title '2e. Heterogeneous Autoregressive';
proc mixed data=dental method=REML;
class id time gender;
model y=time gender time*gender/s chisq;
repeated time/type=arh(1) subject=id r rcorr;
run;
