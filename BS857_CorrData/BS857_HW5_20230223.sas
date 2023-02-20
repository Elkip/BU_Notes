libname HW5 'Z:\';

data birth;
set HW5.birthwt;
run;

*1 Fit the mixed effects model with linear trend for 
maternal age and randomly varying intercepts;
proc mixed data=birth covtest plots=all;
model weight=age/s chisq;
random intercept/type=un subject=mid G V;
run;

proc mixed data=birth covtest plots=all;
class order;
model weight=age/s chisq;
random intercept/type=un subject=mid G V;
run;

*2 Fit the fixed effect model with beta-age (FE);
proc glm data=birth;
class mid;
model weight=age;
run;

proc mixed data=birth covtest plots=all;
class order;
model weight=age/s chisq;
repeated order/type=sp(exp) (order) subject=mid;
run;

*4 Construct a histogram of standardized residuals along with a QQ plot;


*5 Are any mothers outliers?;

*6 Plot the semi-variogram for the standardized residuals and lowess smoothed curve;
