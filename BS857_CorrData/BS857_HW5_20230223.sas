libname HW5 'Z:\';

data birth;
set HW5.birthwt;
run;

*1. Fit the mixed effects model with linear trend for maternal age and randomly varying intercepts;
