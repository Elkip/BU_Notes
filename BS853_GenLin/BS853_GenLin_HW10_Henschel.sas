
*Exercise 1: Custom Dataset;
proc import out=pbc
    datafile="Z:/pbc.csv"
    dbms=csv
    replace;
    getnames=YES;
run;

* Examine events by treatment and time;
proc freq data=pbc;
table stage*status*trt/ nocum nocol norow;
run;

*Assess Proportional Hazards through log-log plots;
proc lifetest data=pbc plot=(s,ls,lls);
	time stage*status(0);
	strata trt;
run;

*Cox Proportional Hazards Model; 
proc phreg data=pbc;
	model stage*status(0)=trt/rl;
	assess var=(trt) PH/resample;
run;

proc phreg data=pbc;
	class sex;
	model stage*status(0) = trt age chol sex/rl;
	assess var=(trt age chol) PH/resample;
run;
