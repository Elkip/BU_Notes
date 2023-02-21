libname HW5 'Z:\';

data od;
set HW5.overdose;
run;

Title 'Kaplan-Meier survival plot of Linkage';
proc lifetest data=od;
time days*linkage(0);
strata int;
run;

Title 'Cox Model of Time Until Linkage';
proc phreg data=od;
class int;
model days*linkage(0)=int/risklimits;
run;

Title 'Log-Log Survival;Plot';
proc lifetest data=od plots=(loglogs);
time days*linkage(0);
strata int;
run;

Title 'Kaplan-Meier survival plot of Death';
proc lifetest data=od;
time days*death(0);
strata int;
run;
