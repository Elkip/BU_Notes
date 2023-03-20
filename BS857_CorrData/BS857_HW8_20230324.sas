DATA  tests ;
LENGTH
 school $ 2
 class $ 1
 gender $ 1
 social $ 1
 id $ 4
 year $ 1
;

INFILE  "/home/elkip/Datasets/new_JSP.txt" 
     DSD 
     LRECL= 40 ;
INPUT school class gender social year1 id eng math year $;
RUN;

*1. Build a multilevel lienar model of the scores for math adjusting for gender and year 1 score;
proc mixed data=tests;
class id school gender year(ref='0');
model math = gender year1 year/solution chisq;
random intercept / subject = id;
random intercept / subject = school;
run;

*5. Same as above but where the correlation between repeated measurements is a function of time;
