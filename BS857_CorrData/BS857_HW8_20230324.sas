data tests;
   infile "/home/elkip/Datasets/JSP.txt";
   input 
      school 1-2
      class 3
      gender 4
      social 5
       ravens 6-7
       id 8-11
       english 12-13
       math 14-15
       schyr 16
;
run;

*1. Build a multilevel lienar model of the scores for math adjusting for gender and year 1 score;
proc mixed data=tests method=REML;
class id school gender year(ref='0');
model math = gender year1 year/solution chisq;
random intercept / subject = id;
random intercept / subject = school;
run;

*5. Same as above but where the correlation between repeated measurements is a function of time;
proc mixed data=tests;
class id school gender;
model math = gender year1 year/solution chisq;
random year/ subject = id;
random intercept / subject = school;
run;