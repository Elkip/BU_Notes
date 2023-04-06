libname S857 'C:\Users\ytrip\Dropbox\Courses\BS857\2020\Datasets'; 
title "Monthly Deaths from Lung Diseases in the UK - both sexes";
   proc sgplot data=s857.ldeaths;
      series x=month y=deaths/ markers;
   run;
ods graphics on; 
proc arima data=s857.ldeaths; 
identify var=deaths nlag=24; 
run;
ods graphics off;
   data ldeaths;
   set s857.ldeaths;
   *deaths (0,0,0) (0,0,0);
   death_1=deaths-lag1(deaths);*(0,1,0) (0,0,0);
   death_S=deaths-lag12(deaths);*(0,0,0) (0,1,0);
   death_12=death_1-lag12(death_1);*(0,1,0) (0,1,0);
   run;
   title "Detrended Monthly Deaths from Lung Diseases in the UK - both sexes";
   proc sgplot data=ldeaths;
      series x=month y=death_1/ markers;
   run;
      title "Deseasonalized Monthly Deaths from Lung Diseases in the UK - both sexes";
   proc sgplot data=ldeaths;
      series x=month y=death_12/ markers;
   run;
ods graphics on;
   proc arima data=ldeaths; 
identify var=death_1 nlag=24; 
run;
   proc arima data=ldeaths; 
identify var=death_12 nlag=24; 
run;
ods graphics on;
   proc arima data=ldeaths; 
identify var=death_12 nlag=24; 
estimate  p=(1 2) (12) noint method=ml;*(2,1,0)(1,1,0);
*estimate  q=(1 2)(12)  noint method=ml;/*(0,1,2)(0,1,1)*/
*estimate p=(1)(12) q=(1 2)(12)  noint method=ml;*(1,1,2)(1,1,1)
*estimate  q=(1)(12)  noint method=ml;*(0,1,1)(0,1,1)

run;
quit;
ods graphics off;
ods graphics on;
   proc arima data=ldeaths plots=all; 
identify var=deaths(1,12) nlag=12; 
estimate p=(1 2)(12)  noint method=ml;
forecast id=month interval=month printall out=b;
run;
quit;
ods graphics off;

