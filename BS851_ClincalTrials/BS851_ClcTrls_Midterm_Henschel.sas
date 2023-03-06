libname MT 'Z:\';

data sbp;
	set MT.MT_2023;
run;

ods pdf file="W:\BS851_Midterm_Henschel_SAS_Output.pdf";

*1. Randomization;
title 'Randomization Schedule';
%let A="Dose1";
%let B="Dose2";
%let C="Placebo";

proc plan seed=1234;
factors sex=2 sites=3 ordered blocks=18 ordered trt=6 random/noprint;
output out=rand trt cvals=(&A &A &B &B &C &C) ;
run;quit;

data rand;
	set rand;
	by sex sites;
	patient+1;
	if first.sites then patient=1;
	if sites=1 and sex=1 then
		patid=patient+1000;
	if sites=1 and sex=2 then
		patid=patient+2000;
	if sites=2 and sex=1  then
		patid=patient+3000;
	if sites=2 and sex=2  then
		patid=patient+4000;
	if sites=3 and sex=1 then
		patid=patient+5000;
	if sites=3 and sex=2 then
		patid=patient+6000;
run;

title1 'Males Site 1';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=1 and sex=1;
run;

title1 'Females Site 1';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=1 and sex=2;
run;

title1 'Males Site 2';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=2 and sex=1;
run;

title1 'Females Site 2';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=2 and sex=2;
run;

title1 'Males Site 3';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=3 and sex=1;
run;

title1 'Females Site 3';
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
where sites=3 and sex=2;
run;

*2. Table of Baseline Comparability;
title 'Tables of Baseline Comparability';
title1 'Mean Baseline SBP By Treatment Groups';
proc means data=sbp noprint median qrange;
	class trt;
	var sbp_base;
	output out=mean_base n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_base noobs;
	var trt n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

title1 'Frequency of Depression and Gender by Treatment Group';
proc sort data=sbp;
	by trt;
run;

proc template;
	edit Base.Freq.OneWayList;  
	edit Percent;
	  format=8.1 ;           
	end;
	edit CumPercent;
	  format=8.1 ;
	end;
  end;
run;

proc freq data=sbp;
	BY TRT;
	tables FEMALE SBP_BASE_HIGH/nocum;
run;

*3. Analysis of Primary Outcome;
title 'Analysis of Primary Outcome';
title1 'Mean Change in SBP Treatment Groups';
proc means data=sbp noprint median qrange;
	class trt;
	var sbp_change;
	output out=mean_chg n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_chg noobs;
	var trt n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

title1 'ttest for Mean Change Between Treatment Groups';
proc glm data=sbp;
	class trt(ref=&C);
	model sbp_change=trt/solution clparm alpha=.025;
	means trt/hovtest=levene welch;
run;quit;

ods pdf close;
