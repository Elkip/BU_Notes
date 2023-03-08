libname MT 'Z:\';

data sbp;
	set MT.MT_2023;
run;

ods pdf startpage=never columns=2 file="W:\BS851_Midterm_Henschel_SAS_Output.pdf";
title 'BS851 Clinical Trials Midterm Henschel';

*1. Randomization;
Proc odstext;
p "Randomization Schedule" /style=[fontweight=bold fontsize=12pt];
run;

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

Proc odstext;
p "Male Site 1" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=1 and sex=1;
run;

Proc odstext;
p "Females Site 1" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=1 and sex=2;
run;

Proc odstext;
p "Males Site 2" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=2 and sex=1;
run;

Proc odstext;
p "Females Site 2" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=2 and sex=2;
run;

Proc odstext;
p "Male Site 3" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=3 and sex=1;
run;

Proc odstext;
p "Females Site 3" /style=[fontweight=bold fontsize=8pt];
run;
proc print data=rand(obs=5)
label noobs;
var patid sites trt blocks sex;
label patid='Patient ID'
blocks='Block #'
sites='Site'
sex='Sex'
trt='Treatment';
where sites=3 and sex=2;
run;

*2. Table of Baseline Comparability;
Proc odstext;
p "Table of Baseline Comparability" /style=[fontweight=bold fontsize=12pt];
run;
Proc odstext;
p "Mean Baseline SBP By Treatment Group" /style=[fontweight=bold fontsize=8pt];
run;
proc means data=sbp noprint median qrange;
	class trt;
	var sbp_base;
	output out=mean_base n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_base noobs;
	var trt n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

Proc odstext;
p "Frequency of Depression and Gender by Treatment Group" /style=[fontweight=bold fontsize=8pt];
run;
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
Proc odstext;
p "Analysis of Primary Outcome" /style=[fontweight=bold fontsize=12pt];
run;
Proc odstext;
p "Mean Change in SBP By Treatment Group" /style=[fontweight=bold fontsize=8pt];
run;
proc means data=sbp noprint median qrange;
	class trt;
	var sbp_change;
	output out=mean_chg n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_chg noobs;
	var trt n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

Proc odstext;
p "Test for Mean Change Between by Treatment Group" /style=[fontweight=bold fontsize=8pt];
run;
proc glm data=sbp;
	class trt(ref=&C);
	model sbp_change=trt/solution clparm alpha=.025;
	means trt/hovtest=levene welch;
run;quit;

Proc odstext;
p "Test for Mean Change Between Treatment Groups (Adjusted for Site)" /style=[fontweight=bold fontsize=8pt];
run;
proc glm data=sbp;
	class trt(ref=&C) site;
	model sbp_change=trt site/solution clparm alpha=.025;
	means trt/hovtest=levene welch;
run;quit;

Proc odstext;
p "Mean Change in SBP by Treatment and Site" /style=[fontweight=bold fontsize=8pt];
run;
proc means data=sbp noprint median qrange;
	class trt site;
	var sbp_change;
	output out=mean_chg n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_chg noobs;
	var trt site n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

*4. Analysis of Secondary Outcomes;
Proc odstext;
p "Analysis of Secondary Outcomes" /style=[fontweight=bold fontsize=12pt];
run;
Proc odstext;
p "Test of Outcome Hypertension Between Treatment Groups" /style=[fontweight=bold fontsize=8pt];
run;
proc logistic data=sbp;
	class trt(ref=&C);
	model sbp_high(event='1')=trt/alpha=.025;
run;quit;

Proc odstext;
p "Survival Analysis of Hospitalization Between Treatment Groups" /style=[fontweight=bold fontsize=8pt];
run;
proc lifetest data=sbp plots=(loglogs);
	time time*hosp(0);
	strata trt;
run;

Proc odstext;
p "Cox Model of Time Until Hospitalization" /style=[fontweight=bold fontsize=8pt];
run;
proc phreg data=sbp;
	class trt(param=ref ref="Placebo");
	model time*hosp(0)=trt/risklimits alpha=.025;
run;

ods pdf close;
