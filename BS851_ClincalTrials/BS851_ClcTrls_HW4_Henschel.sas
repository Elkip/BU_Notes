libname HW4 'Z:\';
*Put entire output in PDF;
*ods pdf file="W:\BS851_HW4_Henschel_SAS_Output.pdf";

data help;
	set HW4.help;
	drop dayslink;
run;

/* 1.
Provide mean (SD) for continuous and n (%) for categorial
Include a table of baseline comparability by treatment group including:
	- Substance used
	- Gender
	- Baseline CESD
Include a statistical comparision between groups
*/
title "HELP Data Information";
proc means data=help noprint;
	class TREAT;
	var CESD1 CESD CHANGE_CESD;
	output out=means(drop=_:) mean= n= std=/ autoname autolabel;
run;

proc print data=means noobs;
	var treat cesd_n cesd1_Mean cesd1_StdDev cesd_Mean cesd_StdDev change_cesd_Mean change_cesd_StdDev;
	format cesd1_Mean cesd1_StdDev cesd_Mean cesd_StdDev change_cesd_Mean change_cesd_StdDev 8.1;
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

proc freq data=help;
	tables FEMALE SUBSTANCE LINKSTATUS TREAT/nocum;
run;

proc sort data=help;
	by treat;
run;

proc freq data=help;
	by treat;
	tables FEMALE SUBSTANCE LINKSTATUS/nocum;
run;

*ttest for difference in change in LINKSTATUS;
proc freq data=help order=data;
	table treat*linkstatus/nocol nopercent chisq
	riskdiff (column=1 CL=wald CL=newcombe(correct) norisks)
	relrisk(column=1 CL=wald) oddsratio(CL=wald);
run;

*4 Use Logistic regression to test the null hypothesis;
proc logistic data=help;
	class treat(ref='0');
	model linkstatus(event='1') = treat;
run; quit;

*5 Use Logistic regression but stratify on Substance;
proc logistic data=help;
	class SUBSTANCE treat(ref='0') ;
	model linkstatus(event='1') = treat substance;
run; quit;

*7 Test difference in mean change in cesd between treatment groups;
proc glm data=help;
	class treat(ref='0');
	model change_cesd = treat /solution clparm;
	means treat/hovtest=levene welch;
run;quit;

*8 Test difference ;
proc glm data=help;
	class treat(ref='0') substance;
	model change_cesd = treat substance /solution clparm;
	means treat/hovtest=levene welch;
run;quit;
