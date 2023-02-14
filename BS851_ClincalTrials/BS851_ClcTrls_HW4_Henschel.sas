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
proc means data=help;
	class TREAT;
	var CESD1 CESD CHANGE_CESD;
	id ID;
	output out=means(drop=_:) mean= n=/ autoname;
run;

proc print data=means noobs;
	format cesd1_Mean cesd_Mean change_cesd_Mean 8.1;
run;

proc freq data=help;
	by treat;
	tables FEMALE SUBSTANCE LINKSTATUS/nocum;
run;

proc print data=means_out noobs;
	var treat n mean std mean min max QRANGE;

run;

proc sort data=help;
	by treat;
run;

proc freq data=help;
	by treat;
	tables FEMALE SUBSTANCE LINKSTATUS/nocum;
run;

