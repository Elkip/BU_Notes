libname HW3 'Z:\';
*Put entire output in PDF;
ods pdf file="W:\BS851_HW1_Henschel_SAS_Output.pdf";

*1. Create a binary variable for depression at visit 8;
data dprsd;
	set HW3.depression;
	if Y>7 then dprsn = 1;
	else dprsn = 0; 
	where visit=8;
run;

title '2. HAMD17 Change from Visit 4 to 8';
proc means data=dprsd noprint median QRANGE;
	class trt;
	var change;
	id patient;
	output out=means_out n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=means_out noobs;
	var trt n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

title '4. Test for Equality of Variance and Means of Change';
proc ttest data=dprsd;
	class trt;
	var change;
run;

title '5 & 6. Chi-Sqaured Test for Binary Outcome';
proc sort data=dprsd;
by descending trt dprsn;
run;

proc freq data=dprsd order=data;
	table trt*dprsn/nocol nopercent chisq
	riskdiff (column=1 CL=wald CL=newcombe(correct) norisks)
	relrisk(column=1 CL=wald) oddsratio(CL=wald);
run;


ods pdf close;
