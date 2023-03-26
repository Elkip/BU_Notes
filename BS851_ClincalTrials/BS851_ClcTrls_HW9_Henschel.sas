proc import out=bp
	datafile='/home/elkip/Datasets/BPdata.csv'
	dbms=csv
	replace;
	getnames=YES;
run;

* 1. Summarize the descriptive statistics of change in SBP in each treatment group;
title "Change in SBP By Treatment Group"
PROC means data=bp noprint median qrange;
	class trt;
	var change;
	output out=mean_base n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

* 2. Perform an unadjusted analysis of effect of change in SBP;


/* 3. Use GLM to assess if there is a significant treatment-by-site interaction 
	in change in SBP */


* 5. Summarize the descriptive statistics of CVD in each treatment group;


/* 6. Use PROC FREQ w/ a Breslow-Day Test to exaimine treatment-by-site interaction, 
	 adjust as needed */


* 7. Use PROC LOGISITIC to evaluate the interaction due to sex;

