proc import out=bp
	datafile='/home/elkip/Datasets/BPdata.csv'
	dbms=csv
	replace;
	getnames=YES;
run;

* 1. Summarize the descriptive statistics of change in SBP in each treatment group;
title "Change in SBP By Site";
PROC means data=bp noprint median qrange;
	class site;
	var change;
	output out=mean_chg n=n mean=mean std=std min=min max=max QRANGE=QRANGE;
run;

proc print data=mean_chg noobs;
	var site n mean std mean min max QRANGE;
	format mean std mean QRANGE 8.1;
run;

* 2. Perform an unadjusted analysis of effect of change in SBP;
title "Analysis of Change in SBP Unadjusted";
proc glm data=bp;
	class trtgrp(ref='0');
	model change=trtgrp / solution clparm;
run;quit;

title "Analysis of Change in SBP Adjusted for Site";
proc glm data=bp;
	class trtgrp(ref='0') site;
	model change=trtgrp site / solution clparm;
run;quit;

/* 3. Use GLM to assess if there is a significant treatment-by-site interaction 
	in change in SBP */
title "Analysis of Change in SBP with Trt*Site Interaction";
proc glm data=bp;
	class site trtgrp(ref='0');
	model change=trtgrp | site / solution clparm ss3;
run;quit;

* 4. Perform a subgroup analysis stratified by site;
title "Analysis of Change in SBP Site 1";
proc glm data=bp;
	class trtgrp(ref='0');
	model change=trtgrp / solution clparm;
    where site = '1';
run;quit;

title "Analysis of Change in SBP Unadjusted";
proc glm data=bp;
	class trtgrp(ref='0');
	model change=trtgrp / solution clparm;
run;quit;

title "Analysis of Change in SBP Unadjusted";
proc glm data=bp;
	class trtgrp(ref='0');
	model change=trtgrp / solution clparm;
run;quit;

* 5. Summarize the descriptive statistics of CVD in each treatment group;
title "CVD By Treatment Group";
proc sort data=bp;
    by sex;
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

proc freq data=bp;
    tables cvd*trtgrp/nocum;
run;

proc freq data=bp;
    BY sex;
    tables cvd*trtgrp/nocum;
run;

/* 6. Use PROC FREQ w a Breslow-Day Test to exaimine treatment-by-site interaction, 
	 adjust as needed */
proc freq data=bp order=formatted;
	tables trtgrp*sex / chisq cmh nocol nopercent;
run;

* 7. Use PROC LOGISITIC to evaluate the interaction due to sex;
proc logistic data=bp;
	class cvd=trtgrp|sex / risklimits;
	oddsratio cvd / at (center=all);
run;
