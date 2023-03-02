data pancreas;
	input Treatment Death Count;
	datalines;
	0 0 16
	1 0 23
	2 0 33
	0 1 32
	1 1 25
	2 1 13
	;
run;

*2. Use PROC FREQ to set up a coningency table; 
proc format;
   value TrtFmt 2='High Dose'
				1='Low Dose'
                0='Control';
   value OutFmt 1='Yes'
                0='No';
run;

title 'Contingency Table of Pancreatic Cancer Treatment - High Dose';
proc freq data=pancreas order=data;
	format Treatment TrtFmt. Death OutFmt.;
	weight count;
	tables Treatment*Death / chisq nopercent;
	where Treatment in (2, 0);
run;

title 'Contingency Table of Pancreatic Cancer Treatment - Low Dose';
proc freq data=pancreas order=data;
	format Treatment TrtFmt. Death OutFmt.;
	weight count;
	tables Treatment*Death / chisq nopercent;
	where Treatment in (1, 0);
run;

*3. Calculate the risk difference for death between treatment groups;
title "Bonferroni: High_dose vs placebo";
proc freq data=pancreas;
weight count;
table treatment*Death/nocol nopercent chisq riskdiff(column=1 cl=wald norisks) alpha=0.025;
where Treatment in (2, 0);
run;

title "Bonferroni: Low_dose vs placebo";
proc freq data=pancreas;
weight count;
table treatment*Death/nocol nopercent chisq riskdiff(column=2
cl=wald norisks) alpha=0.025;
where Treatment in (1, 0);
run;

*4. Use proc logistic statements to obtain Odds Ratios and Dunnett-adjusted p-values;
title 'Dunnett Adjusted Logstic';
proc logistic data=pancreas;
class treatment(ref='0')/param=glm;
model death(event='1')=treatment;
lsmeans treatment/adjust=dunnett exp cl diff=Control("0");
freq count;
run;quit;
