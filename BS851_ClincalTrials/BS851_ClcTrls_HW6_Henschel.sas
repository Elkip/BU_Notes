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

title 'Contingency Table of Pancreatic Cancer Treatment';
proc freq data=pancreas order=data;
	format Treatment TrtFmt. Death OutFmt.;
	weight count;
	tables Treatment*Death / chisq nopercent;
run;

*3. Calculate the risk difference for death between treatment groups;

*4. Use proc logistic statements to obtain Odds Ratios and Dunnett-adjusted p-values;
proc logistic data=pancreas;
class treatment(ref='0');
model death=treatment;
run;quit;

*5. Use proc logistic statements to obtain probability of death in each group;
