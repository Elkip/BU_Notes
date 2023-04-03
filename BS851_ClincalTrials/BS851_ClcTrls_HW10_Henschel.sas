
/* 
Analysis schedule:

(1) 500  participants (.4)
(2) 700  participants (.56)
(3) 900  participants (.72)
(4) 1100 participants (.88)
(5) 1250 participants (1) 
*/

*a. BF Two Sided Significant for 5 analyses;
proc seqdesign errspend;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.4 .56 .72 .88 1)
	method=errfuncobf
	stop=reject;
run;


proc seqdesign errspend bscale=pvalue;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.4 .56 .72 .88 1)
	method=errfuncobf
	stop=reject;
run;

*b. Revise the study if only 450 patients (.36) are in the first analysis;
proc seqdesign errspend;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.36 .56 .72 .88 1)
	method=errfuncobf
	stop=reject;
run;

proc seqdesign errspend bscale=pvalue;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.36 .56 .72 .88 1)
	method=errfuncobf
	stop=reject;
run;

*c. After the first analysis the sample size is increased from 1250 to 1750,
	but the analyses 2-4 are still performed at the same points.
	Additionally, at each time of analysis only spend the same alpha that we
	spent in part b;
proc seqdesign errspend;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.26 .4 .51 .63 1)
	method=errspend(.00038 .00548 .0163 0.0338 .05)
	stop=reject;
run;

proc seqdesign errspend bscale=pvalue;
	TwoSidedObrienFleming: design nstages=5 alpha=.05
	alt=twosided 
	info=cum(.26 .4 .51 .63 1)
	method=errspend(.00038 .00548 .0163 0.0338 .05)
	stop=reject;
run;

*d. Use a chi-sqaure test to determine the p-value;
data srv;
	input trt outcome count;
	cards;
	0 0 228
	0 1 122
	1 0 192
	1 1 158
	;
run;

proc freq data=srv;
	weight count;
	tables trt*outcome/chisq nopercent;
run;