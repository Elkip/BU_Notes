libname hw3 'C:\Users\elkip\Desktop\Data';

proc print data=hw3.exercise3;
run;

/*Working with a subset to save processing time
data hw3exercise3;
	set hw3.exercise3;
	if _N_ <= 100 then output;
run;

proc print data = hw3exercise3;
run;*/

proc sort data = hw3.exercise3;
	by NACCID vnumber;
run;

/* Create wide data sets */
proc transpose data=hw3.exercise3 out=hw3.MMS_WIDE prefix=MMS;
	by naccid;
	id vnumber;
	var NACCZMMS;
run;

proc transpose data=hw3.exercise3 out=hw3.LMI_WIDE prefix=LMI;
	by naccid;
	id vnumber;
	var NACCZLMI;
run;

proc transpose data=hw3.exercise3 out=hw3.LMD_WIDE prefix=LMD;
	by naccid;
	id vnumber;
	var NACCZLMD;
run;

proc transpose data=hw3.exercise3 out=hw3.DFT_WIDE prefix=DFT;
	by naccid;
	id vnumber;
	var NACCZDFT;
run;

/* Create table of Age, Cog Status and sex */
data hw3.demographics;
set hw3.exercise3;
by naccid;
if first.naccid then output;
rename naccageb=age_bl;
label naccageb='Age at Baseline';
rename naccudsd=cog_sts;
label naccudsd='Cognitive Status';
keep naccid sex naccudsd naccageb;
run;

/* Merge wide datasets */
data hw3.ex3_wide(drop = _LABEL_);
merge hw3.demographics hw3.dft_wide hw3.lmd_wide hw3.lmi_wide hw3.mms_wide;
by naccid;
run;

/* Clean Data */
data ex3_wide;
set hw3.ex3_wide;
array missing{40} dft1-dft10 lmd1-lmd10 lmi1-lmi10 mms1-mms10;
do i=1 to 40;
if missing{i} = 99 then missing{i} = .;
end;
drop _NAME_;
run;
