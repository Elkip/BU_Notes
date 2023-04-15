* 1. Create a SAS dataset with a new varaible for diff in PEFR;
data pefr;
	input id sequence $ pefr_salbutamol pefr_formoterol;
	cards;
	01 FS 270 310
	02 SF 370 385
	03 SF 310 400
	04 FS 260 310
	05 SF 380 410
	06 FS 300 370
	07 FS 390 410
	09 SF 290 320
	10 FS 210 250
	11 FS 350 380
	12 SF 260 340
	13 SF 90 220
	14 FS 365 330
	;
run;

data pefr;
	set pefr;
	pefr_dif = pefr_formoterol - pefr_salbutamol;
run;

title 'PEFR in Children';
proc print data=pefr;
run;

* 3. Run diagnostics and a paired t-test to compare the two groups;
title 'PERF Daignostics Between Treatment';
proc means data=pefr NDEC=2;
	var pefr_salbutamol pefr_formoterol pefr_dif;
run;

title 'ttest for Difference in PEFR';
proc ttest data=pefr h0=0;
	var pefr_dif;
run;

* 4. Change the data from wide to long format; 
data long;
  set pefr;
  if sequence = 'FS' then do;
    period = 1; treat = 'formoterol'; pefr = pefr_formoterol; output;
	period = 2; treat = 'salbutamol'; pefr = pefr_salbutamol; output;
  end;
  else if sequence = 'SF' then do;
    period = 1; treat = 'salbutamol'; pefr = pefr_salbutamol; output;
	period = 2; treat = 'formoterol'; pefr = pefr_formoterol; output;
  end;
run;

title 'PEFR in Long Format';
proc print data=long;
run;

* 5. Correlated data analysis with carryover effect with un and alpha = .15;
title 'PROC MIXED with Carryover Effect'
proc mixed data=long;
	class id period treat;
	model pefr = treat period treat*period/s chisq;
	repeated period / type=un subject=id r;
run;

* 6. Same analysis with no carryover effect;
title 'PROC MIXED no Carryover Effect';
proc mixed data=long;
	class id period treat;
	model pefr = treat period/s chisq;
	repeated period / type=un subject=id r;
run;

* 7. Treatment as the only predictor;
title 'PROC MIXED with Treatment Only';
proc mixed data=long;
	class id treat period;
	model pefr = treat/s chisq;
	repeated period / type=un subject=id r;
run;
