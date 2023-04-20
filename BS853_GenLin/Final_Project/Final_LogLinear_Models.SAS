/* Final Project: Log-Linear Models (BS 853) */
/* Import Data */
libname BS853 '/home/elkip/Datasets';
data frmgham;
set BS853.frmgham;
where Period = 3; /* Restrict to Period 3 */
run;

/* Create Variable for Triglycerides */
data frmgham;
set frmgham;
Tri = 5*(totchol - HDLC - LDLC);
run;

* Examine distribution of ages;
proc univariate data=frmgham;
	var AGE;
	histogram;
run;

/**************************************************************/
/* Log Linear Models: Total Cholesterol, Smoking, and CVD */
/**************************************************************/
/* Create Categorical Variable for Total Cholesterol */
/* https://www.hopkinsmedicine.org/health/treatment-tests-and-therapies/lipid-panel */
data frmgham;
set frmgham;
if totchol = "." then chol = .; /* Missing Data */
else if totchol < 200 then chol = 0; /* Normal */
else if totchol >= 200 & totchol < 240 then chol = 1; /* Borderline High */
else if totchol >= 240 then chol = 2; /* High */
run;

/* Determine Counts for Variables of Interest */
proc sql;
select chol, cursmoke, cvd, count(*) as total_count
from frmgham
group by chol, cursmoke, cvd;
quit;

/* Create Dataset to Plot Odds of CVD vs. Total Cholesterol by Current Smoking Status */
Title 'Dataset for Plot of Odds of CVD vs. Total Cholesterol';
data plot;
input chol cursmoke $ 3-12 cvdOdds;
datalines;
0 Non-Smoker 0.27986
0 Smoker 0.30899
1 Non-Smoker 0.28257
1 Smoker 0.32143
2 Non-Smoker 0.32386
2 Smoker 0.30178
;
run;

/* Plot of Odds of CVD vs. Total Cholesterol by Current Smoking Status */
title1 'Odds of CVD vs. Total Cholesterol by Current Smoking Status';
symbol1 v=dot c=black;
symbol2 v=circle;
axis1 label=('Total Cholesterol Category');
axis2 label=(a=90 'CVD Odds') minor=none ;
legend1 label=('Smoking Status');
proc gplot data = plot;
plot cvdOdds*chol = cursmoke/vaxis=axis2 haxis=axis1 legend=legend1;
run;
quit;

/* Create new dataset for log-linear models */
Title 'Dataset for Log Linear Models';
data loglinear;
input chol cursmoke cvd Count;
datalines;
0 0 0 293
0 0 1 82
0 1 0 178
0 1 1 55
1 0 0 545
1 0 1 154
1 1 0 280
1 1 1 90
2 0 0 704
2 0 1 228
2 1 0 338
2 1 1 102
;
run;

/* Model 1: Saturated Model */
title 'Model 1: Saturated Model';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cursmoke|cvd / dist = Poisson;
run;

/* Model 2: All Two Way Interactions Model */
title 'Model 2: All Two Way Interactions Model';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cursmoke chol|cvd cursmoke|cvd / dist = Poisson;
run;

/* Model 3: Conditional Independence of Cholesterol and Smoking */
title 'Model 3: Conditional Independence of Cholesterol and Smoking';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cvd cursmoke|cvd / dist = Poisson;
run;

/* Model 4: Conditional Independence of Cholesterol and CVD */
title 'Model 4: Conditional Independence of Cholesterol and CVD';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cursmoke cursmoke|cvd / dist = Poisson;
run;

/* Model 5: Conditional Independence of Smoking and CVD */
title 'Model 5: Conditional Independence of Smoking and CVD';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cursmoke chol|cvd / dist = Poisson;
run;

/* Model 6: Joint Independence of (Cholesterol and Smoking) from CVD */
title 'Model 6: Joint Independence of (Cholesterol and Smoking) from CVD';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cursmoke cvd / dist = Poisson;
run;

/* Model 7: Joint Independence of (Cholesterol and CVD) from Smoking */
title 'Model 7: Joint Independence of (Cholesterol and CVD) from Smoking';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol|cvd cursmoke / dist = Poisson;
run;

/* Model 8: Joint Independence of (Smoking and CVD) from Cholesterol */
title 'Model 8: Joint Independence of (Smoking and CVD) from Cholesterol';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = cursmoke|cvd chol / dist = Poisson;
run;

/* Model 9: Mutual Independence Model */
title 'Model 9: Mutual Independence Model';
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol cursmoke cvd / dist = Poisson;
run;

/* Final Model: Mutual Independence */
title 'Final Model: Mutual Independence';
ods output ModelFit;
proc genmod data = loglinear;
class chol cursmoke cvd;
model Count = chol cursmoke cvd / dist = Poisson obstats type3;
output RESCHI=PearsonResiduals RESDEV=DevianceResiduals;
run;

/*******************************************************************/
/* Log Linear Models: Low Density Cholesterol, Smoking, and CVD */
/*******************************************************************/
/* Create Categorical Variable for Low Density Cholesterol */
/* https://www.hopkinsmedicine.org/health/treatment-tests-and-therapies/lipid-panel */

data frmgham;
set frmgham;
if LDLC = "." then LDLchol = .; /* Missing Data */
else if LDLC < 100 then LDLchol = 0; /* Optimal */
else if LDLC >= 100 & LDLC < 130 then LDLchol = 1; /* Near Optimal */
else if LDLC >= 130 & LDLC < 160 then LDLchol = 2; /* Borderline High */
else if LDLC >= 160 & LDLC < 190 then LDLchol = 3; /* High */
else if LDLC >= 190 then LDLchol = 4; /* Very High */
run;

/* Determine Counts for Variables of Interest */
proc sql;
select LDLchol, cursmoke, cvd, count(*) as total_count
from frmgham
group by LDLchol, cursmoke, cvd;
quit;

/* Create Dataset to Plot Odds of CVD vs. LDL Cholesterol by Current Smoking Status */
Title 'Dataset for Plot of Odds of CVD vs. LDL Cholesterol';
data plot;
input chol cursmoke $ 3-12 cvdOdds;
datalines;
0 Non-Smoker 0.22951
0 Smoker 0.40000
1 Non-Smoker 0.27922
1 Smoker 0.28571
2 Non-Smoker 0.22955
2 Smoker 0.33136
3 Non-Smoker 0.32258
3 Smoker 0.26147
4 Non-Smoker 0.35084
4 Smoker 0.33813
;
run;

/* Plot of Odds of CVD vs. LDL Cholesterol by Current Smoking Status */
title1 'Odds of CVD vs. LDL Cholesterol by Current Smoking Status';
symbol1 v=dot c=black;
symbol2 v=circle;
axis1 label=('LDL Cholesterol Category');
axis2 label=(a=90 'CVD Odds') minor=none ;
legend1 label=('Smoking Status');
proc gplot data = plot;
plot cvdOdds*chol = cursmoke/vaxis=axis2 haxis=axis1 legend=legend1;
run;
quit;

/* Create new dataset for log-linear models */
Title 'Dataset for Log Linear Models';
data LDLloglinear;
input LDLchol cursmoke cvd Count;
cLDLchol = LDLchol;
cLDLchol_sq = cLDLchol**2;
datalines;
0 0 0 61
0 0 1 14
0 1 0 20
0 1 1 8
1 0 0 154
1 0 1 43
1 1 0 105
1 1 1 30
2 0 0 379
2 0 1 87
2 1 0 169
2 1 1 56
3 0 0 403
3 0 1 130
3 1 0 218
3 1 1 57
4 0 0 533
4 0 1 187
4 1 0 278
4 1 1 94
;
run;

/* Model 1: Saturated Model */
title 'Model 1: Saturated Model';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cursmoke|cvd / dist = Poisson;
run;

/* Model 2: All Two Way Interactions Model */
title 'Model 2: All Two Way Interactions Model';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cursmoke LDLchol|cvd cursmoke|cvd / dist = Poisson;
run;

/* Model 3: Conditional Independence of Cholesterol and Smoking */
title 'Model 3: Conditional Independence of Cholesterol and Smoking';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cvd cursmoke|cvd / dist = Poisson;
run;

/* Model 4: Conditional Independence of Cholesterol and CVD */
title 'Model 4: Conditional Independence of Cholesterol and CVD';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cursmoke cursmoke|cvd / dist = Poisson;
run;

/* Model 5: Conditional Independence of Smoking and CVD */
title 'Model 5: Conditional Independence of Smoking and CVD';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cursmoke LDLchol|cvd / dist = Poisson;
run;

/* Model 6: Joint Independence of (Cholesterol and Smoking) from CVD */
title 'Model 6: Joint Independence of (Cholesterol and Smoking) from CVD';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cursmoke cvd / dist = Poisson;
run;

/* Model 7: Joint Independence of (Cholesterol and CVD) from Smoking */
title 'Model 7: Joint Independence of (Cholesterol and CVD) from Smoking';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol|cvd cursmoke / dist = Poisson;
run;

/* Model 8: Joint Independence of (Smoking and CVD) from Cholesterol */
title 'Model 8: Joint Independence of (Smoking and CVD) from Cholesterol';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = cursmoke|cvd LDLchol / dist = Poisson;
run;

/* Model 9: Mutual Independence Model */
title 'Model 9: Mutual Independence Model';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd / dist = Poisson;
run;

/* Final Model: Mutual Independence */
title 'Final Model: Mutual Independence';
ods output ModelFit;
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd / dist = Poisson obstats type3;
output RESCHI=PearsonResiduals RESDEV=DevianceResiduals;
run;

/* Models Treating Low Density Cholesterol as Continuous */
title 'Model 11';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke cLDLchol_sq*cursmoke cLDLchol*cvd cLDLchol_sq*cvd / dist = P
run;

title 'Model 12';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke cLDLchol_sq*cursmoke cLDLchol*cvd / dist = Poisson;
run;

title 'Model 13';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke cLDLchol*cvd cLDLchol_sq*cvd / dist = Poisson;
run;

title 'Model 14';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke cLDLchol*cvd / dist = Poisson;
run;

title 'Model 15';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke cLDLchol_sq*cursmoke / dist = Poisson;
run;

title 'Model 16';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cvd cLDLchol_sq*cvd / dist = Poisson;
run;

title 'Model 17';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cursmoke / dist = Poisson;
run;

title 'Model 18';
proc genmod data = LDLloglinear;
class LDLchol cursmoke cvd;
model Count = LDLchol cursmoke cvd cLDLchol*cvd / dist = Poisson;
run;