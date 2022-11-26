*****************
* BS 728        *
* Homework 1    *
* NAME          *
*****************;

libname dhs "C:/Users/elkip/Desktop/data/";

data dhs_india;
set dhs.Hw1datasmall;
run;

******************************************************
* Run analysis and do not adjust for sampling design *
******************************************************;
* Overall summaries;
proc means data=dhs_india;
var sex age bmi cookingfuel windows;
run;

* Summaries by TB status;


proc freq data=dhs_india;
tables TB*(age_cat)/chisq;
run;

proc freq data=dhs_india;
tables TB*(bmi_cat)/chisq;
run;

proc means data=dhs_india;
class TB;
var sex age bmi windows;
run;

proc ttest data=dhs_india;
class TB;
var sex age_cat bmi cookingfuel windows;
run;

* multivariate model;
proc logistic data=dhs_india descending;
class tb gender age_cat windows(ref='0') wood_fuel(ref='0')/param=ref;
model tb=gender age_cat wood_fuel windows;
run;

**************************************************
* Run analysis and adjusting for sampling design *
**************************************************;
proc surveyfreq data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
tables sex age_cat bmi_cat cookingfuel windows;
run;

proc surveymeans data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
domain TB;
var age bmi;
run;

proc surveyfreq data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
tables TB*(sex age_cat bmi_cat cookingfuel windows)/chisq;
run;

proc surveyreg data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
class TB;
model TB=age bmi;
run;

proc surveylogistic data=dhs_india;
class tb(ref='0') gender age_cat bmi_cat windows(ref='0') wood_fuel(ref='0')/param=ref;
model tb=gender age age_cat bmi bmi_cat wood_fuel windows;
weight wgtdhs;
stratum hv022;
cluster sh021;
run;

