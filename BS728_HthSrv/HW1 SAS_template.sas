*****************
* BS 728        *
* Homework 1    *
* NAME          *
*****************;

libname dhs "<location of DHS sas dataset on your computer";

data dhs_india;
set dhs.homework1data;
run;

******************************************************
* Run analysis and do not adjust for sampling design *
******************************************************;
* Overall summaries;
proc freq data=dhs_india;
tables /*<variable names here>*/;
run;

proc means data=dhs_india;
var /*<variable names here>*/;
run;

* Summaries by TB status;
proc freq data=dhs_india;
tables TB*(/*<variable names here>*/)/chisq;
run;

proc means data=dhs_india;
class /*<variable name here>*/;
var /*<variable names here>*/;
run;

proc ttest data=dhs_india;
class T/*<variable name here>*/;
var /*<variable names here>*/;
run;

* multivariate model;
proc logistic data=dhs_india descending;
class tb gender age_cat windows(ref='0') wood_fuel(ref='0')/param=ref;
model tb=/*<variable names here>*/;
run;

**************************************************
* Run analysis and adjusting for sampling design *
**************************************************;
proc surveyfreq data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
tables /*<variable names here>*/;
run;

proc surveymeans data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
var /*<variable names here>*/;
run;

proc surveyfreq data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
tables TB*(/*<variable names here>*/)/chisq;
run;

proc surveyreg data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
class TB;
model /*<continuous variable name here>*/=TB;
run;

proc surveyreg data=dhs_india;
weight wgtdhs;
stratum hv022;
cluster sh021;
class TB;
model /*<continuous variable name here>*/=TB;
run;

proc surveylogistic data=dhs_india2;
class tb(ref='0') gender age_cat windows(ref='0') wood_fuel(ref='0')/param=ref;
model tb=/*<variable names here>*/;
weight wgtdhs;
stratum hv022;
cluster sh021;
run;

