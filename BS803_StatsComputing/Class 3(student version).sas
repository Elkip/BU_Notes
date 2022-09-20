libname Class1 'C:\Users\User\Desktop\Data';
/*If you are going to use a SAS PROC with a by statement, you must sort your data  beforehand*/
proc print data=class1.fhs;
run;
proc sort data=Class1.FHS;
by randid;
run;
proc print data=class1.fhs;
run;
proc sort data=Class1.FHS;
by randid period;
run;
proc sort data=Class1.FHS;
by descending sysbp;
run;
proc print data=class1.fhs;
run;
/*Create a dataset that transposes the data from long to wide format*/
proc transpose data=Class1.FHS out=CLASS1.SBP_WIDE prefix=SBP;
    by randid ;
    id period;
    var SYSBP;
run;
proc transpose data=Class1.FHS out=CLASS1.DBP_WIDE prefix=DBP;
    by randid ;
    id period;
    var DIABP;
run;
proc transpose data=Class1.FHS out=CLASS1.AGE_WIDE prefix=AGE;
    by randid ;
    id period;
    var AGE;
run;
data FHS;
set Class1.FHS;
by randid;
counter+1;
if first.randid then counter=1;
run;
*In class exercise: Read the excel file CD4 into a SAS dataset. This includes multiple measurements of CD4 counts for a number of subjects 
in a clinical trial that had 4 arms. Create a wide dataset for the CD4 counts;

proc import out=class1.cd4
datafile="C:\Users\User\Desktop\Data\CD4.xls"
dbmx=xls;
getname=YES;
run;

proc sort data=class1.cd4;
by id week;
run;

data CD4;
set class1.cd4;
by id;
period + 1;
if first.id then period = 1;
run;

proc transpose data=cd4 out=class1.CD4_age_wide prefix=AGE;
	by id;
	id period;
	var AGE;
run;

proc transpose data=cd4 out=class1.CD4_cd_wide prefix=AGE;
	by id;
	id period;
	var log_cd4;
run;


/*Recoding missing values (we will use this later!)*/
data CLASS1.DBP_WIDE;
set CLASS1.DBP_WIDE;
if DBP1=. then DBP1=-99;
if DBP2=. then DBP2=-99;
if DBP3=. then DBP3=-99;
drop _NAME_;
run;
data CLASS1.SBP_WIDE;
set CLASS1.SBP_WIDE;
if SBP1=. then SBP1=-999;
if SBP2=. then SBP2=-999;
if SBP3=. then SBP3=-999;
drop _NAME_;
run;
data CLASS1.AGE_WIDE;
set CLASS1.AGE_WIDE;
if AGE1=. then AGE1=-99;
if AGE2=. then AGE2=-99;
if AGE3=. then AGE3=-99;
drop _NAME_;
run;
/*Create demographics dataset for FHS - keep the first observation*/
data Class1.Demographics;
set Class1.FHS;
by randid;
if first.randid then output;
rename age=age_bl;
label age='Age at baseline';
keep randid sex age ;
run;

/*Merge wide datasets*/
data Class1.FHS_wide;
merge Class1.demographics Class1.DBP_wide Class1.SBP_Wide Class1.AGE_Wide;
by randid;
run;
data FHS_wide;
set Class1.FHS_wide;
if age1=-99 then age1=.;
if age2=-99 then age1=.;
if age3=-99 then age1=.;

if dbp1=-99 then dbp1=.;
if dbp2=-99 then dbp1=.;
if dbp3=-99 then dbp1=.;

if sbp1=-999 then sbp1=.;
if sbp2=-999 then sbp1=.;
if sbp3=-999 then sbp1=.;

mbp1=(sbp1+dbp1)/2;
mbp2=(sbp2+dbp2)/2;
mbp3=(sbp3+dbp3)/2;
run;
/*  Replacing missing values- Using Arrays*/
data FHS_wide;
set Class1.FHS_wide;
array miss2{6} age1 age2 age3 dbp1 dbp2 dbp3;
array miss3{3} sbp1 sbp2 sbp3;
do i=1 to 6;
if miss2{i} = -99 then miss2{i} = .;
end;
do i=1 to 3;
if miss3{i} = -999 then miss3{i}=.;
end;
run;
*In class exercise1;


/*Class exercise: Calculate the mean of blood pressure using an array*/ 


/*Wide to Long using data step*/

data FHS_long;
set FHS_wide;
SBP=SBP1;DBP=DBP1;AGE=AGE1;MBP=MBP1;time=1;output;
SBP=SBP2;DBP=DBP2;AGE=AGE2;MBP=MBP2;time=2;output;
SBP=SBP3;DBP=DBP3;AGE=AGE3;MBP=MBP3;time=3;output;
drop SBP1-SBP3 DBP1-DBP3 AGE1-AGE3 MBP1-MBP3 i;
run;
/*Class exercise: Create a long from wide using arrays and apply it in the tlc dataset. This is measuring lead level in the blood
in a two arms clinical trial*/

*SOLUTION 1;

/*Deleting observation with missing values and create time since baseline*/
data FHS_long;
set FHS_long;
if SBP=. and DBP=. and AGE=. then delete;
Time_since_bl=age-age_bl;
run;
/* Saving particular parts of the output*/
proc means data=FHS_long noprint;
by randid;
var SBP DBP;
output out=FHS_minmax min = SBPmin DBPmin max=SBPmax SBPmax;
run;

/*Using trace on to save particular output tables*/
ods trace on;
ods listing;
proc reg data=FHS_wide;
model SBP1 = age1;
Run;
quit;

ods output ParameterEstimates=Class1.Estimates;
proc reg data=FHS_wide;
model SBP1 = age1;
Run;
quit;

%macro regdiag(indat,pred,outcme);
proc reg data=&indat;
model &outcme = &pred;
Run;
quit;
%mend regdiag;



%regdiag(FHS_wide,Age1,SBP1);
%regdiag(FHS_wide,Age2,DBP2);



