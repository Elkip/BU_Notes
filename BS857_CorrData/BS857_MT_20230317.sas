libname MT 'Z:\';

data rhyme;
	set MT.rhyming(rename=(PatientID=id days_past_first_use=time SessionType=session 
		AvgAccuracy=accuracy AvgLatency=latency Composite_Severity1=severity));
	label id='ID' time='Time' session='Session' accuracy='Accuracy' latency='Latency' 
		severity='Severity';
	t=time;
	tsq=time**2;
	tcb=time**3;
run;

*Part 1: Modeling Average Latency;
/*
1. Is there improvement in avg latency over time when the patients use an ipad?
	- Scheduled visits only
	- Adjust for age language ability and average accuracy
	- Treat time as continous
	- Compare appropraite models for time (linear, qaudratic, etc.)
	- Compare covariance structure
*/
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=un subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=cs subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=csh subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=ar(1) subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=arh(1) subject=id;
where session='SCHEDULED';
run;quit;

/*
2. Is there improvement in avg latency over time when the patients use an ipad in a clinic visit?
	- Assisted visits only
	- Adjust for age language ability and average accuracy
	- Treat time as continous
	- Compare appropraite models for time (linear, qaudratic, etc.)
	- Compare covariance structure
*/
proc mixed data=rhyme method=REML;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=un subject=id G V;
where session='ASSISTED';
run;

proc genmod data=rhyme method=REML;
class id t/param=ref;
model latency=time age wabaq accuracy;
repeated subject=id/withinsubject=t;
where session='ASSISTED';
run;

proc mixed data=rhyme method=REML;
model latency=tsq age wabaq accuracy/s chisq;
random intercept/type=un subject=id G V;
where session='ASSISTED';
run;

proc mixed data=rhyme method=REML;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=arh(1) subject=id G V;
where session='ASSISTED';
run;

proc mixed data=rhyme method=REML;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=ar(1) subject=id G V;
where session='ASSISTED';
run;

proc mixed data=rhyme method=REML;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=cs subject=id G V;
where session='ASSISTED';
run;

proc mixed data=rhyme method=REML;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=csh subject=id G V;
where session='ASSISTED';
run;

*Part 2: Modeling Average Accuracy;
/*
1. Define a new dichotomous variable for accuracy defined as:
	Accurate = 1 if AvgAccuracy > some constant; else Accuarate = 0

	Analyze the average accuracy on this variable.
*/
proc means data=rhyme mean std median;
var accuracy;
run;

proc univariate data=rhyme;
var accuracy;
histogram accuracy;
run;

%let C=.7;

data rhyme;
	set rhyme;
	accurate=0;
	if accuracy > &C then accurate=1;
run;

proc means data=rhyme;
var accurate;
run;

/*
2. Run a model for determining whether there is imporvement in accuracy over time 
in clinic exams only at population level. 
	- Adjust for age and WABAQ.
	- Use an appropriate method to account for any correlation of measurements over time
Note: The observations are not equally spaced and there may be estimation probems.
*/

data assisted;
	set rhyme;
	where session='ASSISTED';
run;

data assisted;
	set assisted;
	by id;
	visitNum + 1;
	if first.id then visitNum = 1;
run;

data practice;
	set assisted;
	by id time;
	if first.id then do;
		output;
		pracNum = 0;
	end;
	pracNum + 1;
run;

data practice;
	set assisted;
	by id time;
	if first.id then pracNum = 0;
	if visitNum = 0 then do;
		output;
		pracNum = 0;
	end;
	else pracNum + 1;
run;

proc genmod data=rhyme;
class id week;
model accurate(event='1')=week age wabaq/dist=bin link=logit type3 wald;
repeated subject=id/withinsubject=week;

run;quit;


/*
3. Use a continous variable that indicates how many times the participant has practiced on 
the ipad since the last assisted visitan appropriate model to determine whether practicing 
improves performance. 
	- Adjust for age and WABAQ
*/
