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
*Random Mixed Effects Models;
*Random intercept;
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
where session='SCHEDULED';
run;quit;

*Random time and intercept;
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept time/type=un subject=id;
where session='SCHEDULED';
run;quit;

data lr;
lr = 5499.1 - 5425.3;
pval = 1 - probchi(lr,5);
run;

proc print data=lr;
run;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept age/type=un subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept wabaq /type=un subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept accuracy /type=un subject=id;
where session='SCHEDULED';
run;quit;

*Random intercept time and accuracy;
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept time accuracy/type=un subject=id;
where session='SCHEDULED';
run;quit;

data lr;
lr = 5499.1 - 5419.6;
pval = 1 - probchi(lr,1);
run;

proc print data=lr;
run;


*Sqaured time;
proc mixed data=rhyme method=REML;
class id;
model latency=time tsq age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
where session='SCHEDULED';
run;quit;

*Cubed time;
proc mixed data=rhyme method=REML;
class id;
model latency=time tsq tcb age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
where session='SCHEDULED';
run;quit;

*No random effects with different covariance structure;
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
where session='SCHEDULED';
run;quit;

*The below covariance structures are too compuationally expensive;
/*
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=un subject=id;
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
repeated /type=arh(1) subject=id;
where session='SCHEDULED';
run;quit;
*/

/*
2. Is there improvement in avg latency over time when the patients use an ipad in a clinic visit?
	- Assisted visits only
	- Adjust for age language ability and average accuracy
	- Treat time as continous
	- Compare appropraite models for time (linear, qaudratic, etc.)
	- Compare covariance structure
*/
* Add number of unassited practices;
data practice;
	set rhyme;
	by id time;
	if session='SCHEDULED' then scheduledNum + 1;
	if session='ASSISTED' then do;
		output;
		scheduledNum = 0;
	end;
run;

*Random Intercept;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
ods select fitstatistics;
run;quit;

*Random time and intercept;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept time/type=un subject=id;
ods select fitstatistics;
run;quit;

*Random intercept and age;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept age/type=un subject=id;
ods select fitstatistics;
run;quit;

*Random intercept and wabaq;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept wabaq /type=un subject=id;
ods select fitstatistics;
run;quit;

data lr;
lr = 1067.1 - 1057.6;
pval = 1 - probchi(lr,5);
run;

proc print data=lr;
run;

*Random intercept and accuracy;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept accuracy /type=un subject=id;
run;quit;

data lr;
lr = 1067.1 - 1056.2;
pval = 1 - probchi(lr,5);
run;

proc print data=lr;
run;

*Random intercept time and accuracy;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
random intercept wabaq accuracy/type=un subject=id;
ods select fitstatistics;
run;quit;

*Sqaured time;
proc mixed data=practice method=REML;
class id;
model latency=time tsq age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
ods select fitstatistics;
run;quit;

proc mixed data=practice method=REML;
class id;
model latency=time tsq age wabaq accuracy/s chisq;
random intercept accuracy/type=un subject=id;
ods select fitstatistics;
run;quit;

proc mixed data=practice method=REML;
class id;
model latency=time tsq age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
ods select fitstatistics;
run;quit;

*Cubed time;
proc mixed data=practice method=REML;
class id;
model latency=time tsq tcb age wabaq accuracy/s chisq;
random intercept/type=un subject=id;
ods select fitstatistics;
run;quit;

proc mixed data=practice method=REML;
class id;
model latency=time tsq tcb age wabaq accuracy/s chisq;
random intercept accuracy/type=un subject=id;
ods select fitstatistics;
run;quit;

*No random effects with different covariance structure;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
ods select fitstatistics;
run;quit;

proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
ods select fitstatistics;
run;quit;

*Adding number of scheduled practices as a variable;
proc mixed data=practice method=REML;
class id;
model latency=time age wabaq accuracy scheduledNum/s chisq;
random intercept accuracy/type=un subject=id;
run;quit;

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
	if accuracy >= &C then accurate=1;
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
	visitNum + 1;
	by id;
	if first.id then visitNum = 1;
run;

*This gives a number of averages per each ID and day;
data assisted;
	set assisted;
	by id time;
	assistNum + 1;
	avgLat + latency;
	avgAcc + accuracy;
	if first.id then do;
		avgLat = latency;
		avgAcc = accuracy;
		assistNum = 1;
	end;
	if first.time then do;
		avgLat = latency;
		avgAcc = accuracy;
		assistNum = 1;
	end;
	if last.time then do;
		avgLat = avgLat / assistNum;
		avgAcc = avgAcc / assistNum;
		output;
	end;
	drop latency accuracy;
run;

data assisted;
	set assisted;
	accurate=0;
	if avgAcc >= &C then accurate=1;
run;

proc genmod data=assisted;
class id;
model accurate(event='1')=time age wabaq/dist=bin link=logit type3 wald;
repeated subject=id;
run;quit;

proc genmod data=assisted;
class id;
model accurate(event='1')=time age wabaq assistNum/dist=bin link=logit type3 wald;
repeated subject=id;
run;quit;

proc genmod data=assisted;
class id;
model accurate(event='1')=time age wabaq assistNum assistNum*time/dist=bin link=logit type3 wald;
repeated subject=id;
run;quit;

/*
3. Use your continous variable that indicates how many times the participant has practiced on 
the ipad since the last assisted visit and model to determine whether practicing improves performance. 
	- Adjust for age and WABAQ
*/
data practice2;
	set rhyme;
	by id time;
	if session='SCHEDULED' then do;
		if last.time and assistedNum > 0 then do;
			avgLat = avgLat / assistedNum;
			avgAcc = avgAcc / assistedNum;
			output;
			scheduledNum = 0;
			assistedNum = 0;
		end;
		if first.id or first.time then do;
			avgLat = 0;
			avgAcc = 0;
			assistedNum = 0;
			scheduledNum + 1;
		end;
		else do;
			scheduledNum + 1;
		end;
	end;
	if session='ASSISTED' then do;
		assistedNum + 1;
		avgLat + latency;
		avgAcc + accuracy;
		if first.id or first.time then do;
			avgLat = latency;
			avgAcc = accuracy;
			assistedNum = 1;
		end;
		if last.time then do;
			avgLat = avgLat / assistedNum;
			avgAcc = avgAcc / assistedNum;
			output;
			scheduledNum = 0;
			assistedNum = 0;
		end;
	end;
	drop latency accuracy session;
run;

data practice2;
	set practice2;
	accurate=0;
	if avgAcc >= &C then accurate=1;
run;

proc genmod data=practice2;
class id;
model accurate(event='1')=time age wabaq assistedNum/dist=bin link=logit type3 wald;
repeated subject=id;
run;quit;

proc genmod data=practice2;
class id;
model accurate(event='1')=time age wabaq assistedNum assistedNum*time/dist=bin link=logit type3 wald;
repeated subject=id;
run;quit;

proc genmod data=practice2;
class id;
model accurate(event='1')=time age wabaq assistedNum scheduledNum/dist=bin link=logit type3 wald;
repeated subject=id / logor=fullclust;
run;quit;
