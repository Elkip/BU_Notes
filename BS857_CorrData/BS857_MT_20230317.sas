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
*Regular time;
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

*Sqaured time;
proc mixed data=rhyme method=REML;
class id;
model latency=tsq age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=tsq age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
where session='SCHEDULED';
run;quit;

*Cubed time;
proc mixed data=rhyme method=REML;
class id;
model latency=tcb age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='SCHEDULED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=tcb age wabaq accuracy/s chisq;
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
*Regular time;
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='ASSISTED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
where session='ASSISTED';
run;quit;

*Sqaured time;
proc mixed data=rhyme method=REML;
class id;
model latency=tsq age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='ASSISTED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=tsq age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
where session='ASSISTED';
run;quit;

*Cubed time;
proc mixed data=rhyme method=REML;
class id;
model latency=tcb age wabaq accuracy/s chisq;
repeated /type=cs subject=id;
where session='ASSISTED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=tcb age wabaq accuracy/s chisq;
repeated /type=ar(1) subject=id;
where session='ASSISTED';
run;quit;

*The below covariance structures are too compuationally expensive;
/*
proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=un subject=id;
where session='ASSISTED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=csh subject=id;
where session='ASSISTED';
run;quit;

proc mixed data=rhyme method=REML;
class id;
model latency=time age wabaq accuracy;
repeated /type=arh(1) subject=id;
where session='ASSISTED';
run;quit;
*/

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
data practice;
	set assisted;
	by id time;
	pracNum + 1;
	avgLat + latency;
	avgAcc + accuracy;
	if first.id then do;
		avgLat = latency;
		avgAcc = accuracy;
		pracNum = 1;
	end;
	if first.time then do;
		avgLat = latency;
		avgAcc = accuracy;
		pracNum = 1;
	end;
	if last.time then do;
		avgLat = avgLat / pracNum;
		avgAcc = avgAcc / pracNum;
		output;
	end;
	drop latency accuracy;
run;

data practice;
	set practice;
	accurate=0;
	if avgAcc >= &C then accurate=1;
run;

*Alternative to get average per day;
/*
proc sql;
	create table practice as
	select id, avg(accuracy) as accuracy, avg(latency) as latency, avg(time) as time,
		avg(WABAQ) as wabaq, avg(age) as age, avg(severity) as severity, max(visitNum) as pracNum
		from assisted
		group by id, time;
	quit;
*/

proc genmod data=practice;
class id;
model accurate(event='1')=time age wabaq/dist=bin link=logit type3 wald;
repeated subject=id/type=cs;
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
		scheduledNum + 1;
		avgLat = latency;
		avgAcc = accuracy;
		assistedNum = 1;
	end;
	if session='ASSISTED' then do;
		assistedNum + 1;
		avgLat + latency;
		avgAcc + accuracy;
		if first.id then do;
			avgLat = latency;
			avgAcc = accuracy;
			assistedNum = 1;
		end;
		if first.time then do;
			avgLat = latency;
			avgAcc = accuracy;
			assistedNum = 1;
		end;
		if last.time then do;
			avgLat = avgLat / assistedNum;
			avgAcc = avgAcc / assistedNum;
			output;
			scheduledNum = 0;
		end;
	end;
	drop latency accuracy session;
run;

data practice;
	set practice;
	accurate=0;
	if avgAcc >= &C then accurate=1;
run;


proc genmod data=practice;
class id;
model accurate(event='1')=time age wabaq pracNum time*pracNum/dist=bin link=logit type3 wald;
repeated subject=id/type=cs;
run;quit;
