libname MT 'Z:\';

data rhyme;
	set MT.rhyming;
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



/*
2. Is there improvement in avg latency over time when the patients use an ipad in a clinic visit?
	- Assisted visits only
	- Adjust for age language ability and average accuracy
	- Treat time as continous
	- Compare appropraite models for time (linear, qaudratic, etc.)
	- Compare covariance structure
*/
*Part 2: Modeling Average Accuracy;
/*
1. Define a new dichotomous variable for accuracy defined as:
	Accurate = 1 if AvgAccuracy > some constant; else Accuarate = 0

	Analyze the average accuracy on this variable.
*/

/*
2. Run a model for determining whether there is imporvement in accuracy over time 
in clinic exams only at population level. 
	- Adjust for age and WABAQ.
	- Use an appropriate method to account for any correlation of measurements over time
Note: The observations are not equally spaced and there may be estimation probems.
*/

/*
3. Use a continous variable that indicates how many times the participant has practiced on 
the ipad since the last assisted visitan appropriate model to determine whether practicing 
improves performance. 
	- Adjust for age and WABAQ
*/
