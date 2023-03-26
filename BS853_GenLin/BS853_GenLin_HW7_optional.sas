/*Exercise 2*/
proc format ;
  value vol 1='High' 0='Low';  
run;

data thr;  
input  year highvol satisf count; 
format highvol vol.;
datalines;  
1         0         1        84
1         0         2       231
1         0         3        99
1         1         1       473
1         1         2       493
1         1         3        93
2         0         1       150
2         0         2       347
2         0         3       117
2         1         1       332
2         1         2       387
2         1         3        62
3         0         1       257
3         0         2       889
3         0         3       234
3         1         1       571
3         1         2       793
3         1         3       112
; 
run;

/*a. Use the SAS statements below to fit a cumulative logits model to the data.*/
title1 "Cumulative Logits models for THR using the response statement";
title2 "all slopes independent of the level of satisfaction";
proc catmod data =thr;
	weight count ;
	response *  1 0 -1 0,
				0 1 0 -1 log * 1 0 0,
								1 1 0,
								0 1 1,
								0 0 1;
	model satisf = _response_ highvol year / wls;
run;
quit;

/*Intercept   0.5667 0.0288 385.96 <.0001 */
/*_RESPONSE_ 1 -1.3951 0.0226 3801.36 <.0001 */
/*highvol Low -0.4960 0.0277 321.11 <.0001 */
/*year 1 0.00499 0.0401 0.02 0.9012 */
/*     2 0.0582 0.0405 2.07 0.1500 */

/*Explanation:*/
/*log(pi_1/(pi_2+pi_3)) = alpha1 + beta1*LowVol + beta2*Year1 + beta3*Year2 */
/*log((pi_1+pi_2)/pi_3) = alpha2 + beta1*LowVol + beta2*Year1 + beta3*Year2 */
/*alpha1 = 0.5667 + 1*(-1.3951) = -0.8284*/
/*alpha2 = 0.5667 + (-1)*(-1.3951) = 1.9618*/
/*beta1 = -0.4960*/
/*beta2 = 0.00499*/
/*beta3 = 0.0582*/



/*b. Fit the same cumulative logit model to the data using PROC LOGISTIC. Compare the results. Note*/
/*that the results can differ slightly as the methods of estimation used by the two procedures are*/
/*different – PROC CATMOD is using the method of Weighted Least Squares while PROC LOGISTIC is*/
/*using the method of Maximum Likelihood.*/
title1 "cumulative Logits models for THR using Proc Logistic";
proc logistic data = THR;
	class highvol(ref = "High") year / param = effect;
	model satisf = highvol year / link = clogit;
	freq count;
run; 

/*Intercept 1 1 -0.8317 0.0313 706.5317 <.0001 */
/*Intercept 2 1 1.9763 0.0418 2239.0941 <.0001 */
/*highvol Low 1 -0.4965 0.0276 322.9185 <.0001 */
/*year 1 1 0.00790 0.0405 0.0379 0.8456 */
/*year 2 1 0.0558 0.0408 1.8734 0.1711 */

/*Explanation:*/
/*log(pi_1/(pi_2+pi_3)) = alpha1 + beta1*LowVol + beta2*Year1 + beta3*Year2 */
/*log((pi_1+pi_2)/pi_3) = alpha2 + beta1*LowVol + beta2*Year1 + beta3*Year2 */
/*alpha1 = -0.8317*/
/*alpha2 = 1.9763*/
/*beta1 = -0.4965*/
/*beta2 = 0.00790*/
/*beta3 = 0.0558*/

/*Explain what are the values for alpha1, alpha2, beta1, beta2, and beta3.
  Compare the results, and briefly comment on why there is the difference.*/


/*c. The next SAS statements fit a cumulative logits model to the data allowing the slopes to depend on*/
/*the level of satisfaction.*/
title2 "all slopes dependent of the level of satisfaction";
title3 "Get a test for the proportional odds assumption by looking at difference in residual statistics";
proc catmod data =thr;
	weight count ;
	response *  1 0 -1 0,
				0 1 0 -1 log * 1 0 0,
								1 1 0,
								0 1 1,
								0 0 1;
	model satisf = highvol year / wls;
run;
quit;

/*Intercept   1 -0.8153 0.0321 643.38 <.0001 */
/*            2 1.9203 0.0430 1998.91 <.0001 */
/*highvol Low 1 -0.4952 0.0313 250.63 <.0001 */
/*        Low 2 -0.5043 0.0418 145.77 <.0001 */
/*year 1 1 0.0689 0.0442 2.43 0.1191 */
/*     1 2 -0.1599 0.0628 6.49 0.0109 */
/*     2 1 0.0779 0.0447 3.03 0.0818 */
/*     2 2 0.0195 0.0631 0.10 0.7568 */


/*d. Argue that the difference between the goodness of fit statistics provided for the two models above*/
/*can serve as a test for the proportional odds assumption. How many degrees of freedom will have*/
/*the distribution of the test statistic?*/

/*Model 1: Assuming Proportional Odds*/
/*log(pi_1/(pi_2+pi_3)) = alpha1 + beta1*LowVol + beta2*Year1 + beta3*Year2 */
/*log((pi_1+pi_2)/pi_3) = alpha2 + beta1*LowVol + beta2*Year1 + beta3*Year2 */

/*Model 2: Allowing slopes to depend on the level of satisfaction*/
/*log(pi_1/(pi_2+pi_3)) = alpha1 + beta11*LowVol + beta12*Year1 + beta13*Year2 */
/*log((pi_1+pi_2)/pi_3) = alpha2 + beta21*LowVol + beta22*Year1 + beta23*Year2 */

/*H0: Model 2 provides adequate fit compared with Model 1 (Proportional Odds Assumption Hold). */
/*Difference in residual chi-square = 31.28 - 4.94, with DF = difference in number of parameters of the two models above = 3, so p-value < 0.0001*/


/*e. Compare your test results with the test results for the proportional odds assumption provided to you*/
/*by PROC LOGISTIC.*/
title1 "cumulative Logits models for THR using Proc Logistic";
proc logistic data = THR;
	class highvol(ref = "High") year / param = effect;
	model satisf = highvol year / link = clogit;
	freq count;
run; 
/*Score Test for the Proportional Odds Assumption */
/*Chi-Square DF Pr > ChiSq */
/*26.5204 3 <.0001 */



/*Exercise 3*/
data ph;
do Type = 1 to 3;
	input YearsLT30 Count @@;
	output;
end;
cards;
1 218 1 13 1 12
2  71 2 25 2 32
run;

/*1. Cumulative Logit Model*/
title1 "Cumulative Logit Model";
proc catmod data = ph;
	weight count ;
	response *  1 0 -1 0,
				0 1 0 -1 log * 1 0 0,
								1 1 0,
								0 1 1,
								0 0 1;
	model Type = _response_ YearsLT30 / wls;
run;
quit;
/*exp(2*0.9670) = 6.917*/

title1 "Cumulative Logit Model";
proc logistic data = ph;
	class YearsLT30 / param = effect;
	model Type = YearsLT30 / link = clogit;
	freq count;
run;
/*exp(2*0.9670) = 6.917*/
/*https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-the-coefficients-of-an-effect-coded-variable-involved-in-an-interaction-in-a-regression-model/*/


/*2. Adjacent Logit Model*/
title1 "Adjacent Logit Model";
proc catmod data = ph;
	weight count ;
	response *  1 -1 0,
				0 1 -1   log * 1 0 0,
								0 1 0,
								0 0 1;
	model Type = _response_ YearsLT30 / wls;
run;
quit;
/*exp(2*0.5835) = 3.212*/


/*3. Continuation Ratio Logit Model*/
title1 "Continuation Ratio Logit Model";
proc catmod data = ph;
	weight count ;
	response *  1 -1 0 0,
				0 0 1 -1   log * 1 0 0,
								  0 1 1,
								  0 1 0,
 								  0 0 1;
	model Type = _response_ YearsLT30 / wls;
run;
quit;
/*exp(2*0.7725) = 4.688*/

/*Interpretation: */
/*(1) For cumulative logits model: OR = 6.197 is odds ratio of subject in none category vs. in mild or severe category, when comparing Year <= 30 vs. Year > 30.*/
/*    It is also the odds ratio of subjects in none or mild category vs. severe category, when comparing Year <= 30 vs. Year > 30, because the parameter estimate is the same in both link functions. */
/*    So we can summarize it in a more general way, for example, the odds ratio of subjects in less severe categories vs. more severe categories is 6.197, when comparing Year <= 30 vs. Year > 30.*/

/*(2) For adjacent logits, the odds ratio of subjects in current level of severity vs. one level increase of severity is 3.212, when comparing Year <= 30 vs. Year > 30.*/

/*(3) For continuation ratio logits, the odds ratio of subjects with current level of severity vs. in higher level of severity is 4.688, when comparing Year <= 30 vs. Year > 30.*/

/*In general, all the 3 models give us the similar message, odds ratio of subjects with low level of severity is xxx, when comparing subjects with years working in mine <= 30 vs. > 30. */
