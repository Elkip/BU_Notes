libname HW9 '/home/elkip/Datasets/';

data birth;
	set HW9.birthwt_missing;
run;

* 1a. Mixed effects model with a linear trend for
	maternal age and randomly varying intercepts;
proc mixed data=birth;
	class mid;
	model weight=age /s;
	random intercept/type=un subject=mid;
run;

* 1b. Run a multiple impuation using a monotone 
	regression with weight as a function of age;
proc mi data=birth seed=2022 nimpute=15 out=miBirth;
	var age weight;
	monotone regression(weight=age);
run;

proc mixed data=miBirth;
	class mid;
	model weight=age /s;
	random intercept/type=un subject=mid;
	by _IMPUTATION_;
	ods output solutionf=beta covb=varbeta;
run;

proc mianalyze parms=beta;
	modeleffects intercept age;
run;

* 1c. Rerun the imputation again with m=5 and m=30;
proc mi data=birth seed=2022 nimpute=5 out=miBirth5;
	var age weight;
	monotone regression(weight=age);
run;

proc mixed data=miBirth5;
	class mid;
	model weight=age /s;
	random intercept/type=un subject=mid;
	by _IMPUTATION_;
	ods output solutionf=beta5 covb=varbeta5;
run;

proc mianalyze parms=beta5;
	modeleffects intercept age;
run;


proc mi data=birth seed=2022 nimpute=30 out=miBirth30;
	var age weight;
	monotone regression(weight=age);
run;

proc mixed data=miBirth30;
	class mid;
	model weight=age /s;
	random intercept/type=un subject=mid;
	by _IMPUTATION_;
	ods output solutionf=beta30 covb=varbeta30;
run;

proc mianalyze parms=beta30;
	modeleffects intercept age;
run;

* 1d. Run a multiple imputation with m=15 using MCMC;
proc mi data=Birth seed=2022 nimpute=15 out=Birth_MCMC;
	var age weight;
	mcmc chain=multiple displayinit initial=em(itprint);
run;

proc mixed data=Birth_MCMC;
	model weight=age/s covb;
	random intercept/type=un subject=mid;
	by _IMPUTATION_;
	ods output solutionf=beta_mcmc covb=varbeta_mcmc;
run;

proc mianalyze parms=beta_mcmc; 
	modeleffects intercept age;
run;

* 2. Calculate sample size for a two group trial, AR(1) structure with 
	 ρ = .4,  α = .01 and sample size per group so we have at least 90% power
	 to detect an increasing average group difference of .2 per observation time;
PROC IML;
za = PROBIT(.995);
zb = PROBIT(.9);
meandiff = {0, .2, .4, .6};
contrast = {-3, -1, 1, 1};
corrmat = {1 .4 .16 .064 ,
			.4 1 .4 .16,
			.16 .4 1 .4,
			.064 .16 .4 1 };
contdiff = T(contrast) * meandiff;
contvar = T(contrast)*corrmat*contrast;
NperGrp = ((2*(za+zb)**2) * contvar)/(contdiff**2);
PRINT NperGrp;
run;