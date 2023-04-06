proc import out=dat
	datafile = '/home/elkip/Datasets/hw10.csv'
	dbms = CSV replace;
	getnames=yes;
run;

/*
data dat_long;
	length scale $10;
	set dat;
	Outcome=NACCBMI;scale='BMI';output;
	Outcome=ADNPV1;scale='AD';output;
	keep NACCID Outcome Scale ADNPV1 NACCAGE NACCBMI NACCDAGE SEX2 EDUC NONWHITE NE4S;
run;
*/

* Random intercept model for BMI;
proc mixed data=dat covtest method=ML;
class NACCID SEX2 NONWHITE NE4S;
model NACCBMI = NACCAGEB SEX2 EDUC NONWHITE NE4S NACCYRS/solution;
random Int/sub=NACCID type=un;
run;

* Random intercept model for binary outcome ADNP;

/*
proc glimmix data=dat;
class NACCID SEX2 NONWHITE NE4S;
model ADNPV1(event='1') = NACCDAGE SEX2 NONWHITE NE4S/dist=binomial link=logit s;
random Int/sub=NACCID;
run;
*/

proc logistic data=dat;
class NACCID SEX2 NONWHITE NE4S;
model ADNPV1(event='1') = NACCDAGE SEX2 NONWHITE NE4S;
run;

* Joint Model;
proc nlmixed data = dat;
parms b0 = 37.6885
	b_naccageb = -.1225 
	b_sex2 = -.7512 
	b_educ = -.03104 
	b_nonwhite = -1.3926
	b_ne4s = .4201 
	b_naccyrs = -.2433
	c0 = -5.3399 
	c_naccdage = 0.0584
	c_sex2 = -.00504
	c_nonwhite = .6551 
	c_ne4s = -.9334 
	variance_BMI = 1.8706 
	gll = 19.2500
	gamma = -.1;
if dist='Normal' then do;
	mean = b0 + b_naccageb*naccageb + b_sex2*sex2 + b_educ*educ +b_nonwhite*nonwhite +b_ne4s*ne4s + b_naccyrs*naccyrs +ul;
	ll = -0.5*log(3.14159265358) - log(sqrt(variance_BMI)) -0.5*(outcome-mean)**2/(variance_BMI);
end;
if dist='AD' then do;
	mean = c0+c_naccdage*naccdage + c_sex2*sex2 + c_nonwhite*nonwhite + c_ne4s*ne4s +ul*gamma;
	p=exp(mean)/(1+exp(mean));
	ll = (outcome)*log(p) + (1-outcome)*log(1-p);
end;
model outcome~general(ll);
random  ul~normal([0],[gll]) subject=naccid;
run;

* -.1 for gamma, scaling value no real meaning;
* gll is the effect of the random intercept BMI on the log odds of ad;
