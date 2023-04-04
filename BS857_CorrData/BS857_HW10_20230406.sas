proc import out=dat
	datafile = '/home/elkip/Datasets/naccbmi_28jun16_Noformatting.csv'
	dbms = CSV replace;
	getnames=yes;
	DATAROW=2;
run;

data dat_long;
	length scale $10;
	set dat;
	Outcome=NACCBMI;scale='BMI';output;
	Outcome=ADNPV1;scale='AD';output;
	keep NACCID Outcome Scale ADNPV1 NACCAGE NACCBMI NACCDAGE SEX2 EDUC NONWHITE NE4S;
run;

* Random intercept models for longitudinal outcome BMI and binary outcome ADNP;
proc mixed data=dat covtest method=ML;
class NACCID SEX2 NONWHITE NE4S;
model NACCBMI = NACCAGE SEX2 EDUC NONWHITE NE4S/solution;
random Int/sub=NACCID;
run;

/*
proc glimmix data=dat;
class NACCID SEX2 NONWHITE NE4S;
model ADNPV1(event='1') = NACCDAGE SEX2 NONWHITE NE4S/dist=binomial link=logit s;
random Int/sub=NACCID;
run;
*/

proc logistic data=dat;
class NACCID SEX2 NONWHITE NE4S;
model ADNPV1(event='1') = NACCDAGE SEX2 NONWHITE NE4S/s;
run;

*-.1 for gamma;
* Joint Model;
proc nlmixed data=dat_long;
parms b0=32.38 b1=-.067 b2=-.6585 b3=-.08 b4=-.32 b5=-.4477 s2e_bmi=0.8104 g11=.0684
	c0=2.52 c1=.0122 c2=-.0634 c3=.3371 c4=-2.7949 s2e_ad=.4458;
if Scale='BMI' then do;
	mean=b0 + b1*NACCAGE + b2*SEX2 + b3*EDUC +b4*NONWHITE + b5*NE4S + u1 ;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_bmi)) -0.5*(outcome-mean)**2/(s2e_bmi);
end;
if Scale='AD' then do;
	mean=c0 + c1*NACCDAGE + c2*SEX2 + c3*NONWHITE + c4*NE4S + u1;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_ad)) -0.5*(outcome-mean)**2/(s2e_ad);
end;
model outcome~general(ll);
random  u1~normal([0],[g11]) subject=NACCID;
run;