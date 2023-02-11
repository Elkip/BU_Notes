*edu: 0 - Primary, 1 - Secondary , 2 - College;
*follow: 0 - No, 1 - Yes;
data politics;
input country $ edu $ follow count;
datalines;
USSR Primary 0 84
USSR Primary 1 94
USSR Secondary 0 120
USSR Secondary 1 318
USSR College 0 72
USSR College 1 473
USA Primary 0 112
USA Primary 1 227
USA Secondary 0 71
USA Secondary 1 371
USA College 0 8
USA College 1 180
UK Primary 0 144
UK Primary 1 356
UK Secondary 0 76
UK Secondary 1 256
UK College 0 2
UK College 1 22
Italy Primary 0 526
Italy Primary 1 166
Italy Secondary 0 76
Italy Secondary 1 256
Italy College 0 7
Italy College 1 47
Mexico Primary 0 430
Mexico Primary 1 447
Mexico Secondary 0 25
Mexico Secondary 1 78
Mexico College 0 2
Mexico College 1 22
;
run;

title 'Loglinear Saturated Model with GENMOD';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu | follow /dist=poisson link=log type3;
run;
	
title 'Loglinear Two-Way Interaction Model with GENMOD';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu  country | follow follow | edu/dist=poisson  link=log type3;
run;

title 'Loglinear Conditional Independence of Country and Education with GENMOD';
proc genmod data=politics;
	class country edu follow;
	model count = country | follow follow | edu/dist=poisson  link=log type3;
run;

title 'Loglinear Conditional Independence of Country and Politics Following with GENMOD';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu follow | edu/dist=poisson  link=log type3;
run;

title 'Loglinear Conditional Independence of Education and Politics Following with GENMOD';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu  country | follow/dist=poisson  link=log type3;
run;

title 'Loglinear Joint Independence of (Country and Education) from Politics Following';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu follow/dist=poisson  link=log type3;
run;

title 'Loglinear Joint Independence of (Country and Politics Following) from Education';
proc genmod data=politics;
	class country edu follow;
	model count = country | follow edu/dist=poisson  link=log type3;
run;

title 'Loglinear Joint Independence of (Education and Politics Following) from Country';
proc genmod data=politics;
	class country edu follow;
	model count = follow | edu country/dist=poisson  link=log type3;
run;

title 'Loglinear Mutual Independence of Education, Country and Politics Following.';
proc genmod data=politics;
	class country edu follow;
	model count = country edu follow/dist=poisson link=log type3;
run;
