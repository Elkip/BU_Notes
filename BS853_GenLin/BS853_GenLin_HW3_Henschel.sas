*edu: 0 - Primary, 1 - Secondary , 2 - College;
*follow: 0 - No, 1 - Yes;
data politics;
input country $ edu follow count;
datalines;
USSR 0 0 84
USSR 0 1 94
USSR 2 0 120
USSR 2 1 318
USSR 3 0 72
USSR 3 1 473
USA 0 0 112
USA 0 1 227
USA 2 0 71
USA 2 1 371
USA 3 0 8
USA 3 1 180
UK 0 0 144
UK 0 1 356
UK 2 0 76
UK 2 1 256
UK 3 0 2
UK 3 1 22
Italy 0 0 526
Italy 0 1 166
Italy 2 0 76
Italy 2 1 256
Italy 3 0 7
Italy 3 1 47
Mexico 0 0 430
Mexico 0 1 447
Mexico 2 0 25
Mexico 2 1 78
Mexico 3 0 2
Mexico 3 1 22
;
run;

title 'Loglinear Saturated Model with GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | edu | follow /dist=poisson obstats type3;
run;

title 'Loglinear Saturated Model with CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin country|edu|follow;
run;quit;
	
title 'Loglinear Two-Way Interaction Model with GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | edu  country | follow follow | edu/dist=poisson obstats type3;
run;

title 'Loglinear Two-Way Interaction Model with CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin country | edu  country | follow follow | edu;
run;quit;

title 'Loglinear Conditional Independence of Country and Education with GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | follow follow | edu/dist=poisson obstats type3;
run;

title 'Loglinear Conditional Independence of Country and Education with CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin  country | follow follow | edu;
run;quit;

title 'Loglinear Conditional Independence of Country and Politics Following with GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | edu follow | edu/dist=poisson obstats type3;
run;

title 'Loglinear Conditional Independence of Country and Politics Following with CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin  country | edu follow | edu;
run;quit;

title 'Loglinear Conditional Independence of Education and Politics Following with GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | edu  country | follow/dist=poisson obstats type3;
run;

title 'Loglinear Conditional Independence of Education and Politics Following with CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin  country | edu  country | follow;
run;quit;

title 'Loglinear Joint Independence of (Country and Education) from Politics Following GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | edu follow/dist=poisson obstats type3;
run;

title 'Loglinear Joint Independence of (Country and Education) from Politics Following CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin  country | edu follow;
run;quit;

title 'Loglinear Joint Independence of (Country and Politics Following) from Education GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country | follow edu/dist=poisson obstats type3;
run;

title 'Loglinear Joint Independence of (Country and Politics Following) from Education CATMOD'
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin  country | follow edu;
run;quit;


title 'Loglinear Joint Independence of (Education and Politics Following) from Country GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = follow | edu country/dist=poisson obstats type3;
run;

title 'Loglinear Joint Independence of (Education and Politics Following) from Country CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin   follow | edu country;
run;quit;

title 'Loglinear Mutual Independence of Education, Country and Politics Following GENMOD';
ods select ModelFit;
proc genmod data=politics;
	class country edu follow;
	model count = country edu follow/dist=poisson obstats type3;
run;

title 'Loglinear Mutual Independence of Education, Country and Politics Following CATMOD';
ods select ANOVA;
proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin country edu follow;
run;quit;

title 'Choosen Model: Two-Way Interaction';
proc genmod data=politics;
	class country edu follow;
	model count = country | edu  country | follow follow | edu/dist=poisson type3;
run;

proc catmod data=politics;
	weight count;
	model country*edu*follow=_response_/param=ref;
	loglin country | edu  country | follow follow | edu;
run;quit;
