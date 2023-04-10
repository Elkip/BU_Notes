* 1. Re-generate the table using ESTIMATE;
data claims;
do age=1 to 4;
input district car C N @@;
logN=log(N);
output;
end;
cards;
1 1 38 197 1 1 35 264 1 1 20 246 1 1 156 1680
1 2 63 284 1 2 84 536 1 2 89 696 1 2 400 3582
1 3 19 133 1 3 52 286 1 3 74 355 1 3 233 1640
1 4  4  24 1 4 18  71 1 4 19  99 1 4  77  452
2 1 22  85 2 1 19 139 2 1 22 151 2 1  87  931
2 2 25 149 2 2 51 313 2 2 49 419 2 2 290 2443
2 3 14  66 2 3 46 175 2 3 39 221 2 3 143 1110
2 4  4   9 2 4 15  48 2 4 12  72 2 4  53  322
3 1  5  35 3 1 11  73 3 1 10  89 3 1  67  648
3 2 10  53 3 2 24 155 3 2 37 240 3 2 187 1635
3 3  8  24 3 3 19  78 3 3 24 121 3 3 101  692
3 4  3   7 3 4  2  29 3 4  8  43 3 4  37  245
4 1  2  20 4 1  5  33 4 1  4  40 4 1  36  316
4 2  7  31 4 2 10  81 4 2 22 122 4 2 102  724
4 3  5  18 4 3  7  39 4 3 16  68 4 3  63  344
4 4  0   3 4 4  6  16 4 4  8  25 4 4  33  114
run;

proc genmod data = claims ;
class district car age ;
model C = district car age / offset = logN dist = poisson link = log obstats ;
estimate ' district 1 vs . 4 ' district 1 0 0 -1/ exp ;
estimate ' district 2 vs . 4 ' district 0 1 0 -1/ exp ;
estimate ' district 3 vs . 4 ' district 0 0 1 -1/ exp ;
estimate ' car 1 vs . 4 ' car 1 0 0 -1/ exp ;
estimate ' car 2 vs . 4 ' car 0 1 0 -1/ exp ;
estimate ' car 3 vs . 4 ' car 0 0 1 -1/ exp ;
estimate ' age 1 vs . 4 ' age 1 0 0 -1/ exp ;
estimate ' age 2 vs . 4 ' age 0 1 0 -1/ exp ;
estimate ' age 3 vs . 4 ' age 0 0 1 -1/ exp ;
run ;

* 2 Example A;
data skin;
input AgeCont Age $ City $ Cases PopSize;
rate=(Cases/PopSize);
rateLog = log(Cases/PopSize);
logPopSize = log(PopSize);
cards;
1 15-24 M 1   172675
1 15-24 D 4   181343
2 25-34 M 16  123065
2 25-34 D 38  146207
3 35-44 M 30  96216
3 35-44 D 119 121374
4 45-54 M 71  92051
4 45-54 D 221 111353
5 55-64 M 102 72159
5 55-64 D 259 83004
6 65-74 M 130 54722
6 65-74 D 310 55932
7 75-84 M 133 32185
7 75-84 D 226 29007
8 85+   M 40  8328
8 85+   D 65  7538
;
run;

* a. Plot the observed log incidence rates by and area;
title1 'Incidence of Skin Cancer';
symbol1 v=dot c=black;
symbol2 v=circle;
axis1 label=('AGE');
axis2 label=(a=90 'Log rate') minor=none ;
legend1 label=('City');
proc gplot data=skin;
 plot rateLog*Age=City / vaxis=axis2 haxis=axis1 legend=legend1;
 run;
quit;

* b. Find the best model, concidering age as both continuous and categorical;
title2 'Intercept Only Model (1)';
ods select ModelFit;
proc genmod data=skin;
    model Cases=/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'City Model (2)';
ods select ModelFit;
proc genmod data=skin;
    class City;
    model Cases=City/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Age (categorical) Model (3)';
ods select ModelFit;
proc genmod data=skin;
    class Age;
    model Cases=Age/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Main Effects Model with Age (categorical) (4)';
ods select ModelFit;
proc genmod data=skin;
    class City Age;
    model Cases=City Age/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Saturated Model with Age (categorical) (5)';
ods select ModelFit;
proc genmod data=skin;
    class City Age;
    model Cases=City | Age/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Age (continuous) Model (6)';
ods select ModelFit;
proc genmod data=skin;
    model Cases=AgeCont/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Main Effects Model with Age (continuous) (7)';
ods select ModelFit;
proc genmod data=skin;
    class City;
    model Cases=City AgeCont/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Saturated Model with Age (continuous) (8)';
ods select ModelFit;
proc genmod data=skin;
    class City;
    model Cases=City | AgeCont/offset=logPopSize dist=poisson link=log obstats;
run;

title2 'Main Effects with Age (categorical) - Negative Binomial (9)';
ods select Modelfit;
proc genmod data=skin;
 class Age City;
 model Cases = Age City /dist=NB link=log offset=logPopSize obstats;
run;

* c. Analyze the residuals of model of best fit;
title1 'Main effects with Age (categorical)';
proc genmod data=skin;
 class Age City;
 model Cases = Age City /dist=poisson link=log offset=logPopSize;
output out=myoutput predicted=pred resdev=resdev reschi=reschi;
run;

proc sgplot data=myoutput;
   scatter x=pred y=resdev;
run;

proc univariate data=myoutput normal plot;
   histogram resdev reschi;
run;

* d. Use ESTIMATE to estimate age specific RR comparing Areas;
proc genmod data=skin;
 class age city;
 model cases = age|city/dist=poisson link=log offset=logPopSize;
 estimate 'D vs M in age 15-24' city 1 -1 age*city 1 -1/exp;
 estimate 'D vs M in age 25-34' city 1 -1 age*city 0 0 1 -1/exp;
 estimate 'D vs M in age 35-44' city 1 -1 age*city 0 0 0 0 1 -1/exp;
 estimate 'D vs M in age 45-54' city 1 -1 age*city 0 0 0 0 0 0 1 -1/exp;
 estimate 'D vs M in age 55-64' city 1 -1 age*city 0 0 0 0 0 0 0 0 1 -1/exp;
 estimate 'D vs M in age 65-74' city 1 -1 age*city 0 0 0 0 0 0 0 0 0 0 1 -1/exp;
 estimate 'D vs M in age 75-84' city 1 -1 age*city 0 0 0 0 0 0 0 0 0 0 0 0 1 -1/exp;
 estimate 'D vs M in age 85+'   city 1 -1 age*city 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1/exp;
run;
