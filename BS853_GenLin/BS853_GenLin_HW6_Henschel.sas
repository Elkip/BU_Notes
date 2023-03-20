
/* Contraceptive data 1=Sterilization,2=Other, 3=None*/
data contra; 
input age method count ref total;
cage=age-4;
agesq=cage**2;
elsn=log((count/total)/(ref/total));
datalines; 
1 1   3 232 296
1 2  61 232 296
1 3 232 232 296
2 1  80 400 617
2 2 137 400 617
2 3 400 400 617
3 1 216 301 648
3 2 131 301 648
3 3 301 301 648
4 1 268 203 547
4 2  76 203 547
4 3 203 203 547
5 1 197 188 435
5 2  50 188 435
5 3 188 188 435
6 1 150 164 338
6 2  24 164 338
6 3 164 164 338
7 1  91 183 284
7 2  10 183 284
7 3 183 183 284
;
run;

/* Model 3 */
title2 ’ Using PROC LOGISTIC - continuous age linear and quadratic effects ’;
proc logistic data = contra order = data ;
freq count ;
model method = cage agesq / link = glogit ;
output out = aa ( where =( _level_ in (1 ,2) and method in (1 ,2) and method = _level_ ) )
xbeta = pred ;
run ;

/* Equivalent using estimate */
proc genmod data=contra;
 class method age;
 model count = method age method*cage method*agesq/dist=p;
 estimate 'cage*method 1 vs 3' cage*method 1 0 -1;
 estimate 'cage*method 2 vs 3' cage*method 0 1 -1;
 estimate 'cage*method 1 vs 3' agesq*method 1 0 -1;
 estimate 'cage*method 2 vs 3' agesq*method 0 1 -1;
run;

*Exercise 2;
/*
Male = 1
Female = 2

White = 1
Black = 2

Democrat = 1
Republican = 2
Independent = 3
*/

data polaff;
    input gender race polaff count;
    datalines;
    1 1 1 132
    1 2 1 42
    2 1 1 172
    2 2 1 56
    1 1 2 176
    1 2 2 6
    2 1 2 129
    2 2 2 4
    1 1 3 127
    1 2 3 12
    2 1 3 130
    2 2 3 15
;
run;

*Find a generalized logit model that fits the data adjusted for race and gender;
proc logistic data=polaff order=data;
    freq count;
    class gender(ref='1') race(ref='1')/param = glm;
    model polaff(ref='3') = gender race/link=glogit;
run;
