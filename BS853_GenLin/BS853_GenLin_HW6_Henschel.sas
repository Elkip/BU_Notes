
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
 model count = cage agesq method agesq*method cage*method/dist=p ;
 estimate 'method 1 vs 3' method*cage 1 0 -1 ;
 estimate 'method 1 vs 3' method*agesq 0 1 -1;
 estimate 'method 2 vs 3' method*agesq 1 0 -1;
 estimate 'method 2 vs 3' method*agesq 0 1 -1;
run;