data traffic;
input year count;
datalines;
1 3
2 1
3 5
4 0
5 2
6 3
7 4
;
run;

title 'Loglinear models with GENMOD';
ods output OBSTATS = check_s ;
proc genmod data=traffic;
model count = year/dist=poisson link=log obstats;
run;

ods output OBSTATS = check_i ;
title2 ' Independence model ';
proc genmod data = traffic ;
model count = year/dist=poisson link=log obstats residuals ;
run ;

title1 " Fuck this stupid-ass class " ;
proc print data = check_i ;
var count year pred std reschi resdev ;
run ;
