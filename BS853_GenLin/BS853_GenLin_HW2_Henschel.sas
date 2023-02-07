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
proc genmod data=traffic;
class year;
model count = /dist=poisson link=log;
run;
