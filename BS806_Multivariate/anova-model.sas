title 'SAS Code: BS806, Class 1 AOVA' ;

data smoking;
infile 'C:\Users\Paola\OneDrive\Documents\teaching\BS806\slides\class-1\SAS\smoking.txt' ;
input TestLev SmokingHX $;
run;
proc print data=smoking;
id TestLev SmokingHX;
run;

proc ANOVA data=smoking;
title 'ANOVA' ;
class SmokingHX;
model TestLev = SmokingHX;
means SmokingHX / cldiff tukey;
run;
