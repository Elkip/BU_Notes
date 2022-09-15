title 'SAS Code: BS806, Class 1' ;

libname bs8061 'C:\Users\Paola\OneDrive\Documents\teaching\BS806\slides\class-1\SAS';

data heights;
infile 'C:\Users\Paola\OneDrive\Documents\teaching\BS806\slides\class-1\SAS\heights.txt' ;
input mheight dheight;
run;
proc print data=heights;
id mheight dheight;
run;

proc reg data=heights;
title 'Simple Linear Regression' ;
model dheight = mheight/ clb;
