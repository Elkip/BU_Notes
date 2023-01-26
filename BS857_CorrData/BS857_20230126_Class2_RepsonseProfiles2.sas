
data lead;
set s857.tlc;
y=y0;time=0;output;
y=y1;time=1;output;
y=y4;time=4;output;
y=y6;time=6;output;
drop y0 y1 y4 y6;
run;

proc mixed data=lead;
class id trt(ref='P') time(ref='0');
model y=trt time trt*time/s chisq ;
repeated time/type=un subject=id;

* ESTIMATES OF MEANS;
* Estimated lead level for treatment group at time 0;
estimate 'Trt @ time0' int 1 trt 1 0 time 0 0 0 1 trt*time 0 0 0 1 0 0 0 0 ;            /* A */

* Estimated lead level for treatment group at time 6;
estimate 'Trt @ time6' int 1 trt 1 0 time 0 0 1 0 trt*time 0 0 1 0 0 0 0 0 ;            /* B */

* Estimated lead level for placebo group at time 0;
estimate 'P @ time0'   int 1 trt 0 1 time 0 0 0 1 trt*time 0 0 0 0 0 0 0 1 ;            /* C */

* Estimated lead level for placebo group at time 6;
estimate 'P @ time6'   int 1 trt 0 1 time 0 0 1 0 trt*time 0 0 0 0 0 0 1 0 ;            /* D */


* ESTIMATES OF DIFFERENCES IN MEANS;
* Estimated change in lead level from time 0 to time 6 for treatment group;
estimate 'Trt change time6 minus time0' time 0 0 1 -1 trt*time 0  0  1 -1  0  0  0  0;  /* E = B minus A */

* Estimated change in lead level from time 0 to time 6 for placebo group;
estimate 'P change time6 minus time0'   time 0 0 1 -1 trt*time 0  0  0  0  0  0  1 -1;  /* F = D minus C */


* ESTIMATES OF INTERACTION TERMS;
* Estimated trt*time interaction term at time 6;
estimate 'Beta for trt*time6' trt*time 0  0  1 -1  0  0 -1  1;                          /* G = E minus F */

* Joint test for trt*time interaction;
contrast 'TRT*TIME '   trt*time 0  0  1 -1  0  0 -1  1, /* first row is G - create the other two rows similarly */
					   trt*time 0  1 -1  0  0 -1  1  0,
					   trt*time 1  0 -1  0 -1  0  1  0;
run;
