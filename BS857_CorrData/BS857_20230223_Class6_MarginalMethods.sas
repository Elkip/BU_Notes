libname S857 'C:\Users\yorghos\Dropbox\Courses\BS857\2021\Datasets';

proc logistic data=s857.BPD plots=EFFECT descending;
class BPD;
model BPD=Weight;
title1 'Simple Logistic Regression using PROC LOGISTIC';
run;

proc genmod data=s857.BPD descending;
class BPD;
model BPD=Weight/DIST=binomial LINK=LOGit;
title1 'Simple Logistic Regression using PROC GENMOD';
run;


proc logistic data=s857.BPD plots=EFFECT descending;
class BPD;
model BPD=Weight  Gest_Age Toxemia;
title1 'Multiple Logistic Regression using PROC LOGISTIC';
run;

proc genmod data=s857.BPD descending;
class BPD;
model BPD=Weight  Gest_Age Toxemia/DIST=BINOMIAL LINK=LOGIT;
title1 'Multiple Logistic Regression using PROC GENMOD';
run;
title1 'Marginal Logistic Regression Model for Obesity';
title2 'Muscatine Coronary Risk Factor Study';

proc freq data=s857.muscatine;
where occasion=2;
by gender;
tables age*y;
run;
 
data muscatine;
set s857.muscatine;
cage=age - 12;
cage2=cage*cage;
cage3=cage2*cage;
run;
/*using a compound symmetry structure working correlation*/
proc genmod data=muscatine ;
     class id occasion;   
     model y(event='1')=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / type=cs  corrw covb;
run;
/*Using Log OR correlation structure*/
proc genmod data=muscatine ;
     class id occasion;   
     model y(event='1')=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion logor=fullclust;
run;

/*Equivalent model*/
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion 
         logor=zrep( (1 2) 1 0 0,
                     (1 3) 0 1 0,
                     (2 3) 0 0 1) ;
run;
/*The same association between any two occasions */
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion 
         logor=zrep( (1 2) 1  ,
                     (1 3) 1  ,
                     (2 3) 1 );
run;
/*Testing whether the association between occasions 1-2 
and 2-3 are the same */
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion 
         logor=zrep( (1 2) 1 0 0,
                     (1 3) 0 1 0,
                     (2 3) 1 0 1) ;
run;
/*Fitting a model with the association between occasions 1-2 
and 2-3 are the same */
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 / dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion 
         logor=zrep( (1 2) 1 0 ,
                     (1 3) 0 1,
                     (2 3) 1 0);
store p1;
run;
ods graphics on;
ods html style=journal;
proc plm source=p1;
  score data = muscatine out=pred /ilink;
run;
proc sort data = pred;
  by gender age;
run;
proc sgplot data = pred;
  series x = age y = predicted /group=gender;
run;
ods graphics off;




/*Cubic Model*/
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 cage3/ dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion
	 logor=zrep( (1 2) 1 0,
                     (1 3) 0 1,
                     (2 3) 1 0);
store p1;
run;
ods graphics on;
ods html style=journal;
proc plm source=p1;
  score data = muscatine out=pred /ilink;
run;
proc sort data = pred;
  by gender age;
run;
proc sgplot data = pred;
  series x = age y = predicted /group=gender;
run;
ods graphics off;
/* Example of a quadratic model with gender age interaction */
proc genmod data=muscatine descending;
     class id occasion gender ;   
     model y=gender cage cage2 gender*cage gender*cage2  / dist=bin link=logit 
     type3 wald;
     contrast 'Age X Gender Interaction' gender*cage 1 -1, gender*cage2 1 -1 /wald;
     repeated subject=id / withinsubject=occasion logor=fullclust;
run;

/*********************************
Diagnostics(Optional)
*********************************/
ods graphics on;
proc genmod data=muscatine descending;
     class id occasion;   
     model y=gender cage cage2 cage3/ dist=bin link=logit 
     type3 wald;
     repeated subject=id / withinsubject=occasion 
         logor=zrep( (1 2) 1 0,
                     (1 3) 0 1,
                     (2 3) 1 0);
	 assess var=(cage)/resamples=10000 seed=7435865;
run;
ods graphics off;
ods rtf close;
