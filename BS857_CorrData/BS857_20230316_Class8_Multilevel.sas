libname S857 'C:\Users\yorghos\Dropbox\Courses\BS857\2021\Datasets';


/*********************************************************************
*																	 *
*			TWO-LEVEL LINEAR MULTILEVEL MODEL						 *
*																	 *
*********************************************************************/
data s857.ntp;
set s857.ntp;
newdose=sqrt(dose/750);
run; 

proc mixed data=s857.ntp method=reml covtest ;
class id;
model weight = newdose / solution chisq;
random intercept/ subject=id g;
run;
/*
The model-based and empirical (sandwich) standard errors are very
similar , indicating that the random eects structure is
adequate.
*/
proc mixed data=s857.ntp method=reml empirical;
class id;
model weight = newdose / solution chisq;
random intercept / subject=id g;
run;
/*********************************************************************
*																	 *
*			THREE-LEVEL LINEAR MULTILEVEL MODEL						 *
*																	 *
*********************************************************************/

proc mixed data=s857.tvsfp covtest method=ml ;
class sid cid CURRICULUM (ref='0') TVPREVENT(ref='0');
model POSTSCORE = PRESCORE CURRICULUM TVPREVENT CURRICULUM*TVPREVENT / s;
random intercept / subject=sid g ;
random intercept / subject=cid g ;
run;

proc mixed data=s857.tvsfp covtest method=reml ;
class sid cid CURRICULUM (ref='0') TVPREVENT(ref='0');
model POSTSCORE = PRESCORE CURRICULUM TVPREVENT CURRICULUM*TVPREVENT / s;
random intercept / subject=sid g ;
random intercept / subject=cid g ;
run;

/*Because the correlations are small, we might conclude that the clustering
is unimportant. But consider an analysis that treats the observations as
independent, ignoring clustering*/
proc glm data=s857.tvsfp ;
class CURRICULUM (ref='0') TVPREVENT(ref='0');;
model POSTSCORE =PRESCORE CURRICULUM TVPREVENT CURRICULUM*TVPREVENT / solution;
run;
quit;
/*A fixed effects model will not give us estimates for cluster invariant covariates*/
proc glm data=s857.tvsfp ;
class sid cid CURRICULUM TVPREVENT;
model POSTSCORE =sid cid PRESCORE CURRICULUM TVPREVENT CURRICULUM*TVPREVENT / solution;
run;
quit;
/*********************************************************************
*																	 *
*			TWO-LEVEL GENERALIZED MULTILEVEL MODEL						 *
*																	 *
*********************************************************************/
proc glimmix  data=s857.ntp method=quad(qpoints=30) empirical ;
class id  ;
model Malform = newdose/dist=binomial link=logit s;
random intercept/subject=id;
run;
proc genmod data=s857.ntp descending;
class id;
model Malform = newdose/dist=binomial link=logit;
repeated subject=id/ covb logor=exch;
run;

/*Revisiting the TVSFP data set for a three level GLME model.
This model is difficult to converge*/

data tvsfp;
set s857.tvsfp;
if POSTSCORE=. then post=.;
else if POSTSCORE<3 then post=1;
else post=0;
proc freq data=tvsfp;
tables post;
run;
proc glimmix  data=tvsfp method=quad(qpoints=5) empirical ;
class sid cid;
model post = PRESCORE CURRICULUM TVPREVENT CURRICULUM*TVPREVENT/dist=binomial link=logit s;

random intercept / subject=cid;
random intercept / subject=sid ;
run;
