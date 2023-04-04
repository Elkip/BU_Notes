/**********************************************************************/
/*** 
/***
/***
/*** Iterative weighted linear regression for Logiostic regression
/***
/***
/***********************************************************************/
data one;
input age child Y N;
   age1=(age=1);
   age2=(age=2);
   age3=(age=3);
   /* Initialize */
   mu=Y;
   eta=log(mu/(N-mu));
   Z=eta+(y-mu)*N/(mu*(N-mu));
   W=mu*(N-MU)/N;
cards ;
1 1 58 323
1 0 14 74
2 1 68 283
2 0  37 121
3 1 79 309
3 0 158 303
4 1 14 57
4 0 79 137
;run;

/*** Parameter estimates by logistic regression ***/
ods output parameterestimates=T0;
proc genmod data=one;
  model Y/N=age1-age3 child;
run;
/* Store estimates */
proc transpose data=T0 out=est prefix=beta;
  var estimate stderr;
run;
data est;
set est;_name_='TRUE Value';label _Name_='Itteration' _Label_='Statistic';
run;

/*** Parameter estimates by weighted linear regression ***/
/* Step 0*/
ods output parameterestimates=b;
proc genmod data=one;
  model Z=age1-age3 child;
  weight w;
  output out=a pred=pred;
run;
proc transpose data=b out=b1 prefix=beta;
  var estimate stderr;
run;
proc append data=b1 base=est;run;

/*** Iterate - nSim times */
%let nSim=10;
%macro fit;
%do i=1 %to &nSim;
/* update data */
data ITT;
 set a;
   mu=N/(1+exp(-Pred));
   eta=log(mu/(N-mu));
   Z=eta+(y-mu)*N/(mu*(N-mu));
   W=mu*(N-MU)/N;
 drop Pred;
run;

ods output parameterestimates=b;
proc genmod data=ITT;
   model Z=age1-age3 child;
   weight w;
   output out=a pred=pred;
run;
proc transpose data=b out=ap prefix=beta;
  var estimate stderr;
run;
proc append data=ap base = est force;
run;
%end;
%mend fit;
%fit;

