   /*soh************************************************************************
   Boston University - Biostatistics Department
   PROGRAM NAME          : script - Generalized Linear Models.sas
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 1)
   PROJECT NAME          : Generalized Linear Models (BS853) 
                           - Lecture: Generalized Linear Models

   DESCRIPTION           : 1. Read data
                           2. Run some Generalized Linear Models
                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3
   INFRASTRUCTURE        : Windows 
   INPUT                 : 
   OUTPUT                : 
  -----------------------------------------------------------------------------
   Author : Gheorghe Doros 
   Last modified 01/18/2023                
 **eoh************************************************************************/
data im;
input Nation $ 1-25 Inc 30-34  En 38-42 Lit 46-50 US5 54-58;
LInc=log(Inc);
cards;
Armenian                      9.73    54.9    92.1    54.6
Bohemian/Moravian            13.07    66.0    96.8    71.2
Bulgarian                    10.31    20.3    78.2     8.5
Canadian (French)            10.62    79.4    84.1    86.7
Canadian (Other)             14.15   100.0    99.0    90.8
Croation                     11.37    50.9    70.7    38.9
Danish                       14.32    96.5    99.2    85.4
Dutch                        12.04    86.1    97.9    81.9
English                      14.13   100.0    98.9    80.6
Finnish                      13.27    50.3    99.1    53.6
Flemish                      11.07    45.6    92.1    32.9
French                       12.92    68.6    94.3    70.1
German                       13.63    87.5    98.0    86.4
Greek                         8.41    33.5    84.2    18.0
Hebrew (Russian)             12.71    74.7    93.3    57.1
Hebrew (Other)               14.37    79.5    92.8    73.8
Irish                        13.01   100.0    96.0    90.6
Italian (Northern)           11.28    58.8    85.0    55.2
Italian (Southern)            9.61    48.7    69.3    47.8
Lithuanian                   11.03    51.3    78.5    53.8
Macedonian                    8.95    21.1    69.4     2.0
Magyar                       11.65    46.4    90.9    44.1
Norwegian                    15.28    96.9    99.7    79.3
Polish                       11.06    43.5    80.1    54.1
Portuguese                    8.10    45.2    47.8    57.5
Roumanian                    10.90    33.3    83.3    12.0
Russian                      11.01    43.6    74.6    38.0
Ruthenian                     9.92    36.8    65.9    39.6
Scotch                       15.24   100.0    99.6    83.6
Servian                      10.75    41.2    71.5    31.4
Slovak                       11.95    55.6    84.5    60.0
Slovenian                    12.15    51.7    87.3    49.9
Swedish                      15.36    94.7    99.8    87.4
Syrian                        8.12    54.6    75.1    45.3
Turkish                       7.65    22.5    56.5    10.0
;
run;


Title 'EDA for data';
proc insight data=IM;
  scatter INC LINC En Lit US5*
          INC LINC En Lit US5;
run;
quit;

options nodate pageno=1;
title "Simple linear regression of income";

ods output outputstatistics=OS;
proc reg data=IM;
model Inc=EN Lit US5/influence;
output out=OutIm(keep=Nation LInc Inc En Lit US5 
                       r lev cd dffit df)
                       rstudent=r h=lev cookd=cd dffits=dffit;
run;
quit;

options nodate pageno=1;
title "Simple linear regression of Log income";

ods output outputstatistics=OS;
proc reg data=IM;
model lInc=EN Lit US5/influence;
output out=OutIm(keep=Nation LInc Inc En Lit US5 
                       r lev cd dffit df)
                       rstudent=r h=lev cookd=cd dffits=dffit;
run;
quit;


Title 'Univariate summaries of data';
proc univariate data=OutIM normal;var Linc INC r;histogram Linc INC r;run;

title 'The 3 types of means';
proc sql;
    create table means as
    select mean(Inc) as Arithmetic, count(Inc)/sum(1/Inc) as Harmonic, 
   exp(mean(log(Inc))) as Geometric
    from IM;
quit;

title 'Kernel estimate of residuals';
proc kde data=OutIm out=den;
  var r;
run;

proc sort data=den;
  by r;
run;

goptions reset=all;
symbol1 c=blue i=join v=none height=1;
proc gplot data=den;
  plot density*r=1;
run;
quit;

options ps=60 ls=89 pageno=1 nodate;

data cd4; 
   input disease cd4 @@; 
   logcd4=log(cd4); 
   datalines; 
   1 396 1 568  1 1212 1 171 1 554  1 1104 1 257 1 435 1 295  1 397
   1 288 1 1004 1 431  1 795 1 1621 1 1378 1 902 1 958 1 1283 1 2415
   2 375 2 375  2 752  2 208 2 151  2 116  2 736 2 192 2 315  2 1252 
   2 675 2 700  2 440  2 771 2 688  2 426  2 410 2 979 2 377  2 503
   ;
   run;
      /* Normal distribution with identity function as link*/
   title 'Normal distribution for the response (CD4 count)'; 
   proc genmod data=cd4;
       class disease;
       model cd4=disease; 
   run; 

  /* Lognormal distribution  */
   title 'Lognormal distribution for the response (CD4 count) - Link=identity';
   title2 'Definition: Lognormal=The Log of the variable is normal';
   ods output OBSTATS=check;
   proc genmod data=cd4; 
      model logcd4=disease/ obstats;
   run; 
   
   title2 'Contents of the outputted OBSTATS dataset';
   proc contents data= check;
   run;

   ods select plots testsfornormality; 
   proc univariate normal plot data= check; 
      var resdev; 
   run;
   title2;

  /* Normal Disribution with LOG link  */
   title 'Normal distribution, Log link';
   ods output OBSTATS=check;
   proc genmod data=cd4; 
     class disease;
     model cd4=disease/dist=Normal link=log obstats;
   run;

 ods select plots testsfornormality; 
   proc univariate normal plot data=check; 
     var resdev; 
   run;



/******************  Simulations ******************/


/* Set parameter values */
%let b0=2.58;
%let b1=0.04;
%let b2=0.08;
%let b3=0.01;
%let sigma=1.06;
%let n=35;

/* Generate Predictors and outcome for single data*/
data one;
do ID = 1 to &n;
  EN=ranuni(10);
  Lit=ranuni(15);
  US5=ranuni(15);
  Xb=&b0.+&b1.*EN+&b2.*Lit+&b3.*US5;
  INC = Xb+rand('normal')*&sigma;
  output;
end;
run;

options nodate nonumber;
/* Analyze data */
ODS output ParameterEstimates=PE;
title "Simple linear regression of Income (INC)";
proc reg data=one;
 model Inc=EN Lit US5;
run;

title Parameter Estimates (Single Simulation);
proc print data=PE noobs labels;
run;

/* Generate Predictors and outcome for 100 datasets*/
%let nSim=100;
title Simulate more than one data;
data MANY;
do ID = 1 to &n;
  EN=ranuni(10);
  Lit=ranuni(15);
  US5=ranuni(15);
do SIMULATION=1 to &nSim;
  Xb=&b0.+&b1.*EN+&b2.*Lit+&b3.*US5;
  INC = Xb+rand('normal')*&sigma;
  output;
end;
end;
run;

/* Analyze data */
proc sort data=MANY;by SIMULATION; run;
ODS output ParameterEstimates=SE;
title "Simple linear regression of Income";
proc reg data=MANY;
by SIMULATION;
 model Inc=EN Lit US5;
run;

/* Summarize results */
title Parameter Estimates Summary (Multiple Simulation);
proc means data=SE mean STDERR;
VAR ESTIMATE;
Class VARIABLE;
WAYS 1;
run;

/**************** Programing in SAS gproc enmod ********/

  /* Poisson distribution  */
   title 'Poisson distribution, Log link';
   ods output OBSTATS=check;
   proc genmod data=cd4; 
     class disease;
     model cd4=disease/dist=poisson link=log obstats;
   run;
   ods select plots testsfornormality; 
   proc univariate normal plot data=check; 
     var resdev; 
   run;

   /* Negative Binomial distribution */
    title 'Negative Binomial distribution';
    ods output OBSTATS=check;
    proc genmod data=cd4; 
      class disease; 
      model cd4=disease/ dist=negbin link=log obstats; 
    run;
   ods select plots testsfornormality; 
   proc univariate normal plot data=check; 
     var resdev; 
   run;

/* Poisson Regression with Distribution and Link predefined*/ 
proc genmod data= Prev;
 class size age;
 model P=size age/d=p link=log offset=ln;
run;

/* Poisson Regression with Distribution defined using 'deviance' and Link predefined*/ 
proc genmod data= Prev;
   class size age;
      A = _MEAN_;
      Y = _RESP_;
      D = 2 * (-Y * log(A) + A);
   variance  VAR = A;                   /* Define the variance*/
   deviance  DEV = D;                   /*Define the Deviance */
   model P = size age / link = log offset = ln;
run;

/* Poisson Regression with Distribution predefined and Link defined using FWD and INV LINK*/ 
   proc genmod data= Prev;
   class size age;
     A = _MEAN_;
	 XB = _XBETA_;
   fwdlink FLINK = log(A/N);           /* Define the lnk*/
   invlink ILINK = exp(XB)*N;
   model P=size age/d=p;
   run;

/* Poisson Regression with Distribution defined using 'deviance' and Link defined using FWD and INV LINK*/ 
proc genmod data= Prev;
   class size age;
      A = _MEAN_;
      Y = _RESP_;
      D = 2 * (-Y * log(A) + A);
	 XB = _XBETA_;
   fwdlink FLINK = log(A/N);           /* Define the lnk*/
   invlink ILINK = exp(XB)*N;
   variance  VAR = A;                  /* Define the variance*/
   deviance  DEV = D;                  /*Define the Deviance */
   model P=size age;
   run;


