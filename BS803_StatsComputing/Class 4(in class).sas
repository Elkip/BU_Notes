libname Class4 'C:\Users\elkip\Desktop\Data';
data FHS_wide;
set Class4.FHS_wide;
array miss2{6} age1 age2 age3 dbp1 dbp2 dbp3;
array miss3{3} sbp1 sbp2 sbp3;
do i=1 to 6;
if miss2{i} = -99 then miss2{i} = .;
end;
do i=1 to 3;
if miss3{i} = -999 then miss3{i}=.;
end;
run;
proc reg data=FHS_wide;
model SBP1 = Age1;
Run;
quit;
proc reg data=FHS_wide;
model SBP2 = Age2;
Run;
quit;
%macro regdiag(indat,pred,outcme);
proc reg data=&indat;
model &outcme = &pred;
Run;
quit;
%mend regdiag;
%regdiag(FHS_wide,Age1,SBP1);
%regdiag(FHS_wide,Age2,DBP2);
/*%LET is a straightforward macro statement that simply assigns a value to a macro variable. */
%LET time=2;
proc print data=Class2.FHS;
where period=&time;
var randid period;
run;
 
 /*You can also use logical operators inside a macros */
%macro regdiag(indat,pred,Outcm, time);

%DO i=1 %TO &time;
title "Linear regression at period &i";
proc reg data=&indat;
where period=&i;
model &Outcm = &PRED;
Run;
quit;
%END;
%END;
%mend regdiag;
%regdiag(Class2.FHS,Age,SYSBP,2);


/*Using CALL SYMPUT to find the patient with the largest SBP*/
 PROC SORT DATA = Class2.FHS;
 BY  descending SYSBP;
 *Use CALL SYMPUT to find biggest SBP;
 DATA _NULL_;
 SET Class2.FHS;
 IF _N_ = 1 THEN
 CALL SYMPUT("biggest", randid);
 ELSE STOP;
 run;
 *Print all obs for customer with biggest SBP;
 PROC PRINT DATA = Class2.FHS NOOBS;
 WHERE randid = &biggest;
 TITLE "Subject  &biggest with largest SBP";
 var randid SYSBP;
 RUN;


 /*PROC IML: Getting Started*/

 proc iml; /* convert temperatures from Celsius to Fahrenheit scale */ 
Celsius = {-40, 0, 20, 37, 100}; 
Fahrenheit = 9/5 * Celsius + 32;
print Celsius Fahrenheit; 
quit;


proc iml; 					/* print marital status of 24 people */ 
ageGroup = {"<= 45", " > 45"}; 			/* row headings */ 
status = {"Single" "Married" "Divorced"}; 		/* column headings */ 
counts = { 5 5 0, 2 9 3 }; 		
p = counts / sum(counts);			 /* compute proportions */ 
print p[colname=status rowname=ageGroup label="Marital Status by Age Group" format=PERCENT7.1]; 			/* data to print */ 
quit;


proc iml; /* manually create matrices of various types */ 
s = 1; /* scalar */ 
x = {1 2 3, 4 5 6}; /* 2 x 3 numeric matrix */ 
y = {"male" "female"}; /* 1 x 2 character matrix */ 
print s, x,y;
quit;
proc iml;
z=j(2,3,0); /* 2x3row vector of zeros */ 
m = repeat({0 1}, 3, 2); /* repeat vector: down 3x and across 2x */ 
print z,m; 
quit;
proc iml;
i = 1:5; /* increment of 1 */ 
k = do(1, 10, 2); /* odd numbers 1, 3, ..., 9 */ 
print i, k; 
quit;
proc iml;
x = {1 2 3, 4 5 6}; 
n = nrow(x); 
p = ncol(x);
 dim = dimension(x); 
print dim; 
row = shape(x, 1); /* 1 x 6 vector */
 m = shape(x, 3, 2); /* 3 x 2 matrix */
 print x,row, m;
z = x // {7 8 9}; /* add new row at bottom */
 y = x || {7 8, 9 10}; /* add two new columns */
 print z,y;  
quit;

/*Matrix operations*/

  proc iml;
  /* true elementwise operations */
  u = {1 2};
  v = {3 4};
  w = 2*u - v;

/* true matrix operations */
A = {1 2, 3 4};
b = {-1, 1};
z = A*b;

/* hybrid elementwise operations */
x= {-4 9,
	2 5, 
	8 7};

mean= {2 7};
std = {6 2};
center = x - mean;
stdX = center / std;
print stdX;
quit;

/*Solving for the normal equations*/
proc iml;
/* set up the normal equations (X`X)b = X`y */
   x = (1:8)`;
y = {5 9 10 15 16 20 22 27}`;
/* Step 1: Compute X`X and X`y */
x = j(nrow(x), 1, 1) || x;
xpx = x` * x;
xpy = x` * y;
 b = solve(xpx, xpy);               /* solve for parameter estimates */
  print x,y,b;
quit;
/*Rows, columns and submatrices*/
proc iml;
A={1 2 3, 4 5 6, 7 8 9,
    10 11 12};
r = A[2, ];
m = A[3:4, 1:2];
print r, m;

  A[2, 1] = .;
  A[3:4, 1:2] = 0;              /* assign 0 to ALL of these elements  */
  A[{1 5 9}] = {-1 -2 -3};      /* assign elements in row-major order */
  print A;
quit;

/*CREATING MATRICES FROM SAS DATASETS*/
 proc iml;
  /* read variables from a SAS data set into vectors */
  varNames = {"Make" "Model" "Mpg_City" "Mpg_Highway"};
use Sashelp.Cars(OBS=3);
read all var varNames;
close Sashelp.Cars;
print Make Model Mpg_City Mpg_Highway;

/* read variables from a SAS data set into a matrix */
  varNames = {"Mpg_City" "Mpg_Highway" "Cylinders"};
use Sashelp.Cars(OBS=3); /* open data for reading     */
read all var varNames into m;  /* create matrix with 3 cols */
print m[c=varNames];  /* C= same as COLNAME=       */
quit;


/*CREATE SAS datasets from matrices*/
proc iml;
 x = 1:5; 				/* define the data */ 
y = 5:1;
 v = "v1":"v5"; 			 
create Out var {"x" "y" "v"}; /* name the vars */	
append; /* write the data */ 
close Out; 
quit;

proc iml;
/* create SAS data set from a matrix */ 
m = {1 2, 3 4, 5 6, 7 8}; 
create Out2 from m[colname={"x" "y"}]; 
append from m; 
close Out2; 
quit;

/*PROGRAMMING FUNDAMENTALS */
*AVOIDING LOOPS;
proc iml;
s = {1 2 3, 4 5 6, 7 8 9, 10 11 12}; /* 4 x 3 matrix        */
results = j(1, ncol(s)); /* allocate results    */


/* First attempt: Double loop (very inefficient) */
do j = 1 to ncol(s);
   sum = 0;
   do i = 1 to nrow(s);
      sum = sum + s[i,j];
end;
   results[j] = sum / nrow(s);
end;

  /* Second attempt: Single loop over columns (slightly inefficient) */
  do i = j to ncol(s);
     results[j] = sum(s[ ,j]) / nrow(s);
  end;

  
  /* MEAN function operates on cols. No loop!  */
  results = mean(s);                             /* mean of each col  */
  quit;

/*SUBSCRIPT REDUCTION OPERATORS*/
  proc iml;
/* compute sum and mean of each column */
x = {1 2 3,
     4 5 6,
     7 8 9,
     4 3 .};
colSums  = x[+, ];
colMeans = x[:, ]; /* equivalent to mean(x) */
rowSums  = x[ ,+];
rowMeans = x[ ,:];
print colSums, colMeans, rowSums rowMeans;
quit;

  /*In class exercise: Using the FHS dataset: 
calculate the mean of systolic and diastolic 
  in IML*/
proc iml;
  varNames = {"Mean Sytolic", "Mean Diastolic"};
  use Class4.FHS;
  read all var varNames into BP;
  colMeans = BP[:,];
  print colMeans[c=varNames];
quit;


/*LOCATE OBSERVATIONS*/
proc iml; 
varNames = {"Cylinders" "Mpg_City"}; 
use Sashelp.Cars; /* read data */ 
read all var varNames into X; 
idx = loc(X[,1]<6 & X[,2]>35); /* row vector */ 
print (idx`)[label="Row"] (X[idx,])[c=varNames]; 
if ncol(idx) > 0 then do; /* obs found... do something with them */
 end; 
else do; 
print "No observations satisfy the condition."; 
end; 
quit;

  /*In class exercise: Using the FHS dataset: identify subjects who are hypertensive i.e,
Systolic BP is greater than 140 or diastolic is greater than 80*/
proc iml;
  varNames = {"ID"};
  use Class4.FSH;
  read all var varNames into BP;
  HT = loc(BP[,2] > 140 & bp[,3] > 80);
  print (HT`)[label="Row"] (BP[HT,])[c=varNames];
  if ncol(HT) > 0 then do; /* obs found... do something with them */
  end; 
  else do; 
    print "No observations satisfy the condition."; 
  end; 
quit;

/*HANDLE MISSING VALUES*/
proc iml; 
x = {1, ., 2, 2, 3, .}; 
nonMissing = loc(x ^= .); /* {1 3 4 5} */ 
y = x[nonMissing]; /* y = {1,2,2,3}; */ 
print x,y;
/* exclude rows with missing values */ 
z = {1 ., 2 2, . 3, 4 4}; 
numMiss = countmiss(z, "row"); /* count missing in each row */
y = z[ loc(numMiss=0), ];  /*z[{2 4}, ] = {2 2, 4 4} */ 
print z,y;
quit;

/*ANALYZE LEVELS OF CATEGORICAL VARIABLES*/
proc iml;
  use Sashelp.Cars;
  read all var {"Type" "Mpg_City"};
  close Sashelp.Cars;

  /* UNIQUE-LOC technique */
  uC = unique(Type);
  mean = j(1, ncol(uC));
  do i = 1 to ncol(uC);
   idx=loc(Type=uC[i]);                             /* locate these obs            */
	mean[i] = mean( Mpg_City[idx] );  /* find mean of mpg            */
  end;
  print mean[colname=uC label="Average MPG (City)" format=4.1];
  quit;

/*USER DEFINED VARIABLES*/
  proc iml; /* standardize each column of x to have mean 0 and unit variance */ 
start Stdize(x); 
return( (x - mean(x)) / std(x) ); 
finish; 
/* test it */ 
A = {0 2 9, 1 4 3, -1 6 6};
 z = Stdize(A); 
print z; 
quit;

/*CALLING SAS PROCEDURES*/
/* start with data in SAS/IML vector */ 
proc iml;
x = {1 1 1 1 2 2 2 3 4 4 5 6 6 8 9 11 11 15 22}`; 
/* 1. Write to SAS data set */ 
create In var {"x"}; 
append; 
close In; 
/* 2. Call SAS procedure */ 
submit; 
proc means data=In noprint; 
var x; 
output out=Output Skewness=Skew; 
run; 
endsubmit; 
/* 3. Read results */ 
use Output; 
read all var {"Skew"}; 
close Output; 
print Skew; 
submit; 
proc sgplot data=In; 
title "Created by PROC SGPLOT"; 
histogram x; 
density x / type=kernel; 
run; 
endsubmit; 
quit;
/*SIMULATING DATA*/
proc iml; 
N = 10; 		 /* number of obs in each sample */ 
NumSamples = 10000; 	 /* number of samples */ 
call randseed(123); 	 /* set seed for random number stream */ 
x = j(N, NumSamples);	 /* each column is sample of size N */
 call randgen(x, "Uniform"); 	 /* simulate data */ 
s = mean(x); 		/* compute mean for each col */ 

/* summarize approximate sampling distribution */ 
s = T(s); 
Mean = mean(s); 
StdDev = std(s); 
print Mean StdDev; 
quit;

/*OPTIMIZATION*/
proc iml; 
use Sashelp.Iris; 
read all var {SepalWidth} into x; 
close Sashelp.Iris; 
/* print the optimal parameter values */ 
muMLE = mean(x); 
n = countn(x); 
sigmaMLE = sqrt( (n-1)/n * var(x) ); 
print muMLE sigmaMLE; 
quit;


/*Maximizing the log likelihood of the normal distribution*/
proc iml;
use Sashelp.Iris; 
read all var {SepalWidth} into x; 
close Sashelp.Iris; 
/* 1. compute the log-likelihood function for Normal distrib */
start NormLogLik(parm) global (x); 
mu = parm[1]; 	 /* param = {mu sigma} */ 
sigma2 = parm[2]##2; 
n = nrow(x);
 return( -n/2*log(sigma2) - 0.5/sigma2*sum((x-mu)##2) ); 
finish; 

parm = {35 5.5}; /* 2. initial guess for solution (mu, sigma) */ 
optn = {1, 		 /* 3. find max of function, and */ 
            4}; 		 /* print moderate amount of output */ 
con={. 0, 	/* 4. lower bound: -infty < mu; 0 < sigma, and */ 
           . .}; 	 /* upper bound: mu < infty; sigma < infty */ 
/* 5. Provide initial guess and call NLP function */ 
call nlpnra(rc, result, "NormLogLik", parm, optn, con); 
quit;
