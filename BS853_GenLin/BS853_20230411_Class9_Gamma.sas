  /*soh************************************************************************                                                        
   Boston University - Biostatistics Department                                                                                         
   PROGRAM NAME          : script - Lecture 9.sas                                                                                
                           (C:\Documents and Settings\doros\My Documents\Projects\BS853\Class 9)                                        
   PROJECT NAME          : Generalized Linear Models (BS853)                                                                            
                           - Lecture: Gamma Regression                                                                        
                                                                                                                                        
   DESCRIPTION           : 1. Read data                                                                                                 
                           2. Gamma Regression                                                    
                                                                                                                                        
   SOFTWARE/VERSION#     : SAS/Version 9.3                                                                                              
   INFRASTRUCTURE        : Windows                                                                                                      
   INPUT                 :                                                                                                              
   OUTPUT                :                                                                                                              
  -----------------------------------------------------------------------------                                                         
   Author : Gheorghe Doros                                                                                                              
   Last modified                                                                                                          
 **eoh************************************************************************/                                                         
options ps=60 ls=89 pageno=1 nodate;                     

/********************************************************/
/*** Generate Gamma density ***/
/********************************************************/

data density;
 do nu=0.5, 1, 2, 10;
  mu=7;
  do x=0.1 to 15 by 0.1;
   fx=1/(gamma(nu)*x)*(nu*x/mu)**nu*exp(-nu*x/mu);
  output;
  end;
 end;
run;
 goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=7in aspect=1 horigin=1.5in vorigin=0.3in 
            Colors=( black, blue brown cyan gold charcoal cream ) device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;

filename TempOut1 "Z:\Documents\BS853\Class 9\densities.emf";  

symbol v=none i=join;
axis1 label=(a=90 'Gamma Density') minor=none;
proc gplot data=density;
plot fx*x=nu/vaxis=axis1;
run;

/*****************************************************/
/***            Car insurance claims               ***/
/*****************************************************/
data claims;
infile 'Z:\Documents\BS853\Spring 2017\Class 9\cardamag.dat';
input policyholderage cargroup ageofcar cost number;
lcost=log(cost+1);
run;

/* estimate the coefficients of variation */
title1 ' Coefficient of variation ';
options pageno=1 ls=97 nodate;
proc means data=claims cv;
  class policyholderage cargroup ageofcar;
  var cost;
  ways 1;
run;

/* Exploratory Data Analysis (EDA) */
proc means data=claims;
  class policyholderage ageofcar;
  var cost;
  ways 1 2;
  output out=means mean=mean;
  weight number;
run;
data meanout;
  set means;
  imeans=mean**(-1);
  lmeans=log(mean);
run;
filename TempOut1 "Z:\Documents\BS853\Class 9\plotim.emf";  
symbol1 i=j line=2 v=circle;
axis1 label=(a=90 'Raw inverse means');
axis2 label=(a=90 'Raw Log means');
title 'Exploratory data analysis';
proc gplot data=meanout;
plot imeans*policyholderage/vaxis=axis1;
where _type_ ne 3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotlm.emf";  
symbol1 i=j line=2 v=circle;

proc gplot data=meanout;
plot lmeans*policyholderage/vaxis=axis2;
where _type_ ne 3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotimi.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot imeans*policyholderage=ageofcar/vaxis=axis1;
where _type_ =3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotlmi.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot lmeans*policyholderage=ageofcar/vaxis=axis2;
where _type_ =3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotic.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot imeans*ageofcar/vaxis=axis1;
where _type_ ne 3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotlc.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot lmeans*ageofcar/vaxis=axis2;
where _type_ ne 3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotici.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot imeans*ageofcar=policyholderage/vaxis=axis1;
where _type_ =3;
run;

filename TempOut1 "Z:\Documents\BS853\Class 9\plotlci.emf";  

symbol1 i=j line=2 v=circle;
proc gplot data=meanout;
plot lmeans*ageofcar=policyholderage/vaxis=axis2;
where _type_ =3;
run;

/*****************************************************************/
/*** Model Selection - Inverse Link                            ***/
/*****************************************************************/


title1 ' Gamma regression - Saturated model';
title3 'Inverse Link';
options pageno=1 nodate ls=97;
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup|ageofcar 	/dist=gamma ;
weight number;
run;

title1 ' Gamma regression - Intercept only model';
title3 'Inverse Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost= 	/dist=gamma ;
weight number;
run;
title1 ' Gamma regression - Main effects model';
title3 'Inverse Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar;
model cost=policyholderage cargroup ageofcar/dist=gamma ;
weight number;
run;
title1 ' Gamma regression - all two way interactions';
title3 'Inverse Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup|ageofcar @2	/dist=gamma type3;
weight number;
run;

title1 ' Gamma regression - Joint independence of policyholder and cargroup from ageofcar';
title3 'Inverse Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup ageofcar 	/dist=gamma;
weight number;
run;
title1 ' Gamma regression - Quadratic Age of car effect';
title3 'Inverse Link';
ods output obstats=check1;
proc genmod data=claims;
class policyholderage cargroup  ;
model cost=policyholderage|cargroup ageofcar|ageofcar 	/dist=gamma obstats;
weight number;
run;

/*****************************************************************/
/*** Model Selection - Log Link                            ***/
/*****************************************************************/


title1 ' Gamma regression - Saturated model';
title3 'Log Link';
options pageno=1 nodate ls=97;
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup|ageofcar 	/dist=gamma link=log ;
weight number;
run;

title1 ' Gamma regression - Intercept only model';
title3 'Log Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost= 	/dist=gamma link=log ;
weight number;
run;
title1 ' Gamma regression - Main effects model';
title3 'Log Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar;
model cost=policyholderage cargroup ageofcar/dist=gamma link=log ;
weight number;
run;
title1 ' Gamma regression - all two way interactions';
title3 'Log Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup|ageofcar @2	/dist=gamma type3 link=log;
weight number;
run;

title1 ' Gamma regression - Joint independence of policyholder and cargroup from ageofcar';
title3 'Log Link';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model cost=policyholderage|cargroup ageofcar 	/dist=gamma link=log ;
weight number;
run;
title1 ' Gamma regression -  Quadratic Age of car effect';
title3 'Log Link';
ods output obstats=check2;
proc genmod data=claims;
class policyholderage cargroup  ;
model cost=policyholderage|cargroup ageofcar|ageofcar 	/dist=gamma link=log obstats;
weight number;
run;

/*****************************************************************/
/*** Model Selection - Log normal                           ***/
/*****************************************************************/


title1 ' Log-Normal regression - Saturated model';
options pageno=1 nodate ls=97;
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model lcost=policyholderage|cargroup|ageofcar @2	/dist=normal ;
weight number;
run;

title1 ' Log-Normal regression - Intercept only model';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model lcost= 	/dist=normal ;
weight number;
run;
title1 ' Log-Normal regression - Main effects model';
proc genmod data=claims;
class policyholderage cargroup ageofcar;
model lcost=policyholderage cargroup ageofcar/dist=normal ;
weight number;
run;
title1 ' Log-Normal regression - all two way interactions';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model lcost=policyholderage|cargroup|ageofcar @2	/dist=normal type3;
weight number;
run;

title1 ' Log-Normal regression - Joint independence of policyholder and cargroup from ageofcar';
proc genmod data=claims;
class policyholderage cargroup ageofcar ;
model lcost=policyholderage|cargroup ageofcar 	/dist=normal ;
weight number;
run;
title1 ' Log-Normal regression -  Quadratic Age of car effect';
ods output obstats=check3;
proc genmod data=claims;
class policyholderage cargroup  ;
model lcost=policyholderage|cargroup ageofcar|ageofcar 	/dist=normal obstats;
weight number;
run;

data allcheck;
merge check1(rename=(pred=pred1)) check2(rename=(pred=pred2)) check3(rename=(pred=pred3));
by observation;
pred3=exp(pred3);
keep obs cost pred1-pred3;
run;
 goptions reset=all ftext='arial' htext=1.2 hsize=9.5in vsize=9.5in aspect=1 horigin=1.5in vorigin=1.5in 
            Colors=( black, blue brown cyan gold charcoal cream ) device=emf rotate=landscape gsfname=TempOut1 gsfmode=replace;

filename TempOut1 "Z:\Documents\BS853\Class 9\check.emf";  
axis1 label=(a=90 'Observed');
axis2 label=('Predicted');
symbol1 v=dot cv=black;
symbol2 v=circle cv=green;
symbol3 v=triangle cv=blue;
title1 'Predicted vs. Observed';
legend1 value=('Inverse Link' 'Log Link' 'Log-Normal') label=('Type of Model'); 
proc gplot;
where cost ne 0;
plot cost*pred1 cost*pred2 cost*pred3/overlay vaxis=axis1 haxis=axis2 legend=legend1;;
run;
quit;
