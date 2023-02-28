/*******************************************/
/*** Multiple Group Comparison           ***/
/***                                     ***/
/*** Using contrast in Proc Genmod       ***/
/*******************************************/
%let n1=20;
%let n2=25;
%let n3=27;
%let n4=27;

/*** Single data ****/
data single;
Group=1;
do i=1 to &n1;  Y=rand('normal')*1; output; end;
Group=2;
do i=1 to &n2; Y=rand('normal')*2; output; end;
Group=3;
do i=1 to &n3; Y=rand('normal')*3; output; end;
Group=4;
do i=1 to &n4;Y=rand('normal')*3;output; end;
run;

/* Analyze data : Comapre Groups */
ods output contrasts=one;
proc genmod data=single;
 class Group;
 model Y=group;
 contrast 'G1vs2' Group 1 -1  0  0; * two group comparison ;
 contrast 'G1vs3' Group 1  0 -1  0;
 contrast 'G3vs4' Group 0  0  1 -1;
 contrast 'G3vs4&1vs2' Group 0  0  1 -1, Group 1 -1  0  0; * Multiple Comparison;
 contrast 'G1vs3&1vs2' Group 1  0 -1  0, Group 1 -1  0  0; * Multiple Comparison;
run;

data one;set one;
if ProbChiSq<0.05 then Significance=1; else significance=0;
if ProbChiSq<0.025 then Significance0=1; else significance0=0;
label Significance='Significance at 5%' Significance0='Significance at 2.5%';
run;

/*** Multiple simulations ***/
%let nSim=10000;
data multiple;
call streaminit(2020);
do Simulation=1 to &nSim;
Group=1;
do i=1 to &n1;  Y=rand('normal')*3; output; end;
Group=2;
do i=1 to &n2; Y=rand('normal')*3; output; end;
Group=3;
do i=1 to &n3; Y=rand('normal')*3; output; end;
Group=4;
do i=1 to &n4;Y=rand('normal')*3;output; end;
end;
run;
/* Sort data */
proc sort data=multiple;by simulation;run;
/* Analyze and compare groups */
ods select none;
ods output contrasts=mult;
proc genmod data=multiple;
by simulation;
 class Group;
 model Y=group;
 contrast 'G1vs2' Group 1 -1  0  0; * two group comparison ;
 contrast 'G1vs3' Group 1  0 -1  0;
 contrast 'G3vs4' Group 0  0  1 -1;
 contrast 'G3vs4&1vs2' Group 0  0  1 -1, Group 1 -1  0  0; * Multiple Comparison;
 contrast 'G1vs3&1vs2' Group 1  0 -1  0, Group 1 -1  0  0; * Multiple Comparison;
run;
/*** Compute cignificance for single comparison and multiple comparisons */
data mult;set mult;
if ProbChiSq<0.05 then Significance=1; else significance=0;
if ProbChiSq<0.025 then Significance0=1; else significance0=0;
label Significance='Significance at 5%' Significance0='Significance at 2.5%';
run;
/* Transpose to sumarize and repeated testing interaction */
proc transpose data=mult out= tmult;
by simulation;
var Significance Significance0;run;
data tmult;set tmult;
col6=(col1=1 or col3=1); *G3vs4 or 1vs2;
col7=(col1=1 or col2=1); *G1vs3 or 1vs2;
label
col1='G1vs2' col2='G1vs3' col3='G3vs4' col4='G3vs4&1vs2' col5='G1vs3&1vs2'
col6='G3vs4&1vs2r' col7='G1vs3&1vs2r';
run;
/* Summarize results */
ods select all;
proc means data=tmult mean;
var col1-col7;
class _name_;
ways 1;
run;



