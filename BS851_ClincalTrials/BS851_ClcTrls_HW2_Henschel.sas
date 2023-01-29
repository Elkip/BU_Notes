*Question 1a. Simple Randomization;
data random;
seed=1;
do id=1 to 200;
	r=ranuni(seed);
	if r<0.5 then group='Vitamin D';
	if r>=0.5 then group='Placebo';
	output;
end;
run;
*Summarize;
proc sql;
create table summary as
select r.group as c,
	count(c) as total
	from random as r
	group by c;
quit;

proc print data=random(obs=20);
run;

*1b Permuted Blocks;
proc plan seed=1234;
factors blocks=34 ordered trt=6 random/noprint;
output out=vitD trt cvals=('A' 'A' 'A' 'B' 'B' 'B');
run; quit;

* Add patient ID ;
data vitD;
set vitD;
patient+1;
run;
* Final randomization schedule;
proc print data=vitD(obs=20)
label noobs;
var patient trt blocks;
label patient='Patient ID'
blocks='Block #'
trt='Treatment';
title1 'Vitamin D Study';
title2 'Permuted Blocking Randomization Schedule';
run;


* 1c. Stratified Randomized Blocks;
proc plan seed=4321;
factors sites=2 blocks=17 ordered trt=6 random/noprint;
output out=srb trt cvals=('A' 'A' 'A' 'B' 'B' 'B');
run; quit;

* Add patient ID ;
data srb;
set srb;
by sites;
patient+1;
if sites=1 then
patid=patient+100;
if sites=2 then
patid=patient+200;
run;
* Final randomization schedule;
proc print data=srb(obs=10)
label noobs;
var patid trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
title1 'Vitamin D Study - Chicago';
title2 'Stratified Blocking Randomization Schedule';
where sites=1;
run;
proc print data=srb(obs=10)
label noobs;
var patid trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
title1 'Vitamin D Study - Boston';
title2 'Stratified Blocking Randomization Schedule';
where sites=2;
run;

*Question 2;
%let A="GLU-001 10mg per Day";
%let B="GLU-001 20mg per Day";
%let C="Placebo";

proc plan seed=4321;
factors sites=4 ordered blocks=9 ordered trt=5 random/noprint;
output out=dbts trt cvals=( &A &A &B &B &C );
run; quit;

* Add patient ID ;
data dbts;
	set dbts;
	by sites;
	patient+1;
	if first.sites then patient=1;
	if sites=1 then
		patid=patient+1000;
	if sites=2 then
		patid=patient+2000;
	if sites=3 then
		patid=patient+3000;
	if sites=4 then
		patid=patient+4000;
run;

proc print data=dbts(obs=175)
label noobs;
var patid sites trt blocks;
label patid='Patient ID'
blocks='Block #'
sites='Site'
trt='Treatment';
title1 'Glucose Level in Diabetics';
run;
