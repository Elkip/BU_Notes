*1a. Sample size and power calculation;
proc power;
twosamplemeans alpha=.05
meandiff=6
test=diff
sides=2
power=.
stddev=12
ntotal=192;
*ntotal= 128 64;
run;

proc power;
twosamplemeans alpha=.05
meandiff=6
test=diff
sides=2
power=.
stddev=12
ntotal= 225;
run;

*1b. Is the relation function linear?;
proc power plotonly;
twosamplemeans alpha=.05
meandiff=6
test=diff
sides=2
power=.9
stddev=12
ntotal= .;
plot x=power min=.7 max=.99
key=byfeature(pos=inset);
run;

*2a. Sample size calculation and power;
proc power;
twosamplefreq alpha=.05
groupproportions=(.3 .21)
test=pchi
sides=2
power=.
ntotal=734;
run;

proc power;
twosamplefreq alpha=.05
groupproportions=(.3 .21)
test=pchi
sides=2
power=.
ntotal=808;
run;

*2b. Calculate the risk ratio;
proc power;
twosamplefreq alpha=.05
refproportion = .3
relativerisk=.7
test=pchi
sides=2
power=.
ntotal=734;
run;

proc power;
twosamplefreq alpha=.05
refproportion = .3
relativerisk=.7
test=pchi
sides=2
power=.
ntotal=734;
run;

*3. Non-inferiority Trials;
data lice;
input trt outcome count;
cards;
0 0 117
0 1 100
1 0 96
1 1 131
;
run;

proc freq data=lice;
table trt*outcome/
	riskdiff(column=2 noninf method=fm margin=0.15) alpha=0.025;
weight count;
run;