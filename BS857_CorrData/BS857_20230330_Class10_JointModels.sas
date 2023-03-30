
*libname TBI "/usr2/faculty/yorghos/TBI";
libname s857 "C:\Users\ytrip\Dropbox\Courses\BS857\2021\Datasets";
options fmtsearch=(s857.formats);
options nofmterr;

proc mixed data=s857.Tbi;
class sex naccid  status;
model Mem1=time  naccageb status sex time*educ time*naccageb  /solution;
random Int time/sub=naccid;
run;
proc mixed data=s857.Tbi;
class sex naccid  status;
model Att1=time  naccageb status sex time*educ time*naccageb  /solution;
random Int time/sub=naccid;
run;
/* Multivariate models  */
data s857.tbi_long;
length scale $10;
set s857.tbi;
Outcome=Mem1;scale='Memory';output;
Outcome=Att1;scale='Attention';output;
keep Outcome Scale time trauma_dich dep2yrs status sex educ naccageb naccid;
run;
proc mixed data=s857.tbi_long covtest method=ML;
class Scale sex naccid  status;
model Outcome=Scale*time Scale*naccageb Scale*status Scale*time*status
 Scale*sex Scale*time*educ Scale*time*naccageb  /solution;
random Int time/sub=naccid;
*random int/sub=Scale type=un;
run;

proc nlmixed data=s857.tbi_long;
parms b0=-0.17 b1=0.43 b2=0.39 b3=-0.83 b4=0.08 b5=-0.01
s2e_mem=0.361 g11=0.60
	c0=0.05 c1=-0.05 c2=0.5 c3=0.1 
s2e_att=0.19;
if Scale='Memory' then do;
	mean=b0+b1*time + b2*status + b3*sex +b4*time*status +b5*time*naccageb  +u1 ;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_mem)) -0.5*(outcome-mean)**2/(s2e_mem);
end;
if Scale='Attention' then do;
	mean=c0+c1*time + c2*status + c3*time*status  +u1;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_att)) -0.5*(outcome-mean)**2/(s2e_att);
end;
model outcome~general(ll);
random  u1~normal([0],[g11]) subject=naccid;
run;

/*Shared parameter model*/
title 'Memory';
proc mixed data=s857.tbi covtest method=ML;
class trauma_dich sex naccid  status;
model Mem1=time status  sex time*status time*naccageb  /solution;
random intercept/sub=naccid;
run;
title 'Attention';
proc mixed data=s857.tbi covtest method=ML;
class trauma_dich sex naccid  status;
model Att1=time /solution;
random intercept/sub=naccid;
run;

proc nlmixed data=s857.tbi_long;
parms b0=-0.13 b1=0.43 b2=0.39 b3=-0.83 b4=0.08 b5=-0.01
s2e_mem=0.361 g11=0.60
	c0=0.05 c1=-0.05 c2=0.5 c3=0.1 gamma=0.20
s2e_att=0.19;
if Scale='Memory' then do;
	mean=b0+b1*time + b2*status + b3*sex +b4*time*status +b5*time*naccageb  +u1 ;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_mem)) -0.5*(outcome-mean)**2/(s2e_mem);
end;
if Scale='Attention' then do;
	mean=c0+c1*time + c2*status + c3*time*status  +gamma*u1;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_att)) -0.5*(outcome-mean)**2/(s2e_att);
end;
model outcome~general(ll);
random  u1~normal([0],[g11]) subject=naccid;
run;
proc nlmixed data=s857.tbi_long;
parms b0=-0.13 b1=0.43 b2=0.39 b3=-0.83 b4=0.08 b5=-0.01
s2e_mem=0.361 g11=0.60 g22=.01
	c0=0.05 c1=-0.05  c2=-0.3 c3=-0.1 gamma=1 g33=0.01
s2e_att=0.19;
if Scale='Memory' then do;
	mean=b0+b1*time + b2*status + b3*sex +b4*time*status +b5*time*naccageb  +u1 +u2*time;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_mem)) -0.5*(outcome-mean)**2/(s2e_mem);
end;
if Scale='Attention' then do;
	mean=c0+c1*time + c2*status + c3*time*status  +gamma*u1 +u3*time;
	ll = -0.5*log(3.14159265358) - log(sqrt(s2e_att)) -0.5*(outcome-mean)**2/(s2e_att);
end;
model outcome~general(ll);
random  u1 u2 u3~normal([0,0,0],[g11,0,g22,0,0,g33]) subject=naccid;
run;
proc nlmixed data=s857.tbi_long;
parms b0=-0.13 b1=0.43 b2=0.39 b3=-0.83 b4=0.08 b5=-0.01
s2e_mem=0.361 g11=0.60 g22=.01 g12=0.01
	c0=0.05 c1=-0.05  c2=-0.3 c3=-0.1  
s2e_att=0.19;
if Scale='Memory' then do;
	mean=b0+b1*time + b2*status + b3*sex +b4*time*status +b5*time*naccageb  +u1 ;
	dens = -0.5*log(3.14159265358) - log(sqrt(s2e_mem)) -0.5*(outcome-mean)**2/(s2e_mem);
	ll=dens;
end;
if Scale='Attention' then do;
	mean=c0+c1*time + c2*status + c3*time*status  +u2;
	dens = -0.5*log(3.14159265358) - log(sqrt(s2e_att)) -0.5*(outcome-mean)**2/(s2e_att);
	ll=dens;
end;
model outcome~general(ll);
random  u1 u2~normal([0,0],[g11,g12,g22]) subject=naccid;
run;

/*
if dist='Binary' then do;
		eta=v0 + v1*u1   ;
		p=exp(eta)/(1+exp(eta));
		ll = (outcome)*log(p) + (1-outcome)*log(1-p);
end;
*/
