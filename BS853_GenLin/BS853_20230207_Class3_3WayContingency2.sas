/* Parameter models */

%let L0=1;
%let LX=2; 
%let LY=1;
%let LZ=3;
%let LXY=-1;
%let LXZ=1;
%let LYZ=-2;



/*** Single data ***/
data one;
do X = 1 to 2;
 do Y = 1 to 2;
  do Z = 1 to 2;

  Yi=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1))); * Mutual ind model;
  Yij=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1)+&LXY*(X=1)*(Y=1))); * Jpoint independence model;
  Y2=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1)+&LXY*(X=1)*(Y=1) 
+&LXZ*(X=1)*(Z=1)+&LYZ*(Z=1)*(Y=1))); * 2-way int model;
  output;
  end;
 end;
end;
run;


/*** Multiple data (Simulation) ***/
%let nSim=100;
data Multi;
do X = 1 to 2;
 do Y = 1 to 2;
  do Z = 1 to 2;
  do Simulation = 1 to &nSim;
  Yi=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1))); * Mutual ind model;
  Yij=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1)+&LXY*(X=1)*(Y=1))); * Jpoint independence model;
  Y2=rand('poisson',exp(&L0+&LX*(X=1)+&LY*(Y=1)+&LZ*(Z=1)+&LXY*(X=1)*(Y=1) 
+&LXZ*(X=1)*(Z=1)+&LYZ*(Z=1)*(Y=1))); * 2-way int model;
  output;
  end;
  end;  
 end;
end;
run;
proc sort data=Multi;
 by Simulation;
 run;
title Testing mutual independce using mutual independence model;
ods output ModelFit=ind(where = (Criterion in ('Deviance','Pearson Chi-Square'))  keep = Simulation Criterion DF Value);
proc genmod data=multi;
 by simulation;
 model Yi=X Y Z/d=poisson type3;
run;
data ind;set ind; pval=1-cdf('Chisq',Value, DF);Label pval='P-Value';run;
Title1 Summary of testing (TRUE model mutual independence);
proc means data=ind mean std;
 var  Value pval;
 class criterion;
 run;
title Testing mutual independce using joint independence model;
ods output ModelFit=nind(where = (Criterion in ('Deviance','Pearson Chi-Square'))  keep = Simulation Criterion DF Value);
proc genmod data=multi;
 by simulation;
 model Yij=X Y Z/d=poisson type3;
run;
data nind;set nind; pval=1-cdf('Chisq',Value, DF);Label pval='P-Value';run;
Title1 Summary of testing (TRUE model joint independence);
proc means data=nind mean std;
 var  Value pval;
 class criterion;
 run;
