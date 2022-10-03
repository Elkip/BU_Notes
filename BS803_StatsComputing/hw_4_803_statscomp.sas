libname hw4 'C:\Users\User\Desktop\data\';

proc print data=hw4.exercise4(OBS=100);
run;


proc iml;
/*1 Read only the following variables into a matrix called CG: NACZZMS, NACCLMI, NACCZLMD, NACCDFT, NACCAGEB*/
varNames = {"NACCZMMS", "NACCZLMI", "NACCZLMD", "NACCZDFT", "NACCAGEB"};

use hw4.exercise4(OBS=100);
	read all var varNames INTO CG[colname=varNames];
close hw4.exercise4;

/*2 Using loop, replace the missing codes 99 and -99 for the following neuropsychological scores: NACZZMS, NACCLMI, NACCZLMD and NACCDFT */
do i=1 to 4;
	do j=1 to nrow(CG);
		if (CG[j,i] = 99) 
			then CG[j,i] = .;
		if (CG[j,i] = -99) 
			then CG[j,i] = .;
	end;
end;

*print CG; 

/* 3 Calculate the mean of the scores: NACZZMS, NACCLMI, NACCZLMD and NACCDFT and name the new variable “Cognition”*/ 
scores = CG[,1:4];
cog = scores[,:];

varNames2 = {"NACCZMMS", "NACCZLMI", "NACCZLMD", "NACCZDFT", "NACCAGEB", "Cognition"};
cg2 = CG || cog;
print cg2;


/* 4) Identify the subjects who have mean cognitive score less than -1.5 (Note: SAS thinks that a missing value is equal to –infinity) */
idx = loc(cg2[, 6] < -1.5 & cg2[,6] > -99);
cg3 = CG2[idx,];
*print (idx`)[label="Row"] (CG2[idx,])[c=varNames2];
print cg3;

/* 5) Export the matrix of the row numbers of subjects from question 4 in a dataset called IMPAIRED. */
create IMPAIRED from idx;
append from idx;
close IMPAIRED;

/* 6) Within IML, run a regression (proc REG) with the “Cognition” as the dependent variable and age as the independent variable */
create CG var varNames2;
append from cg2;
close CG;

submit;
proc reg data=CG;
title "Cognition vs Age";
model NACCAGEB = Cognition;
run;
endsubmit;

quit;
