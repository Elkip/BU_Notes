libname HW11 "Z:\";

data drvrdth;
    set HW11.ukdriverdeaths;
	deaths_1 = deaths - lag1(deaths);
	deaths_2 = deaths - lag2(deaths);
	deaths_s = deaths - lag12(deaths);
	deaths_12 = deaths_1 - lag12(deaths_1);
run;

/* 1. Plot the data and ACF and PACF */
proc arima data = drvrdth;
identify var = deaths nlag = 24;
run;

title 'Deaths_12, ACF, and PACF';
proc arima data = drvrdth;
identify var = deaths_12 nlag = 24;
run;

title 'Seasonalized Monthly Deaths';
proc sgplot data = drvrdth;
series x = date y = deaths / markers;
run;

*Plot the deseasonalized series;
title 'Deseasonalized Monthly Deaths';
proc sgplot data = drvrdth;
series x = date y = deaths_12 / markers;
run;

*2. Fit an ARMA model to the stationary series;
*(p=0, d=1, q=1)(P=0, D=1, Q=1);
title 'MA(1) model';
proc arima data = drvrdth;
	identify var = deaths_12 nlag = 24;
	estimate q = (1)(12) noint method = ml;
run;

*(p=0, d=1, q=2)(P=0, D=1, Q=1);
title 'MA(2) model';
proc arima data = drvrdth;
	identify var = deaths_12 nlag = 24;
	estimate q = (1 2)(12) noint method = ml;
run;

*(p=1, d=1, q=2)(P=1, D=1, Q=1);
title 'ARMA model';
proc arima data = drvrdth;
	identify var = deaths_12 nlag = 24;
	estimate p = (1)(12) q = (1 2)(12) noint method = ml;
run;

*(p=2, d=1, q=2)(P=1, D=1, Q=1);
title 'ARMA model';
proc arima data = drvrdth;
	identify var = deaths_12 nlag = 24;
	estimate p = (1 2)(12) q = (1 2)(12) noint method = ml;
run;

* 3. Create seat belt dummy variable to include in model;
data drvrdth;
	set drvrdth;
	if  date>'01JAN1983'D then Seat_Belt=1;
	else Seat_Belt=0;
run;

title 'Adjusted Model';
proc arima data = drvrdth;
	identify var = deaths_12 nlag = 24 crosscorr= Seat_Belt(1);
	estimate q = (1 2)(12) input = (Seat_Belt) noint method = ml;
run;
