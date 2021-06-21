*South Africa GE Corn Yield Project with Lanier and Aaron;
#delimit;
clear all;
set more off;
set maxvar 32000;
set matsize 8000;


********************************************************;
global dr = "C:\Users\JesseTack\Dropbox\SACorn\rawdata";  
global dl = "C:\Users\JesseTack\Dropbox\SACorn\log";
global dd = "C:\Users\JesseTack\Dropbox\SACorn\data";  
global dt = "C:\Users\JesseTack\Dropbox\SACorn\temp";  
global do = "C:\Users\JesseTack\Dropbox\SACorn\out";  

global dr = "C:\Users\JTack\Dropbox\SACorn\rawdata";  
global dl = "C:\Users\JTack\Dropbox\SACorn\log";
global dd = "C:\Users\JTack\Dropbox\SACorn\data";  
global dt = "C:\Users\JTack\Dropbox\SACorn\temp";  
global do = "C:\Users\JTack\Dropbox\SACorn\out";  

capture log close;
log using "$dl\Regressions.log",replace;

************************************;
** Load data, clean and merge data;
************************************;

use "$dr\All_Maize_1980_2010.dta", clear;
desc;

*drop missing yield and outliers;
tab Yield, missing;
drop if Yield == .;
summ Yield;
*hist Yield;
drop if Yield < 0 | Yield > 30;

*removes spaces to eliminate redundancies;
replace Cultivar = subinstr(Cultivar," ","",.);

gen ND = substr(Cultivar,-1,1);
gen Nm1D = substr(Cultivar,-2,1);
gen Nm2D = substr(Cultivar,-3,1);

gen Wmaize = 0;
replace Wmaize = 1 if ND == "W";

replace Wmaize = 1 if ND == "1"; 
replace Wmaize = 1 if ND == "3"; 
replace Wmaize = 1 if ND == "5"; 
replace Wmaize = 1 if ND == "7"; 
replace Wmaize = 1 if ND == "9"; 

replace Wmaize = 1 if ND == "B" & Nm1D == "1"; 
replace Wmaize = 1 if ND == "B" & Nm1D == "3"; 
replace Wmaize = 1 if ND == "B" & Nm1D == "5"; 
replace Wmaize = 1 if ND == "B" & Nm1D == "7"; 
replace Wmaize = 1 if ND == "B" & Nm1D == "9"; 

replace Wmaize = 1 if ND == "R" & Nm1D == "1"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "3"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "5"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "7"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "9"; 

replace Wmaize = 1 if ND == "R" & Nm1D == "B" & Nm2D == "1"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "B" & Nm2D == "3"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "B" & Nm2D == "5"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "B" & Nm2D == "7"; 
replace Wmaize = 1 if ND == "R" & Nm1D == "B" & Nm2D == "9"; 

drop ND Nm1D Nm2D;

gen breeder = substr(Cultivar,1,3);
forvalues n = 2/3 {;
  gen n`n' = 0;
  forvalues i= 0/9 {;
    replace n`n' = 1 if substr(breeder,`n',1) == "`i'";
  };  
};

gen breed = breeder;
replace breed = substr(breed,1,1) if n2 == 1;
replace breed = substr(breed,1,2) if n3 == 1;

forvalues n = 0/9 {;
forvalues i = 1/20 {;
  gen test`n'_`i' = substr(Cultivar,`i',1) == "`n'";
};
};

egen sum = rowtotal(test*);
drop test*;

replace breed = "ARC" if sum == 0;
drop sum;

replace breed = "SA" if breed == "SA ";
replace breed = upper(breed);

tab breed;

*traits;
gen trait = "A";

gen last2 = substr(Cultivar,length(Cultivar)-1,2);
replace trait = "BR" if last2 == "BR";
replace trait = "BT" if last2 == "BT";

gen last1 = substr(Cultivar,length(Cultivar),1);
replace last1 = "A" if breed == "ARC";

replace trait = "R" if last1 == "R" & breed != "ARC" & last2 != "BR";
replace trait = "B" if last1 == "B" & breed != "ARC" ;

replace trait = "B" if trait == "BT";
tab trait;

drop last*;

*crop year;
tab Season;

split Season, p(_);
rename Season1 Year;
destring Year, replace;
drop Season*;

order Cultivar breed trait Year;


egen GE = group(trait);
tab GE trait;

*drop GE before 1998 since not possible;
tab Year GE;
drop if GE > 1 & Year < 1998;

replace Cultivar = upper(Cultivar);

split Locality, p(-);
split Locality1,p(/);
split Locality11,p("(");

gen location = Locality111;
drop Locality*;
replace location = upper(location);
replace location = subinstr(location," ","",.); 

replace TOTAL_N = . if TOTAL_N == 0;

*just traits ;
gen Bt = 0;
replace Bt = 1 if GE == 2 | GE == 3; 

gen RR = 0;
replace RR = 1 if GE == 3 | GE == 4;

tab GE Bt;
tab GE RR;

gen AGE = 0;
replace AGE = 1 if GE > 1; 

tab Dryland_Irrigation_Complementary;
gen Dryland = 0;
replace Dryland = 1 if Dryland_Irrigation_Complementary == "Dryland";

tab Dryland_Irrigation_Complementary Dryland;

rename location Location;

*see LLNs Locations_CorrectedLLN file for location name corrections;
tab Location;
replace Location = "BOTHAVILLE" if Location == "BOTHAVIILE";
replace Location = "WESSELSBRON" if Location == "WEESELSBRON";
replace Location = "GROBLERSDAL" if Location == "GROBLERDAL";

gen temp = ustrfix(Location, "");
replace Location = "DANIELSRUS" if temp == "DANILSRUS";
replace Location = "DOHNE" if temp == "DHNE";
replace Location = "JIMFOUCHE" if temp == "JIMFOUCH";
drop temp;

*drop missing location;
drop if Location == "H";
drop if Location == "TY";

tab Location, missing;

*add in province data;
preserve;

  use "$dr\sa location province.dta", clear;
  rename location Location;
  rename province Province;
  replace Province = "Free State" if Province == "free state";
  replace Province = "Limpopo" if Province == "limpopo";
  replace Province = "North West" if Province == "north west";
  replace Province = "Northern Cape" if Province == "northern";
  replace Province = "Western Cape" if Province == "western cape";
  tempfile province;
  save `province';

restore;
merge m:1 Location using `province';
keep if _m == 3; drop _m;

*drop if we dont know the province;
drop if Province == ".";

*only locations with multiple years;
by Location Year, sort: gen nvals = _n == 1;
order Location Year nvals;
by Location: replace nvals = sum(nvals);
by Location: replace nvals = nvals[_N];
tab nvals;
drop if nvals < 2;
drop nvals;

*only cultivars with multiple years or locations;
by Cultivar Location Year, sort: gen nvals = _n == 1;
order Cultivar Location Year nvals;
by Cultivar: replace nvals = sum(nvals);
by Cultivar: replace nvals = nvals[_N];
tab nvals;
drop if nvals < 2;
drop nvals;

rename breed Breed;

egen cu = group(Cultivar);
summ cu;
egen br = group(Breed);
summ br;
egen loc = group(Location);
summ loc;
egen pro = group(Province);
summ pro;

summ Year;
gen trend = Year - r(min) + 1;

drop GE;

order Location loc Province pro Year trend Cultivar cu Breed br Wmaize AGE Bt RR Dryland Yield ;
sort Location Year Cultivar Wmaize AGE Bt RR Dryland Yield ;

save "$dd\RegData.dta", replace;
outsheet using "$dd\RegData.xls", comma replace;



************************************;
** Summary Stats and Figures;
************************************;

*Number of trials by Year and summ stats;
use "$dd\RegData.dta", clear;
by Year Location, sort: gen nvals = _n == 1;
by Year: replace nvals = sum(nvals);
by Year: replace nvals = nvals[_N];
gen Ntrials = nvals;
order Year Location Ntrials;
drop nvals;

bys Year : gen Nobs = _N;
order Year Location Ntrials Nobs;

egen Pwhite = mean(Wmaize), by(Year);
egen Pdry = mean(Dryland), by(Year);
egen Pge = mean(AGE), by(Year);
egen Pbt = mean(Bt), by(Year);
egen Prr = mean(RR), by(Year);

order Year Location Ntrials Nobs Pwhite Pdry Pge Pbt Prr;
collapse (mean) Ntrials Nobs Pwhite Pdry Pge Pbt Prr, by(Year);
tostring Year, replace;
tempfile file1;
save `file1';

use "$dd\RegData.dta", clear;
egen Ntrials = group(Location Year);
gen Nobs = _n;

rename Wmaize Pwhite;
rename Dryland Pdry;
rename AGE Pge;
rename Bt Pbt;
rename RR Prr;

drop Year; gen Year = "Total";
order Year Location Ntrials Nobs Pwhite Pdry Pge Pbt Prr;
collapse (max) Ntrials Nobs (mean) Pwhite Pdry Pge Pbt Prr, by(Year);

append using `file1';
sort Year;
list;


*Number of cultivars by Year and summ stats;
use "$dd\RegData.dta", clear;
by Year Cultivar, sort: gen nvals = _n == 1;
by Year: replace nvals = sum(nvals);
by Year: replace nvals = nvals[_N];
gen Ncults = nvals;
order Year Cultivar Ncults;
drop nvals;

collapse (mean) Ncults AGE Bt RR, by(Year Cultivar);
gen Nge = AGE;
gen Nbt = Bt;
gen Nrr = RR;
collapse (mean) Ncults (sum) Nge Nbt Nrr, by(Year);
tostring Year, replace;
tempfile file2;
save `file2';

use "$dd\RegData.dta", clear;
egen Ncults = group(Cultivar);
gen Nobs = _n;

collapse (mean) Ncults AGE Bt RR, by(Cultivar);
gen Nge = AGE;
gen Nbt = Bt;
gen Nrr = RR;

gen Year = "Total";
order Year Cultivar Ncults AGE Bt RR;
collapse (max) Ncults (sum) Nge Nbt Nrr, by(Year);

append using `file2';
sort Year;
list;



*number of trials by cultivar;
use "$dd\RegData.dta", clear;
by Cultivar Location Year, sort: gen nvals = _n == 1;
by Cultivar: replace nvals = sum(nvals);
by Cultivar: replace nvals = nvals[_N];
collapse (mean) nvals, by(Cultivar);
gsort -nvals;
gen obs = _n;
summ obs ;
local min = r(min) - 1;
local max = r(max);

twoway bar nvals obs, scheme(s2mono)
      title("All Cultivars") ytitle("Trials") 
	  ylabel(, nogrid angle(horizontal)) ysca(titlegap(0)) xlabel(`min'(100)`max') xtitle("Cultivars")
	  graphregion(color(white)) ;
graph save "$dt/CultTrials", replace;
summ obs nvals;

*number of trials by cultivar, conventional;
use "$dd\RegData.dta", clear;
keep if AGE == 0;
by Cultivar Location Year, sort: gen nvals = _n == 1;
by Cultivar: replace nvals = sum(nvals);
by Cultivar: replace nvals = nvals[_N];
collapse (mean) nvals, by(Cultivar);
gsort -nvals;
gen obs = _n;
summ obs;
local min = r(min) - 1;
local max = r(max);

twoway bar nvals obs, scheme(s2mono)
      title("Conventional Cultivars") ytitle("Trials") 
	  ylabel(, nogrid angle(horizontal)) ysca(titlegap(0)) xlabel(`min'(100)`max') xtitle("Cultivars")
	  graphregion(color(white)) ;
graph save "$dt/CultTrialsCB", replace;
summ obs nvals;

*number of trials by cultivar, GE;
use "$dd\RegData.dta", clear;
keep if AGE == 1;
by Cultivar Location Year, sort: gen nvals = _n == 1;
by Cultivar: replace nvals = sum(nvals);
by Cultivar: replace nvals = nvals[_N];
collapse (mean) nvals, by(Cultivar);
gsort -nvals;
gen obs = _n;
summ obs;
local min = r(min) - 1;
local max = r(max);

twoway bar nvals obs, scheme(s2mono)
      title("GE Cultivars") ytitle("Trials") 
	  ylabel(, nogrid angle(horizontal)) ysca(titlegap(0)) xlabel(`min'(20)`max') xtitle("Cultivars")
	  graphregion(color(white)) ;
graph save "$dt/CultTrialsGE", replace;
summ obs nvals;

gr combine "$dt\CultTrials" "$dt\CultTrialsCB" "$dt\CultTrialsGE", 
		   rows(2) cols(2) name(CultTrialsAll, replace) 
		   graphregion(fcolor(white));
graph export "$do\CultTrialsAll.png", replace;


*yield variation;
use "$dd\RegData.dta", clear;
summ Yield;
bys Dryland: summ Yield;
bys Wmaize: summ Yield;
bys AGE: summ Yield;
bys AGE: summ Yield if Year >= 1999;

gen zero = Yield == 0;
summ zero;


summ Yield;
local avg = r(mean);
graph box Yield, over(Year, sort(ord) label(angle(90) labsize(small))) yline(`avg')
marker(1, mfcolor(none) msize(small)) graphregion(color(white)) 
ytitle("Maize Yield (MT/hectacre)") ylabel(, nogrid) ysca(titlegap(0)) 
ylabel(, angle(horizontal)) scheme() name(Yield, replace) title("(a)")  ;
graph save "$dt\Yield", replace;

twoway (kdensity Yield if Dryland == 1, lc(red)) (kdensity Yield if Dryland == 0, lc(blue)) ,
graphregion(color(white)) legend( order(1 "Dryland" 2 "Irrigated") pos(1) ring(0) row(2))
xtitle("Maize Yield, MT/hectacre") xlabel(0(2)20) ylabel(, nogrid) ysca(titlegap(0)) ytitle("Density") scheme(s2color)
name(YieldDry, replace) title("(b)") ;
graph save "$dt\YieldDry", replace;

twoway (kdensity Yield if Wmaize == 0, lc(red)) (kdensity Yield if Wmaize == 1, lc(blue)) ,
graphregion(color(white)) legend( order(1 "Yellow" 2 "White") pos(1) ring(0) row(2))
xtitle("Maize Yield, MT/hectacre") xlabel(0(2)20) ylabel(, nogrid) ysca(titlegap(0)) ytitle("Density") scheme(s2color)
name(YieldColor, replace) title("(c)") ;
graph save "$dt\YieldColor", replace;

twoway (kdensity Yield if AGE == 0, lc(red)) (kdensity Yield if AGE == 1, lc(blue)),
graphregion(color(white)) legend( order(1 "CH" 2 "GM") pos(1) ring(0) row(5))
xtitle("Maize Yield, MT/hectacre") xlabel(0(2)20) ylabel(, nogrid) ysca(titlegap(0)) ytitle("Density") scheme(s2color)
name(YieldGE, replace) title("(d)") ;
graph save "$dt\YieldGE", replace;

twoway (kdensity Yield if AGE == 0 & Wmaize == 0, lc(red))
       (kdensity Yield if AGE == 0 & Wmaize == 1, lc(blue))
	   (kdensity Yield if AGE == 1 & Wmaize == 0, lc(orange))
	   (kdensity Yield if AGE == 1 & Wmaize == 1, lc(green)),
graphregion(color(white)) legend( order(1 "CH yellow" 2 "CH white" 3 "GM yellow" 4 "GM white") pos(1) ring(0) row(5))
xtitle("Maize Yield, MT/hectacre") xlabel(0(2)20) ylabel(, nogrid) ysca(titlegap(0)) ytitle("Density") scheme(s2color)
name(YieldGE, replace) title("(d)") ;
graph save "$dt\YieldGE2", replace;


gr combine "$dt\Yield" "$dt\YieldDry" "$dt\YieldColor" "$dt\YieldGE2", 
		   rows(2) cols(2) imargin (0 0 0 0) name(YieldDryColorGE2, replace) scheme(lean2)
		   graphregion(color(white));
graph export "$do\YieldDryColorGE2.png", replace;
graph export "$do\YieldDryColorGE2.eps", replace;


************************************;
** Regression Results;
************************************;

*some bt vs rr distinctions;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);

gen justRR = 0;
replace justRR = 1 if RR == 1 & Bt == 0;

gen justBt = 0;
replace justBt = 1 if Bt == 1 & RR == 0;

gen stacked = 0;
replace stacked = 1 if Bt == 1 & RR == 1;


reg Yield i.loc i.Year 1.RR 1.Bt if Dryland==1, cluster(Year);
test 1.RR = 1.Bt;


*Main result and robust;
*homogeneous effect, dryland;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
estimates store m1, title(Model 1);
keep if Dryland==1;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*province X year FE;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year#pro 1.AGE if Dryland==1, cluster(Year);
estimates store m2, title(Model 2);
keep if Dryland==1;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only years since 1999;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & Year >= 1999, cluster(Year);
estimates store m3, title(Model 3);
keep if Dryland==1 & Year >= 1999;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only Cultivars since 1999;
use "$dd\RegData.dta", clear;
bys Cultivar: egen YearEnt = min(Year);
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & YearEnt >= 1999, cluster(Year);
estimates store m4, title(Model 4);
keep if Dryland==1 & YearEnt >= 1999;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only year since 2005;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & Year >= 2005, cluster(Year);
estimates store m5, title(Model 5);
keep if Dryland==1 & Year >= 2005;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only Cultivars since 2005;
use "$dd\RegData.dta", clear;
bys Cultivar: egen YearEnt = min(Year);
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & YearEnt >= 2005, cluster(Year);
estimates store m6, title(Model 6);
keep if Dryland==1 & YearEnt >= 2005;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only non-zero yields;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & Yield > 0, cluster(Year);
estimates store m7, title(Model 7);
keep if Dryland==1 & Yield > 0;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only if loc ever had GE;
use "$dd\RegData.dta", clear;
bys loc: egen HGE = total(AGE);
order loc AGE HGE;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & HGE > 0, cluster(Year);
estimates store m8, title(Model 8);
keep if Dryland==1 & HGE > 0;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*only trial has GE;
use "$dd\RegData.dta", clear;
egen trial = group(Location Year); 
bys trial: egen HGE = total(AGE);
order trial AGE HGE;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & HGE > 0, cluster(Year);
estimates store m9, title(Model 9);
keep if Dryland==1 & HGE > 0;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*input controls;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE TOTAL_N Row_width Plant_population if Dryland==1, cluster(Year);
estimates store m10, title(Model 10);
keep if Dryland==1 & TOTAL_N != . & Row_width != . & Plant_population != .;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

estout m* using "$do\RegOutput.xls", replace cells(b(star fmt(3)) se(fmt(3))) legend label varlabels(_cons constant)
   stats(r2 , fmt(3) label(R-sqr)) ;
   

*some mgmt interactions;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE TOTAL_N Row_width Plant_population if Dryland==1, cluster(Year);

summ Plant_population;

gen s_pop = (Plant_population - r(mean))/r(sd); 

reg Yield i.loc i.Year 1.AGE 1.AGE#c.s_pop TOTAL_N Row_width Plant_population if Dryland==1, cluster(Year);


***************************
*Yield levels prior to GE;

use "$dd\RegData.dta", clear;
reg Yield c.Year if Dryland==1 & AGE == 0, cluster(Year);
lincom _b[_cons] + _b[Year]*1998;
summ Yield if Dryland==1 & AGE == 0;
summ Yield if Dryland==1 & Year <= 1998;
summ Yield if Dryland==1 & Year <= 1998 & Year >=1994;

reg Yield i.Year if Dryland==1 & Year <= 1998, cluster(Year);
lincom _b[_cons] + (1994.Year+1995.Year+1996.Year+1997.Year+1998.Year)/5;

*semi log see Giles post https://davegiles.blogspot.com/2011/03/dummies-for-dummies.html;
gen ly = log(Yield);
reg ly i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
nlcom 100*[exp(_b[1.AGE]) - 1];
nlcom 100*[exp(_b[1.AGE] - .5*(_se[1.AGE])^2) - 1];



***************************
*Heterogeneous effects;

*homogeneous effect, dryland;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
estimates store m1, title(Model 1);
keep if Dryland==1;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*pooled with irrigated effect;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year#Dryland 0.Dryland 1.AGE#0.Dryland 1.AGE#1.Dryland, cluster(Year);
test 1.AGE#0.Dryland = 1.AGE#1.Dryland ;
estimates store m4, title(Model 4);
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*Bt vs RR heterogeneous effect, dryland;
use "$dd\RegData.dta", clear;
gen Btonly = 0;
replace Btonly = 1 if Bt == 1 & RR == 0;
gen RRonly = 0;
replace RRonly = 1 if Bt == 0 & RR == 1;
reg Yield i.loc i.Year 1.AGE 1.Btonly 1.RRonly if Dryland==1, cluster(Year);
estimates store m3, title(Model 3);
test 1.Btonly = 1.RRonly = 0;
keep if Dryland==1;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

*white maize heterogeneous effect, dryland;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.Wmaize 1.AGE#0.Wmaize 1.AGE#1.Wmaize if Dryland==1, cluster(Year);
estimates store m2, title(Model 2);
test 1.AGE#0.Wmaize = 1.AGE#1.Wmaize;
keep if Dryland==1;
egen Nloc = group(Location);
egen Ncult = group(Cultivar);
egen Nyears = group(Year);
qui: summ Yield;
gen Nobs = r(N); 
summ Nyears Nloc Ncult Nobs;

estout m* using "$do\RegOutputHetero.xls", replace cells(b(star fmt(3)) se(fmt(3))) legend label varlabels(_cons constant)
   stats(r2 , fmt(3) label(R-sqr)) ;


*GM catch up for white, note the differential effect in the regression is the same as the mean yield differences prior to GM;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.Wmaize 1.AGE#0.Wmaize 1.AGE#1.Wmaize if Dryland==1, cluster(Year);

summ Yield if Wmaize == 1 & Dryland==1 & Year < 1999 & AGE == 0;
summ Yield if Wmaize == 0 & Dryland==1 & Year < 1999 & AGE == 0;
   
   
*province specific effects, ignore the northern cape b/c only one dryland loc and GE never tested;
*see this: reg Yield i.loc i.Year 1.AGE if Dryland==1 & pro == 8, cluster(Year);
*use "$dd\RegData.dta", clear;
*keep if pro == 8;
*tab AGE if Dryland == 1;

use "$dd\RegData.dta", clear;
summ Yield if Dryland==1;
gen R2 = .;
	
forvalues p = 1/9 {;
  tab Province if pro == `p';
  if `p' != 8 {;  
	qui: reg Yield i.loc i.Year 1.AGE if Dryland==1 & pro == `p', cluster(Year); 
	estimates store mh`p', title(Model `p');
	replace R2 = e(r2) if _n == `p';
  };
};
summ R2;

estout mh1 mh2 mh3 mh4 mh5 mh6 mh7 mh9  using "$do\RegOutputProvince.xls", replace cells(b(star fmt(3)) se(fmt(3))) legend label varlabels(_cons constant)
   stats(r2 , fmt(3) label(R-sqr)) ;

   


***************************
*More White vs Yellow Heterogeneous effects;

*split sample;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1 & Wmaize == 1, cluster(Year);
reg Yield i.loc i.Year 1.AGE if Dryland==1 & Wmaize == 0, cluster(Year);


*rank the cultivars for each of the four types if the last five years;
use "$dd\RegData.dta", clear;
gen type = "XX";
replace type = "WC" if Wmaize == 1 & AGE == 0;  
replace type = "WG" if Wmaize == 1 & AGE == 1;  
replace type = "YC" if Wmaize == 0 & AGE == 0;  
replace type = "YG" if Wmaize == 0 & AGE == 1;  
tab type;
keep if Year >= 2005;
save "$dt\RegDataType.dta", replace;


foreach ty in "WC" "WG" "YC" "YG" {;
  use "$dt\RegDataType.dta", clear;
  keep if Dryland==1 & type =="`ty'";
  egen tcu = group(cu);
  reg Yield i.loc i.Year i.tcu , cluster(Year);
  gen fe = .;
  capture: replace fe = _b[1b.tcu] if tcu == 1;
  summ tcu;
  local max = r(max);
  forvalues i = 2/`max' {;
    capture: replace fe = _b[`i'.tcu] if tcu == `i';
  };
  drop if fe == .;
  collapse (mean) fe , by(cu Cult type);
  egen rank = rank(-fe), by(type);
  sort type rank;
  keep cu type rank;
  if ty == "WC" {; save "$dt\RankYields.dta", replace; };
  if ty != "WC" {; append using "$dt\RankYields.dta"; save "$dt\RankYields.dta", replace; };
};

tab rank;
*note that the rank is clean ;

use "$dt\RegDataType.dta", clear;
merge m:1 cu using "$dt\RankYields.dta";
*some in the master dont get matched because those cults only ever in irrigated;
keep if _m == 3;
save "$dt\RegDataTypeRanked.dta", replace;



*split sample last five years top ranks;
use "$dt\RegDataTypeRanked.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1 & Wmaize == 1 & rank <= 5, cluster(Year);
reg Yield i.loc i.Year 1.AGE if Dryland==1 & Wmaize == 0 & rank <= 5, cluster(Year);



*best WC vs highest ranked WG in those years;
forvalues Wrank = 1/10 {;

  use "$dt\RegDataTypeRanked.dta", clear;
  keep if Dryland == 1;
  keep if Wmaize == 1;
  drop if type == "WC" & rank != `Wrank';

  *keep years of WC;
  levelsof Year if type == "WC", local(Ylevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if Year == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab Year;

  *keep locations of WC;
  levelsof lo if type == "WC", local(Llevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if lo == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab lo;

  summ rank if type == "WC";
  local mrankWC = r(mean);
  display `mrankWC';
  summ rank if type == "WG";
  local mrankWG = r(min);
  display `mrankWG';
  drop if type == "WG" & rank > `mrankWG';
  summ rank if type == "WC";
  summ rank if type == "WG";

  *only keep location/years where both are there;
  *location years for WC;
  egen gr = group(lo Year);
  levelsof gr if type == "WC", local(Glevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if gr == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab gr; drop gr;

  *location years for WG;
  egen gr = group(lo Year);
  levelsof gr if type == "WG", local(Glevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if gr == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab gr; drop gr;
  
  reg Yield i.loc i.Year 1.AGE if Dryland==1 , ;
  gen effect = _b[1.AGE];
  gen RWC = `mrankWC';
  gen RWG = `mrankWG';
  summ Yield ;
  gen nobs = r(N);
  summ Yield if type == "WC";
  gen AYWC = r(mean);
  summ Yield if type == "WG";
  gen AYWG = r(mean);
  egen l = group(lo);
  summ l;
  gen Lnobs = r(max);
  egen y = group(Year);
  summ y;
  gen Ynobs = r(max);
  levelsof Year , local(NYlevels);

  collapse (mean) RWC RWG AY* effect *nobs;
  gen years = "`NYlevels'";

  if `Wrank' == 1 {; save "$dt\WhiteMaizeComparisons.dta", replace; };
  if `Wrank' != 1 {; append using "$dt\WhiteMaizeComparisons.dta" ; sort RWC; save "$dt\WhiteMaizeComparisons.dta", replace; };

}; 


*best YC vs highest ranked YG in those years;
forvalues Yrank = 1/10 {;

  use "$dt\RegDataTypeRanked.dta", clear;
  keep if Dryland == 1;
  keep if Wmaize == 0;
  drop if type == "YC" & rank != `Yrank';

  *keep years of YC;
  levelsof Year if type == "YC", local(Ylevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if Year == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab Year;

  *keep locations of YC;
  levelsof lo if type == "YC", local(Llevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if lo == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab lo;

  summ rank if type == "YC";
  local mrankYC = r(mean);
  display `mrankYC';
  summ rank if type == "YG";
  local mrankYG = r(min);
  display `mrankYG';
  drop if type == "YG" & rank > `mrankYG';
  summ rank if type == "YC";
  summ rank if type == "YG";

  *only keep location/years where both are there;
  egen gr = group(lo Year);
  levelsof gr if type == "YC", local(Glevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if gr == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab gr; drop gr;
  
  *location years for YG;
  egen gr = group(lo Year);
  levelsof gr if type == "YG", local(Glevels);
  gen k = 0;
  foreach l in `r(levels)' {;
    replace k = 1 if gr == `l';
  };
  tab k;
  keep if k == 1; drop k;
  tab gr; drop gr;


  reg Yield i.loc i.Year 1.AGE if Dryland==1 , ;
  gen effect = _b[1.AGE];
  gen RYC = `mrankYC';
  gen RYG = `mrankYG';
  summ Yield ;
  gen nobs = r(N);
  summ Yield if type == "YC";
  gen AYYC = r(mean);
  summ Yield if type == "YG";
  gen AYYG = r(mean);
  egen l = group(lo);
  summ l;
  gen Lnobs = r(max);
  egen y = group(Year);
  summ y;
  gen Ynobs = r(max);
  levelsof Year , local(NYlevels);

  collapse (mean) RYC RYG AY* effect *nobs;
  gen years = "`NYlevels'";

  if `Yrank' == 1 {; save "$dt\YellowMaizeComparisons.dta", replace; };
  if `Yrank' != 1 {; append using "$dt\YellowMaizeComparisons.dta" ; sort RYC; save "$dt\YellowMaizeComparisons.dta", replace; };

}; 



*************************;
*Risk effects;

*******;
*variance;
use "$dd\RegData.dta", clear;
reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
estimates store m1, title(Mean);
local beta = _b[1.AGE]; display `beta';
predict res, res;
summ res if Dryland==1;
gen res2 = res*res;
reg res2 i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
estimates store m2, title(Variance);
local delt = _b[1.AGE]; display `delt';
drop res*;

*mean yields prior to GE;
preserve;
  reg Yield i.loc if Dryland==1 & Year < 1999;
  tempfile estimates;
  parmest, format(estimate) saving(`estimates');
  use `estimates', clear;
  summ estimate if parm == "_cons";
  gen unot = estimate + r(mean);
  drop if parm == "_cons";
  summ unot;
  local unot = r(mean); display `unot';
restore;

*variance yields prior to GE;
qui: reg Yield i.loc if Dryland==1 & Year < 1999;
predict res, res;
gen res2 = res*res;
reg res2 if Dryland==1 & Year < 1999, ;
local vnot = _b[_cons]; display `vnot';
drop res*;


*******;
*CV calculation ;
display `beta';
display `delt';
display `vnot';
display `unot';

local CVnot = [sqrt(`vnot')]/(`unot');
display `CVnot';

local CVnew = [sqrt(`delt' + `vnot')]/(`beta' + `unot');
display `CVnew';

local CVdiff = [sqrt(`delt' + `vnot')]/(`beta' + `unot') - [sqrt(`vnot')]/(`unot');
display `CVdiff';

local CVchange = 100*[[sqrt(`delt' + `vnot')]/(`beta' + `unot') - [sqrt(`vnot')]/(`unot')]/[sqrt(`vnot')]/(`unot');
display `CVchange';

*******;
*Skewness;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
local beta = _b[1.AGE]; display `beta';
predict res, res;
summ res if Dryland==1;
gen res3 = res*res*res;
summ res3 if Dryland==1;
reg res3 i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
estimates store m3, title(Skewness);
local gamm = _b[1.AGE]; display `gamm';
drop res*;

*skewness yields prior to GE;
qui: reg Yield i.loc if Dryland==1 & Year < 1999;
predict res, res;
gen res3 = res*res*res;
reg res3 if Dryland==1 & Year < 1999, ;
local m3not = _b[_cons]; display `m3not';

display `beta';
display `delt';
display `vnot';
display `unot';
display `gamm';
display `m3not';

local SKnot = `m3not'/[(`vnot')^(3/2)] ;
display `SKnot';

local SKnew = (`gamm' + `m3not')/[(`delt' + `vnot')^(3/2)];
display `SKnew';

local SKdiff = `SKnew' - `SKnot' ;
display `SKdiff';

local SKchange = 100*(`SKnew' - `SKnot')/`SKnot';
display `SKchange';


*******;
*Cost of risk calc;

local CRKnot = (3/2)*(`vnot'/`unot') - 2*(`m3not'/(`unot'^2));
display `CRKnot';

local CRKnew = (3/2)*((`delt'+`vnot')/(`beta'+`unot')) - 2*((`gamm'+`m3not')/((`beta'+`unot')^2));
display `CRKnew';

local CRKdiff = `CRKnew' - `CRKnot' ;
display `CRKdiff';

local CRKchange = 100*(`CRKnew' - `CRKnot')/`CRKnot';
display `CRKchange';


*******;
*lower semi-variance;
use "$dd\RegData.dta", clear;
qui: reg Yield i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
predict res, res;
summ res if Dryland==1;
gen Lsemires2 = res*res if Dryland==1 & res < 0;
gen Usemires2 = res*res if Dryland==1 & res > 0;

*reg Lsemires2 i.loc i.Year 1.AGE if Dryland==1, cluster(Year);
*reg Usemires2 i.loc i.Year 1.AGE if Dryland==1, cluster(Year);

gen res2 = res*res;
gen U = 0;
replace U = 1 if res > 0;

reg res2 i.loc#i.U i.Year#i.U 1.U 0.U#1.AGE 1.U#1.AGE if Dryland==1, cluster(Year);
estimates store m4, title(Semivariance);


estout m*  using "$do\RegOutputRisk.xls", replace cells(b(star fmt(3)) se(fmt(3))) legend label varlabels(_cons constant)
   stats(r2 , fmt(3) label(R-sqr)) ;



*back of envelope rate calc based on log normals;
local beta = .42322654;
local delt = .26142478;
local vnot = 3.3088264;
local unot = 4.8853149;

local m0 = `unot';
local s0 = sqrt(`vnot');

local m1 = `beta' + `unot';
local s1 = sqrt(`delt' + `vnot');

display `m0';
display `m1';
display `s0';
display `s1';

clear all;
set obs 1000000;

local u0 = log(`m0'^2 / (sqrt(`s0'^2 + `m0'^2)));
local v0 = sqrt(log(`s0'^2/`m0'^2 + 1)) ;

gen y0LN = exp(`u0' + `v0' * invnormal(uniform()));
summ y0LN;

local u1 = log(`m1'^2 / (sqrt(`s1'^2 + `m1'^2)));
local v1 = sqrt(log(`s1'^2/`m1'^2 + 1)) ;

gen y1LN = exp(`u1' + `v1' * invnormal(uniform()));
summ y1LN;

set seed 100;

forvalues i = 0/1 {;
    egen mean`i' = mean(y`i'LN);
    egen std`i' = sd(y`i'LN);
    egen skew`i' = skew(y`i'LN);
    gen cov`i' = (85/100)*mean`i';
    gen indem`i' = 0 ;
    replace indem`i' = (cov`i' - y`i'LN) if y`i'LN <= cov`i';
    egen Eindem`i' = mean(indem`i');
    gen rate`i' = (Eindem`i'/(85/100))/cov`i';
};

summ mean* std* skew* rate*;



log close;


