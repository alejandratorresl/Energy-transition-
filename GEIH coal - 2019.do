************************************************************************
* Coal sector labor estimations
************************************************************************

global dir "" // Working directory

cd "$dir"


use geih2019personas.dta, clear

* Department labels
destring depto, replace
label define dept 5 "Antioquia" 8 "Atlántico" 11 "Bogotá" 13 "Bolívar" 15 "Boyacá" 17 "Caldas" 18"Caquetá" 19 "-Cauca" 20 "Cesar" 23 "Córdoba" 25 "Cundinamarca" 27 "Chocó" 41 "Huila" 44 "La Guajira" 47 "Magdalena" 50 "Meta" 52 "Nariño" 54 "Norte de Santander" 63 "Quindío"66 "Risaralda" 68 "Santander" 70 "Sucre" 73 "Tolima" 76 "Valle del Cauca"  
label values depto dept

* Only the 10 departments with the most coal sector workers
*keep if depto==15 | depto==44 | depto==5 | depto==54 | depto==20 | depto==11 | depto==19 | depto==47 | depto==25 | depto==8


* Sector according to ISIC	
		
destring sector*, replace

drop sector

gen sector =.

* CIIU 3.0 revision 

replace sector = 0 if sector2d == 0
replace sector = 1 if sector2d == 1 | sector2d == 2 | sector2d == 5
replace sector = 2 if sector2d == 10 | sector2d == 11 | sector2d == 12 | sector2d == 13 | sector2d == 14
replace sector = 3 if sector2d > 14 & sector2d < 38
replace sector = 4 if sector2d == 40 
replace sector = 5 if sector2d == 41
replace sector = 6 if sector2d == 45
replace sector = 7 if sector2d == 50 | sector2d == 51 | sector2d == 52
replace sector = 8 if sector2d >= 60 & sector2d <= 64
replace sector = 9 if sector2d == 55
replace sector = 10 if sector2d == 64
replace sector = 11 if sector2d == 65 | sector2d == 66 | sector2d == 67
replace sector = 12 if sector2d == 70 
replace sector = 13 if sector2d >= 71 & sector2d <= 73
replace sector = 14 if sector2d == 74
replace sector = 15 if sector2d == 75
replace sector = 16 if sector2d == 80
replace sector = 17 if sector2d == 85
replace sector = 18 if sector2d == 92
replace sector = 19 if sector2d == 93
replace sector = 20 if sector2d == 95
replace sector = 21 if sector2d == 99
replace sector = 22 if sector2d == 90
replace sector = 23 if sector2d == 91

		
label define sector 0 "No information" 1 "Agriculture, cattle raising, hunting, forestry and fishing" 2 "Mining" 3 "Manufacturing" 4 "Electricity, gas, vapor and air conditioning"  5 "Water distribution, evacuation and treatment of sewage water" 6 "Construction" 7 "Commerce and vehicle repair" 8 "Transportation and storage" 9 "Accommodation and food services" 10 "Information and communications" 11 "Financial and insurance activities" 12 "Real estate" 13 "Professional, scientific and technic activities" 14 "Administrative services" 15	"Public administration and defense, mandatory social security plans" 16 "Education" 17 "Health and social assistance" 18 "Arts, entertainment and recreation" 19 "Other services" 20 "Household activities as employers, self consumption production" 21 "Extraterritorial organizations and entities activities" 22 "Waste elimination and sanitation" 23 "Association activities"
label values sector sector
	
* Drop NA values on occupied
drop if oc==.

***Variables

		* Occupied
		
		bys sector: egen occupied_sector=sum(oc*fex_dpto) // Occupied by sector
		replace occupied_sector=occupied_sector/1000
		replace occupied_sector=round(occupied_sector,1) 
		bys dpto: egen occupied_dpto=sum(oc*fex_dpto)
		replace occupied_dpto=occupied_dpto/1000
		replace occupied_dpto=round(occupied_dpto,1) // Occupied by department
		drop occupied
		bysort mes: egen occupied=sum(oc*fex_dpto)
		replace occupied = occupied/1000
		replace occupied=round(occupied,1) // Total number of occupied

		save geihpre.dta, replace
	
	
	
use geihpre.dta, clear

* Occupied by sector / Occupied by department	
gen prop = occupied_sector/occupied_dpto

** Coal sector
gen coal = .

replace coal = 1 if sector4d == 1010 | sector4d == 1020 | sector4d == 1030 | sector4d == 2310 | sector4dp8 == "1010" | sector4dp8 == "1020" | ///
sector4dp8 == "1030" | sector4dp8 == "2310"
replace coal = 0 if coal == .
gen coke = .
replace coke = 1 if sector4d == 2310 | sector4dp8 == "2310"
replace coke = 0 if coke == .

* Add indirect jobs: Take activities related to coal mining
gen coal_related=.
replace coal_related = 1 if sector4d == 2310 | sector4d==2924 | sector4d==5161 | sector4d==7514 | sector4d==2699 | sector4d==2924 | ///
sector4d==5151 | sector4d==5237 | sector4d==1010 

replace coal_related = 0 if coal_related == .

* Mining and quarrying 
gen mining = oc * fex_dpto if sector == 2

* Mining and quarrying dummy
gen miningdummy = 1 if sector == 2
replace miningdummy = 0 if miningdummy == .

global cats "coal coal_related mining coke"

* Weighted number of workers and proportions
foreach a in $cats{
gen empleo`a'1 = oc * fex_dpto if `a' == 1
replace empleo`a'1 = 0 if empleo`a' == .

gen prop`a' = prop * `a' *100 // Proportion of total workers
}

format empleocoal1 empleocoal2 mining %20.0g

save geih_coal.dta, replace


* Plot proportions
* Coal
graph bar propcoal, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Proportion of workers in the coal sector") title("Direct workers in the coal sector/Total occupation by department") name(p1, replace)
graph export p1.png, replace
* Direct and indirect workers
graph bar propcoal2, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Proportion of workers in the coal sector") title("Direct and indirect workers in the coal sector/"  "Total occupation by department") name(p2, replace)
graph export p2.png, replace
* Mining and quarrying
graph bar propmining, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Proportion of workers in the mining sector") title("Workers in the mining sector/Total occupation by department") name(p3, replace)
graph export p3.png, replace

* Levels
* Coal
preserve
graph bar (sum) empleocoal1, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Workers in the coal sector") title("Workers in the coal sector") name(n1, replace)
graph export n1.png, replace
restore
* Direct and indirect
graph bar (sum) empleocoal2, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Workers in the coal sector") title("Direct and indirect workers in the coal sector") name(n2, replace)
graph export n2.png, replace
* Mining and quarrying
graph bar (sum) mining, over(depto, label(angle(90) labsize(small))) graphregion(fcolor(white)) ylabel(, nogrid) ytitle("Workers in the mining sector") title("Workers in the mining sector") name(n3, replace)
graph export n3.png, replace


*/
**Collapse 
* Make sure that the numbers are the same as the ones shown on the graphs

preserve
foreach a in $cats{
	gen emp`a' = oc if `a' == 1
}
bys depto: egen emp_dpto = sum(oc)
* Employees for each group
collapse (count) empcoal empcoal_related empmining empcoke oc [iw=fex_dpto], by(depto)

export excel using tabla19.xlsx, firstrow(variables) replace
restore

****** Informality
* Informality is defined according to social security contributions: P6920. 
* 1 if contributes, 2 if does not, 3 if it's already retired

* Coal
preserve

foreach a in $cats{
	bys dpto: egen total_oc_dpto_`a' = sum(oc * fex_dpto) if `a' == 1
	* Informality
	bys dpto: egen informal_`a' = sum(oc * fex_dpto) if p6920 == 2 & `a' == 1
	* Proportion
	gen informal_`a'_prop = informal`a' * 100 /total_oc_dpto
}

collapse informal_coal_prop informal_coal_related_prop informal_mining_prop, by(depto)
restore


*******Income

* Income distribution for each group
preserve
format impa %20.0g
hist impa if coal == 1,  kdensity freq color(dknavy) graphregion(fcolor(white)) ylabel(, nogrid) title("Income distribution of workers in the coal sector") xtitle("First activity monetary income")
graph export h1.png, replace
hist impa if coal_related == 1, kdensity freq color(dknavy) graphregion(fcolor(white)) ylabel(, nogrid) title("Income distribution of direct and indirect workers in the coal sector") xtitle("First activity monetary income")
graph export h2.png, replace
hist impa if miningdummy == 1, kdensity freq color(dknavy) graphregion(fcolor(white)) ylabel(, nogrid) title("Income distribution of workers in the mining sector") xtitle("First activity monetary income")
graph export h3.png, replace

* Median income by group
foreach a in $cats{
tabstat impa if `a' == 1, by(depto) stat(median min max mean count)

* Box plot
graph box impa if (depto == 25 | depto == 15 | depto == 20 | depto == 44 | depto == 5 | depto == 54) & `a' == 1, over(p6920)
keep if (depto == 25 | depto == 15 | depto == 20 | depto == 44 | depto == 5 | depto == 54)
graph box impa if `a' == 1 & p6920 == 1, over(depto) 
graph box impa if `a' == 1 , over(depto) 
}

restore

* Type of contract
preserve
gen contract = .
replace contract = 1 if p6460 == 1
replace contract = 2 if p6460 == 2
replace contract = 3 if p6460 == .


foreach a in $cats{
	bys depto: egen total_oc_dpto_`a' = sum(oc*fex_dpto) if `a' == 1 // Total
	bys depto: egen indefinido_`a' = sum(oc*fex_dpto) if p6460 == 1 & `a' == 1 // Undefined term
	bys depto: egen fijo_`a' = sum(oc*fex_dpto) if p6460 == 2 & `a' == 1 // Fixed term
	bys depto: egen nn_`a' = sum(oc*fex_dpto) if p6460 == 3 & `a' == 1 // None

	* Proportions
	gen indefinido_`a'_prop = (indefinido_`a'/total_oc_dpto_`a')*100
	gen fijo_`a'coal''_prop = (fijo_`a'/total_oc_dpto_`a')*100
	gen nn_`a'_prop = (nn_`a'/total_oc_dpto_`a')*100
} 

collapse indefinido* fijo* nn*, by(depto)

restore

* Type of job
preserve
gen job_type = .
replace job_type = 1 if p6430 == 1 | p6430 == 2
replace job_type = 2 if p6430 == 5
replace job_type = 3 if p6430 == 4
replace job_type = 4 if p6430 == 3 | p6430 == 6 | p6430 == 7 | p6430 == 8 | p6430 == 9 | p6430 == .

foreach a in $cats{
	* Total occupied
	bys depto: egen total_oc_dpto_`a' = sum(oc*fex_dpto) if `a' == 1

	* Occupied by type of job and department
	bys depto: egen c1_`a' = sum(oc*fex_dpto) if job_type == 1 & `a' == 1
	bys depto: egen c2_`a' = sum(oc*fex_dpto) if job_type == 2 & `a' == 1
	bys depto: egen c3_`a' = sum(oc*fex_dpto) if job_type == 3 & `a' == 1
	bys depto: egen c4_`a' = sum(oc*fex_dpto) if job_type == 4 & `a' == 1

	* Proportions
	gen c1_`a'_prop = (c1_`a'/total_oc_dpto_`a')*100
	gen c2_`a'_prop = (c2_`a'/total_oc_dpto_`a')*100
	gen c3_`a'_prop = (c3_`a'/total_oc_dpto_`a')*100
	gen c4_`a'_prop = (c4_`a'/total_oc_dpto_`a')*100
}

collapse c1* c2* c3* c4*, by(depto)

graph pie empleocoal1, over(job_type) by(depto, graphregion(fcolor(white)))  legend(off)

keep impa depto coal coal_related p6920 orden secuencia_p mes directorio
gen impa_wmin2019=impa/877802
save impa2019.dta, replace

restore

* Ecucation
preserve
gen educ = .
replace educ = 1 if p6220 == 4 | p6220 == 5
replace educ = 2 if p6220 == 3
replace educ = 3 if p6220 == 1 | p6220 == 2 | p6220 == 6 | p6220 == .

gen emp1 = oc if coal == 1
gen emp2 = oc if coal_related == 1
gen emp3 = oc if miningdummy == 1
bys depto: egen emp_dpto = sum(oc)

collapse (count) emp1 emp2 [iw=fex_dpto], by(depto educ)

save educ2019.dta, replace
restore

* Create local to store id values
preserve

levelsof directorio if coal == 1
local carb=r(levels)
gen id=.
foreach i of local carb{
	replace id=1 if directorio==`i'
}

keep if id == 1
asdoc tab sector if coal != 1  [iw = fex_dpto], save(tab1) replace

foreach i in 5 8 11 15 19 20 25 44 47 54  {
	asdoc tab sector if coal!=1 & depto==`i' [iw=fex_dpto], title save(tab1) append
}
restore
