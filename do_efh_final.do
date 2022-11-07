***CASO 1: rci_dcon***

**1.0 Preparación y estadística descriptiva**
clear all
*cd "C:\Users\Fernanda\OneDrive\1MAE\4to Semestre\TESIS\"
cd "C:\Users\mflopezc\OneDrive\1MAE\4to Semestre\TESIS"
use Panel_EFH.dta
global años 2007 2011 2014

quietly{

//Se generan promedios de las 30 imputaciones y se reemplazan por variables originales

egen rci_dnhip_imp=mean(rci_dnhip) , by(id_panel año)
egen ytotef_imp=mean(ytotef) , by(id_panel año)
egen cf_dnhip_imp=mean(cf_dnhip) , by(id_panel año)
egen cf_deduc_imp=mean(cf_deduc) , by(id_panel año)
drop rci_dnhip ytotef cf_dnhip cf_deduc
rename rci_dnhip_imp rci_dnhip
rename ytotef_imp ytotef
rename cf_dnhip_imp cf_dnhip 
rename cf_deduc_imp cf_deduc 

//se eliminan observaciones de hogares no disponibles en los 3 años y las imputaciones por hogar (quedan 2982 obs)

keep if imp == 0

//se genera ratio carga financiera de consumo (deuda no hipotecaria sin deuda educacional) y se eliminan ratios negativos (8 obs)

gen rci_con = (cf_dnhip - cf_deduc)/ytotef
drop if rci_con < 0 

//Se eliminan los p(99) para 2007, 2011 y 2014 en los hogares a utilizar (54 en total)

foreach i in $años {
sum rci_con if rci_con>0 & año == `i' & panel== 3, d
drop if rci_con >= r(p99) & año == `i' & panel== 3
}

//se recalcula el panel

drop panel 
egen panel_=count(año) if id_panel != 0 , by(id_panel) 

//Comparacion muestra completa y a utilizar
gen autilizar = panel == 3

foreach i in $años {
sum id_panel if año == `i'
sum id_panel if año == `i' & rci_dt > 0
sum id_panel if año == `i' & rci_dt > 0 & autilizar == 1
ttest rci_dt if año == `i', by(autilizar)
ttest rci_dnhip if año == `i', by(autilizar)
ttest rci_con if año == `i', by(autilizar)
}

drop autilizar 

//se eliminan hogares no disponibles en 3 periodos y quedan 935 hogares o 2805 observaciones
keep if panel== 3
tab panel año

//Gráficos distribución carga financiera en endeudados (con línea en 0.25)

*kdensity rci_con if rci_con > 0, lc(dknavy) xline(0.25, lcolor(edkblue)) xtitle(Ratio Carga Financiera) ytitle(Densidad) yscale(range(0 3.5)) xscale(range(0 2.5)) title("Distribución Ratio Carga Financiera Consumo 2007")
*gr export hist_con_2007.png, replace

*kdensity rci_con if rci_con > 0, lc(dknavy) xline(0.25, lcolor(edkblue)) xtitle(Ratio Carga Financiera) ytitle(Densidad) yscale(range(0 3.5)) xscale(range(0 2.5)) title("Distribución Ratio Carga Financiera Consumo 2011")
*gr export hist_con_2011.png, replace

*kdensity rci_con if rci_con > 0, lc(dknavy) xline(0.25, lcolor(edkblue)) xtitle(Ratio Carga Financiera) ytitle(Densidad) yscale(range(0 3.5)) xscale(range(0 2.5)) title("Distribución Ratio Carga Financiera Consumo 2014")
*gr export hist_con_2014.png, replace

}

**1.1 Dummies sobrendeudamiento*

//Se define umbral de 25% 
scalar umbral = 0.25

//Se generan dummies de sobrendeudamiento por año y se asignan al ID del hogar

gen se_07 = rci_con>=umbral & año ==2007
gen se_11 = rci_con>=umbral & año ==2011
gen se_14 = rci_con>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

//% de sobreendeudamiento por año

tab se_07_ año if año == 2007, col 
tab se_11_ año if año == 2011, col 
tab se_14_ año if año == 2014, col 

//arbol de trayectorias

tab se_11_ año if se_07_==1 & año == 2011, col 
tab se_14_ año if se_07_==1 & se_11_==0 & año == 2014, col 
tab se_14_ año if se_07_==1 & se_11_==1 & año == 2014, col 

*** 1.2 Hogares por trayectoria

//se eliminan los hogares que no tuvieron deuda en 2007 (734/935)

drop if se_07_== 0

// se genera la dummy de trayectoria y se calculan que hogares corresponden a que categoría

gen d_i = (se_11_==0)

** 1.3 test de medias (primera aproximación de que son distintos)**

ttest se_14_ if año == 2014, by(d_i)

** 1.4 estimacion ** 

//crear variables de control 
scalar ajuste_2011 = 0.13581 //dic 07 - dic 11
scalar ajuste_2014 = 0.09858 // dic 11 - nov 14

scalar ir_2011 = 0.13013 //dic 07 - dic 11
scalar ir_2014 = 0.07973 // dic 11 - nov 14

quietly{

gen ytotef_aj_2014 = ytotef if año == 2014
replace ytotef_aj_2014 = ytotef * (1+ir_2011+ir_2014) if año == 2007
replace ytotef_aj_2014 = ytotef * (1+ir_2014) if año == 2011

gen cf_dnhip_aj_2014 = cf_dnhip if año == 2014
replace cf_dnhip_aj_2014 = cf_dnhip * (1+ajuste_2011+ajuste_2014) if año == 2007
replace cf_dnhip_aj_2014 = cf_dnhip * (1+ajuste_2014) if año == 2011
	
gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 
}


//Estadística descriptiva por tipo de hogar ** agregar tenencia deuda hipotecaria

foreach i in $años {
	display "***************Ingreso ****************AÑO" `i'
	ttest ytotef_aj_2014 if año == `i', by(d_i)
	display "***************Carga financiera ****************AÑO" `i'
	ttest cf_dnhip_aj_2014 if año == `i', by(d_i)
	display "***************Ocupados ****************AÑO" `i'
	ttest ocup_pr if año == `i', by(d_i)
	display "***************Pareja ****************AÑO" `i'
	ttest pareja if año == `i', by(d_i)
	display "***************Miembros inact ****************AÑO" `i'
	ttest miembros_inact  if año == `i', by(d_i)
	display "***************Propietario vivienda ****************AÑO" `i'
	ttest propietario  if año == `i', by(d_i)
}

//Estadística descriptiva 2 por año: 2007 //REVISAR VARIABLE PERCEPCION

replace perc = 1 if perc < 3 & año == 2007
replace perc = 0 if perc > 2 & año == 2007
ttest perc if año == 2007, by(d_i) // perc = g2
replace g3a = 0 if g3a == 2 & año == 2007
replace g3a = . if g3a > 2 & año == 2007
ttest g3a if año == 2007, by(d_i)
replace g4 = 0 if g4 != 1 & año == 2007
ttest g4 if año == 2007, by(d_i)
replace g5 = 0 if g5 != 1 & año == 2007
ttest g5 if año == 2007, by(d_i)
replace g10 = 0 if g10 != 2 & año == 2007
replace g10 = 1 if g10 == 2 & año == 2007
ttest g10 if año == 2007, by(d_i) //No le gusta solicitar créditos

replace k4 = 0 if k4 != 1 & año == 2007 //mas altos de lo normal
ttest k4 if año == 2007, by(d_i)
replace k5 = 0 if k5 != 2 & año == 2007 //menores
ttest k5 if año == 2007, by(d_i)
replace k6 = 0 if k6 != 1 & año == 2007 //mayores
ttest k6 if año == 2007, by(d_i)
replace k9 = 0 if k9 != 2 & año == 2007 //menores
ttest k9 if año == 2007, by(d_i)
replace k10 = 0 if k10 != 4 & año == 2007 //no están dispuestos a asumir riesgos financieros
replace k10 = 1 if k10 == 4 & año == 2007 
ttest k10 if año == 2007, by(d_i)

//Estadística descriptiva 2 por año: 2011

replace c33 = 0 if (c33 == 1 | c33 == 3) & año == 2011 // mis gastos exceden a mis ingresos
replace c33 = 1 if c33 == 2 & año == 2011
replace c33 = . if c33 > 1 & año == 2011
ttest c33 if año == 2011, by(d_i) 

gen p1_bajo = 0
replace p1_bajo = 1 if p1 == 3 & año == 2011 // anormalmente bajo
ttest p1_bajo if año == 2011, by(d_i) 
replace p1 = 0 if p1 != 1 & año == 2011 // anormalmente alto
ttest p1 if año == 2011, by(d_i) 
replace p3 = 0 if p3 != 1 & año == 2011 // menor que el actual
ttest p3 if año == 2011, by(d_i) 

replace g1 = 0 if g1 == 2 & año == 2011 // mayor que el actual
ttest g1 if año == 2011, by(d_i) 
replace g1_1_4 = 0 if g1_1_4 == 2 & año == 2011 // si (solo los que han tenido que pedir)
ttest g1_1_4 if año == 2011, by(d_i) 
gen cambio_aum = 0
replace cambio_aum = 1 if cambio == 1 & año == 2011 // sí, ha aumentado
ttest cambio_aum if año == 2011, by(d_i) 
gen cambio_dism = 0
replace cambio_dism = 1 if cambio == 2 & año == 2011 // sí, ha disminuido
ttest cambio_dism if año == 2011, by(d_i) 
replace g13_2  = 0 if g13_2  == 2 & año == 2011 
ttest g13_2  if año == 2011, by(d_i) 
replace g13_5  = 0 if g13_5  == 2 & año == 2011 
ttest g13_5  if año == 2011, by(d_i) 
replace g16 = 0 if g16 != 2 & año == 2011
replace g16 = 1 if g16 == 2 & año == 2011
ttest g16 if año == 2011, by(d_i) //No le gusta solicitar créditos

//Estadística descriptiva 2 por año: 2014

replace i9 = 0 if i9 != 2 & año == 2014 // mis gastos exceden a mis ingresos
replace i9 = 1 if i9 == 2 & año == 2014
ttest i9 if año == 2014, by(d_i) 

replace perc = 1 if perc < 3 & año == 2014
replace perc = 0 if perc > 2 & año == 2014
ttest perc if año == 2014, by(d_i) // perc = g1
ttest inesperados if año == 2014, by(d_i) //g2
replace g3_4  = 0 if g3_4  == 2 & año == 2014 
ttest g3_4  if año == 2014, by(d_i) 
replace g4_4  = 0 if g4_4  == 2 & año == 2014 
ttest g4_4  if año == 2014, by(d_i) 
replace g4_4  = 0 if g4_4  == 2 & año == 2014 
ttest g4_4  if año == 2014, by(d_i) 

replace g11 = 0 if g11 != 2 & año == 2014
replace g11 = 1 if g11 == 2 & año == 2014
ttest g11 if año == 2014, by(d_i) //No le gusta solicitar créditos

//Trayectorias por tipo de hogar

gen ytotef_aj_2011 = ytotef if año == 2011
replace ytotef_aj_2011 = ytotef * (1+ir_2011) if año == 2007

gen ytotef_2007 = ytotef_aj_2011 if año == 2007
egen ytotef_comp2007=sum(ytotef_2007), by(id_panel)
replace ytotef_comp2007 = . if año != 2011

gen variacion_ingresos = (ytotef - ytotef_comp2007)/ytotef_comp2007
gen a_i_2007 = variacion_ingresos >= 0.1 if año == 2011 //aumento ingresos superior a un 10%
egen a_ing=sum(a_i_2007), by(id_panel)
gen d_i_2007 = variacion_ingresos <= -0.1 if año == 2011 //disminución ingresos superior a un 10%
egen d_ing=sum(d_i_2007), by(id_panel)
gen m_i_2007 = (variacion_ingresos < 0.1 & variacion_ingresos > -0.1) if año == 2011 //ingresos se mantienen con un rango +-10%
egen m_ing=sum(m_i_2007), by(id_panel)

sum variacion_ingresos if a_ing == 1 & año == 2011
sum variacion_ingresos if m_ing == 1 & año == 2011
sum variacion_ingresos if d_ing == 1 & año == 2011
sum variacion_ingresos if año == 2011

replace a_ing = . if año != 2014
replace d_ing = . if año != 2014
replace m_ing = . if año != 2014
drop ytotef_2007 a_i_2007 d_i_2007 m_i_2007

gen cf_dnhip_aj_2011 = cf_dnhip if año == 2011
replace cf_dnhip_aj_2011 = cf_dnhip * (1+ajuste_2011) if año == 2007

gen cf_dnhip_2007 = cf_dnhip_aj_2011 if año == 2007
egen cf_dnhip_comp2007=sum(cf_dnhip_2007), by(id_panel)
replace cf_dnhip_comp2007 = . if año != 2011

gen variacion_carga = (cf_dnhip - cf_dnhip_comp2007)/cf_dnhip_comp2007
gen a_cf_2007 = variacion_carga >= 0.1 if año == 2011 //aumento carga financiera superior a un 10%
egen a_cf=sum(a_cf_2007), by(id_panel)
gen d_cf_2007 = variacion_carga <= -0.1 if año == 2011 //aumento carga financiera superior a un 10%
egen d_cf=sum(d_cf_2007), by(id_panel)
gen m_cf_2007 = (variacion_carga < 0.1 & variacion_carga > -0.1) if año == 2011 //aumento carga financiera superior a un 10%
egen m_cf=sum(m_cf_2007), by(id_panel)

sum variacion_carga if a_cf == 1 & año == 2011
sum variacion_carga if m_cf == 1 & año == 2011
sum variacion_carga if d_cf == 1 & año == 2011
sum variacion_carga if año == 2011

replace a_cf = . if año != 2014
replace d_cf = . if año != 2014
replace m_cf = . if año != 2014
drop cf_dnhip_2007 a_cf_2007 d_cf_2007 m_cf_2007

gen grupo = (a_ing == 1 & a_cf == 1)
replace grupo = 2 if (a_ing == 1 & m_cf == 1)
replace grupo = 3 if (a_ing == 1 & d_cf == 1)

replace grupo = 4 if (m_ing == 1 & a_cf == 1)
replace grupo = 5 if (m_ing == 1 & m_cf == 1)
replace grupo = 6 if (m_ing == 1 & d_cf == 1)

replace grupo = 7 if (d_ing == 1 & a_cf == 1)
replace grupo = 8 if (d_ing == 1 & m_cf == 1)
replace grupo = 9 if (d_ing == 1 & d_cf == 1)

replace grupo = . if año != 2014

tab grupo d_i

// modelo de probabilidad y efectos marginales

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

outreg2 using "est1", replace stats (coef pval)
 
logit se_14 d_cf log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_cf log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

outreg2 using "est_cf", replace 
 
//Análisis de sensibilidad

//1. Se define umbral de 20% con carga financiera de consumo

scalar umbral = 0.2

gen se_07 = rci_con>=umbral & año ==2007
gen se_11 = rci_con>=umbral & año ==2011
gen se_14 = rci_con>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

outreg2 using "est2", replace stats (coef pval)

//2. Se define umbral de 30% con carga financiera de consumo

scalar umbral = 0.3

gen se_07 = rci_con>=umbral & año ==2007
gen se_11 = rci_con>=umbral & año ==2011
gen se_14 = rci_con>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

outreg2 using "est3", replace stats (coef pval)

***CASO 2: rci_dnhip***

**1.0 Preparación y estadística descriptiva**
clear all
cd "C:\Users\mflopezc\OneDrive\1MAE\4to Semestre\TESIS"
*cd "C:\Users\Fernanda\OneDrive\1MAE\4to Semestre\TESIS\"
use Panel_EFH.dta
global años 2007 2011 2014

//Se generan promedios de las 30 imputaciones y se reemplazan por variables originales

egen rci_dnhip_imp=mean(rci_dnhip) , by(id_panel año)
egen ytotef_imp=mean(ytotef) , by(id_panel año)
egen cf_dnhip_imp=mean(cf_dnhip) , by(id_panel año)
egen cf_deduc_imp=mean(cf_deduc) , by(id_panel año)
drop rci_dnhip ytotef cf_dnhip cf_deduc
rename rci_dnhip_imp rci_dnhip
rename ytotef_imp ytotef
rename cf_dnhip_imp cf_dnhip 
rename cf_deduc_imp cf_deduc 

//se eliminan observaciones de hogares no disponibles en los 3 años y las imputaciones por hogar 

keep if imp == 0

//Se eliminan los p(99) para 2007, 2011 y 2014 en los hogares a utilizar 

foreach i in $años {
sum rci_dnhip if rci_dnhip>0 & año == `i' & panel== 3, d
drop if rci_dnhip >= r(p99) & año == `i' & panel== 3
}

//se recalcula el panel

drop panel 
egen panel_=count(año) if id_panel != 0 , by(id_panel) 

//se eliminan hogares no disponibles en 3 periodos 
keep if panel== 3

scalar umbral = 0.25

gen se_07 = rci_dnhip>=umbral & año ==2007
gen se_11 = rci_dnhip>=umbral & año ==2011
gen se_14 = rci_dnhip>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

//Análisis de sensibilidad

//1. Se define umbral de 20% con carga financiera no hipotecaria

scalar umbral = 0.2

gen se_07 = rci_dnhip>=umbral & año ==2007
gen se_11 = rci_dnhip>=umbral & año ==2011
gen se_14 = rci_dnhip>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post


//2. Se define umbral de 30% con carga financiera no hipotecaria

scalar umbral = 0.3

gen se_07 = rci_dnhip>=umbral & año ==2007
gen se_11 = rci_dnhip>=umbral & año ==2011
gen se_14 = rci_dnhip>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

***CASO 3: rci_dt***

**1.0 Preparación y estadística descriptiva**
clear all
cd "C:\Users\mflopezc\OneDrive\1MAE\4to Semestre\TESIS"
*cd "C:\Users\Fernanda\OneDrive\1MAE\4to Semestre\TESIS\"
use Panel_EFH.dta
global años 2007 2011 2014

//Se generan promedios de las 30 imputaciones y se reemplazan por variables originales

egen rci_dt_imp=mean(rci_dt) , by(id_panel año)
egen ytotef_imp=mean(ytotef) , by(id_panel año)
egen cf_dt_imp=mean(cf_dnhip) , by(id_panel año)
drop rci_dt ytotef cf_dt
rename rci_dt_imp rci_dt
rename ytotef_imp ytotef
rename cf_dt_imp cf_dt 

//se eliminan observaciones de hogares no disponibles en los 3 años y las imputaciones por hogar 

keep if imp == 0

//Se eliminan los p(99) para 2007, 2011 y 2014 en los hogares a utilizar 

foreach i in $años {
sum rci_dt if rci_dt>0 & año == `i' & panel== 3, d
drop if rci_dt >= r(p99) & año == `i' & panel== 3
}

//se recalcula el panel

drop panel 
egen panel_=count(año) if id_panel != 0 , by(id_panel) 

//se eliminan hogares no disponibles en 3 periodos 
keep if panel== 3

**

scalar umbral = 0.25

gen se_07 = rci_dt>=umbral & año ==2007
gen se_11 = rci_dt>=umbral & año ==2011
gen se_14 = rci_dt>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

//Análisis de sensibilidad

//1. Se define umbral de 20% con carga financiera total

scalar umbral = 0.2

gen se_07 = rci_dt>=umbral & año ==2007
gen se_11 = rci_dt>=umbral & año ==2011
gen se_14 = rci_dt>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post

//2. Se define umbral de 30% con carga financiera total

scalar umbral = 0.3

gen se_07 = rci_dt>=umbral & año ==2007
gen se_11 = rci_dt>=umbral & año ==2011
gen se_14 = rci_dt>=umbral & año ==2014

egen se_07_=sum(se_07) , by(id_panel)
egen se_11_=sum(se_11) , by(id_panel) 
egen se_14_=sum(se_14) , by(id_panel)
drop se_07 se_11 se_14

drop if se_07_== 0

gen d_i = (se_11_==0)

gen log_ytotef = log(ytotef)
*ocup_pr = 1 si está ocupado
rename genero_pr hombre
gen pareja = (est_civil_pr < 3)  //si está casado o conviviendo
gen miembros_inact = numh - ocuph
*t_atoth = 1 si tiene activos
*t_dhip  = 1 si tiene deuda hipotecaria
*t_dauto = 1 si tiene deuda por vehículo
*t_deduc = 1 si tiene deuda educacional
gen ed_terciaria = (neduc_pr > 3)
*i.tr_edad_pr es el tramo de edad
*i.estrato es bajo/medio/alto
gen propietario = (reg_vp == 1)
gen gastosaltos = (i9 == 2)
gen inesperados = (g2 == 1)
gen sh_salud = (g7_2 == 1)
gen sh_empleo = (g7_1 == 1)
gen sh_hogar = (g7_5 == 1)
gen expectativas = (k2 == 1)
*#macrozona 

keep if año == 2014 

logit se_14 d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona

margins, dydx(d_i log_ytotef ocup_pr hombre pareja miembros_inact t_atoth t_dhip t_dauto t_deduc ed_terciaria propietario gastosaltos inesperados sh_salud sh_empleo sh_hogar expectativas i.tr_edad_pr i.estrato i.macrozona) post


