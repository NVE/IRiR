
## IRiR

#### R-teknisk oppsett ####

# Remove all objects from memory
remove(list=ls())
# Get current working directory
getwd()
# Set working directory to where the data file is located
# The address can be copied from the address bar in Windows Explorer
# Remember to change "\" to "/" or "\\" 
#my.path = "C:\\users\\roam\\Dropbox\\IRcalc i R"
my.path = "C:\\Users\\ens\\Jottacloud\\GitHub\\IRiR"
setwd(my.path)
# Load benchmarking package of Bogetoft & Otto
# Følgende pakker benyttes
# Benchmarking, xlsx, plyr, dplyr
library(Benchmarking)
library(xlsx)
library(xlsxjars)
library(plyr)
library(dplyr)
library(FactoMineR)
library(outliers)


source("./R-script/functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)

#### Grunnlag for DEA ####

source("./R-script/1_0_Config_Assumptions_Data_new.R")

source("./R-script/1_1_Calculated_Input_Values_hist1.R")

source("./R-script/1_2_Company_Selection.R")

source("./R-script/1_3_Input_Data_DEA.R")

#### Trinn 1 - DEA ####
        #D-nett
#source("./R-script/1_4_DEA_DistributionGrid.R")
source("./R-script/1_4_x_DEA_Dnett_gr.R")
        #R-nett
source("./R-script/1_6_DEA_RSnett.R")

####  Trinn 2 - RVK-justering vha regresjon ####
source("./R-script/2_0_Bootstrap_Data.R")
#D-nett
# Estimerer Fjellbekk
Geo1.comp = cbind(d_tilDEA[,c("dr_he1", "dr_s7", "dr_skysz")])
d_tilDEA$dr_Geo1 = z.est(geovar.in = Geo1.comp, restricted.obs = Geo1.comp)
rm(Geo1.comp)

# Estimerer Øyvind
Geo2.comp = cbind(d_tilDEA[,c("dr_vr2_k2lukk", "dr_aoey1sz", "dr_hssjoand")])
d_tilDEA$dr_Geo2 = z.est(geovar.in = Geo2.comp, restricted.obs = Geo2.comp)*-1
rm(Geo2.comp)

#Estimerer Frost
d_tilDEA$dr_tempneg = d_tilDEA$dr_temp*-1
#Setter alle verdier i snittbreddegrad under 65,9 til 65,9
d_tilDEA$dr_brgrad_gjsn = pmax(d_tilDEA$dr_brgrad_gjsn, 65.9)
Geo3.comp =cbind(d_tilDEA[,c("dr_snog", "dr_brgrad_gjsn", "dr_is_gjsn", "dr_tempneg")])
d_tilDEA$dr_Geo3 = z.est(geovar.in = Geo3.comp, restricted.obs = Geo3.comp)
rm(Geo3.comp)

#Beregner Geo-koeffisen1ter for D-nett
Geovar.d = cbind(d_tilDEA[,c("dr_hsjordand","dr_s4", "dr_Geo1", "dr_Geo2", "dr_Geo3")])
d.coeff = rvk1(x=x.snitt.d,z=Geovar.d,d_tilDEA$d_bs_correst_e3,
     lambda = d_lambda.snitt,
     id = names(x.snitt.d),
     id.ut = as.character(d_separat_dmuer))$coeff
d.coeff
#Utfører faktisk rammevilkårsjustering for D-nett
rvk2(x = x.snitt.d, eff = eff.faktisk.snitt.d, id = names(x.snitt.d),
     lambda = d_lambda, coeff = d.coeff, z = Geovar.d)

        #R-nett
source("./R-script/2_5_Bootstrap_Data_R.R")
GeoR.comp = cbind(r_tilDEA[,c("rr_s12", "rr_he")])
row.names(GeoR.comp) = names(x.snitt.r)
GeoR.tech = GeoR.comp[as.character(r_normal),]
#Estimerer Helskog
r_tilDEA$rr_Geo1 = z.est(geovar.in = GeoR.tech, restricted.obs = GeoR.comp)




#### Trinn 3 - Kalibrering av kostnadsnormer ####
        #D-net
source("./R-script/3_1_1_OOTO-model.R")
source("./R-script/3_5 Kalibrering_D3.R")

# #kalibrerer kostnadsnormer basert på avkastningsgrunnlag
res.stage3 = calibrate(res.faktisk.snitt.r$eff,x.faktisk,weight=kap.faktisk)
# #vis gjennomsnittlig kostnadsvektet effektivitet
res.stage3$industry.avg
# #plott kalibrerte effektivitetstall
plot(sort(res.stage3$eff.cal))
# #sjekk at sum kalibrert kostnadsnorm = sum kostnad
x%*%res.stage3$eff.cal
sum(x)

source("X_4_Excel_export.R")
