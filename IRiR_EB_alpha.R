
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
library(robustX)
library(outliers)


source("./R-script/functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)

#### Grunnlag for DEA ####

source("./R-script/1_0_Config_Assumptions_Data.R")

source("./R-script/1_1_Calculated_Input_Values.R")

source("./R-script/1_2_Company_Selection.R")

source("./R-script/1_3_Input_Data_DEA.R")

#### Trinn 1 - DEA ####
        #D-nett
source("./R-script/1_4_x_DEA_Dnett_gr.R")
        #R-nett
source("./R-script/1_6_DEA_RSnett.R")

####  Trinn 2 - RVK-justering vha regresjon ####
        #D-nett
source("./R-script/2_0_Bootstrap_Data.R")
source("./R-script/2_4_GEO_correction_stage2.R")

rvk1(x=x.snitt.d,z=cbind(d_tilDEA$dr_hsjordand, d_tilDEA$dr_s4, d_tilDEA$dr_Geo1, d_tilDEA$dr_Geo2, d_tilDEA$dr_Geo3), eff=d_tilDEA$d_bs_correst_e3, lambda=d_lambda, id=d_tilDEA$id, id.ut=d_separat_dmuer)

source("./R-script/2_4_1_TargetU_Geo.R")
source("./R-script/2_3_Act_GEO_Correct.R")
        #R-nett
source("./R-script/2_5_Bootstrap_Data_R.R")

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
