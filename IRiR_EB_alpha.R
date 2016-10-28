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
library(plyr)
library(dplyr)
library(FactoMineR)

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
source("./R-script/1_4_DEA_DistributionGrid.R")
        #R-nett

####  Trinn 2 - RVK-justering vha regresjon ####
        #D-nett
source("./R-script/2_0_Bootstrap_Data.R")

source("./R-script/2_4_GEO_correction_stage2.R")


# #correct for environmental effects
res.stage2 = two.stage(x.snitt,z.snitt,res.snitt.snitt.r$eff,res.snitt.snitt.r$lambda) 
plot(sort(res.stage2$eff.corr.NVE))
# 


#### Trinn 3 - Kalibrering av kostnadsnormer ####
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
