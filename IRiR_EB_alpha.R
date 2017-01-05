
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

source("./R-script/0_1_Config_Assumptions_Data.R")

source("./R-script/0_2_Calculated_Input_Values.R")

source("./R-script/0_3_Company_Selection.R")

#### Trinn 1 - DEA ####
source("./R-script/1_0_DEA.R")

####  Trinn 2 - RVK-justering vha regresjon ####
source("./R-script/2_X_Bootstrap_Data.R")

source("./R-script/2_0_Stage2_GeoCorrection.R")


#### Trinn 3 - Kalibrering av kostnadsnormer ####
        #D-nett
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
