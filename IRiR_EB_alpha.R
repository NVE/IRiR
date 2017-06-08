
## IRiR - InntektsRammer i R##
## RevenueCap calculation in R##

#### R set up ####

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
# Following packages are used
# Benchmarking, xlsx, plyr, dplyr
library(Benchmarking)
library(xlsx)
library(xlsxjars)
library(plyr)
library(dplyr)
library(FactoMineR) # Kan fjernes?
library(outliers)
library(plot3D)

source("./R-script/functions_nve.R") # File containing functions created for/by NVE

# Avoid showing large numbers in scientific mode
options(scipen = 100)

#### Preperation stage - Configurations, data import, data preparation ####

source("./R-script/0_1_Config_Assumptions_Data.R")

source("./R-script/0_2_Calculated_Input_Values.R")

source("./R-script/0_3_Company_Selection.R")

#### Stage 1 - DEA ####
# As described in report 71/2012
# NOR http://publikasjoner.nve.no/rapport/2012/rapport2012_71.pdf
source("./R-script/1_0_DEA.R")

#### Trinn 2 - RVK-justering vha regresjon ####
source("./R-script/2_X_Bootstrap_Data.R") # TEMP - new branch will be created for implementing bootstrap in R

source("./R-script/2_0_Stage2_GeoCorrection.R")


#### Trinn 3 - Kalibrering av kostnadsnormer ####
       
source("./R-script/3_0_Stage3_Calibration.R")


#### Selskaper utenfor DEA ####

source("./R-script/X_X_OOTO-model.R") # Ytterligere spesialbehandling av 35, 162 & 173
source("./R-script/X_X_COREC-model.R")

#### Faktisk beregning av IR ####
source("./R-script/X_X_Revenue_Cap_Calculation.R")



source("X_4_Excel_export.R")
