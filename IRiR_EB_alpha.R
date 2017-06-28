
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
start.time =  Sys.time()
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

#### Stage 2 - Z factor adjustment using regression ####
# As described in report 71/2012, see above
# Techincal description in: "Second stage adjustment for firm heterogeneity in DEA:
# A novel approach used in regulation of Norwegian electricity DSOs, H.M. Kvile, O. Kordahl, T. Langset & R. Amundsveen, 2014"
# ENG: http://bit.ly/2sH5oLV

source("./R-script/2_0_Stage2_GeoCorrection.R")

#### Stage 3 - Cost norm calibration ####
# As described in circular 1/2013
# NOR http://webfileservice.nve.no/API/PublishedFiles/Download/201607005/1944365
# Based on analysis in report 11/2011
# NOR http://publikasjoner.nve.no/rapport/2011/rapport2011_21.pdf

source("./R-script/3_0_Stage3_Calibration.R")


#### Companies exempted from DEA - Special models ####

source("./R-script/Spec_OOTO-model.R") # Further special treatment of 35, 162 & 173
source("./R-script/Spec_AvEff-model.R")


#### Calculating Revenue caps ####
source("./R-script/4_0_Revenue_Cap_Calculation.R")

end.time =  Sys.time()
calc.time = end.time - start.time
calc.time
ldz.coeff
rdz.coeff
write.csv(RevCap, file = "qa_revcap_bs_6.csv")
source("X_4_Excel_export.R")
