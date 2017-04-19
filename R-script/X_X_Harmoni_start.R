#Harmoni alpha.

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
library(FactoMineR) # Kan fjernes?
library(outliers)
library(plot3D)

source("./R-script/functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)

#### Grunnlag for DEA ####

source("./R-script/H_0_1_Config_Assumptions_Data.R")

#som skal fusjoneres.
#Selskap1 Alta
fun.comp1 = c(971029390)
#Selskap2 Andøy
fun.comp2 = c(971048611)

fun.comps = c(fun.comp1, fun.comp2)

## Years in data set


dat.harm$dr_fusj.ruter = dat.harm$dr_antall.ruter
dat.harm$rr_fusj.ruter = dat.harm$rr_antall.ruter

harm.var_sum = c("d_391", "d_DVxL", "d_ab", "d_abavs", "d_abbfv", "d_aoey1", "d_avs",
                 "d_bfv", "d_grs", "d_hs", "d_hsjord", "d_hsll", "d_hssjo", "d_impl",
                 "d_kile", "d_lonn", "d_lonnakt", "d_nettap", "d_ns", "d_pensj", 
                 "d_pensjek", "d_skytelse", "d_utred", "r_391", "r_DVxL", "r_abavs", 
                 "r_abavs_melk", "r_abbfv", "r_abbfv_melk", "r_avs", "r_bfv", "r_impl",
                 "r_kile", "r_lonn", "r_lonnakt", "r_nettap", "r_pensj", "r_pensjek",
                 "r_utred", "r_vgrs", "r_vjord", "r_vluft", "r_vsjo", "s_391", "s_DVxL",
                 "s_avs", "s_bfv", "s_impl", "s_kile", "s_lonn", "s_lonnakt", "s_pensj",
                 "s_pensjek", "s_vgrs", "s_vjord", "s_vluft", "s_vsjo", "dr_fusj.ruter",
                 "rr_fusj.ruter")


dr_harm.var_ruter = c("dr_antall_ruter", "d_score_bs100_pre", "dr_brgrad_gjsn",
                      "dr_he1", "dr_is_gjsn", "dr_k2lukk", "dr_s4", "dr_s7",
                      "dr_snog", "dr_temp", "dr_vr")

rr_harm.var_ruter = c("rr_antall_ruter", "r_score_bs100_pre", "rr_he", "rr_s12")

#Gjør L70-75 til en funksjon
tilleg = as.data.frame(matrix(NA,ncol = ncol(df.test), nrow = length(unique(df.test$aar))))
colnames(tilleg)=colnames(df.test)
tilleg$orgnr = 999999999
tilleg$id = 999
tilleg$idaar = as.numeric(paste(tilleg$id, tilleg$aar, sep = ""))


# # KPIA-justering
# > var_kpia = c("d_391", "r_391", "s_391", "d_pensj", "r_pensj", "s_pensj", "d_pensjek", "r_pensjek", "s_pensjek", 
#                +              "d_impl", "r_impl", "s_impl", "d_DVxL", "d_utred", "r_DVxL", "r_utred", "s_DVxL", "d_lonn", 
#                +              "d_lonnakt", "r_lonn", "r_lonnakt", "s_lonn", "s_lonnakt")
# > fp_var_kpia = paste("fp_", var_kpia, sep = "")
# > dat.harm = cbind(dat.harm, t(matrix(NA, ncol = nrow(dat.harm), nrow = length(var_kpia), dimnames = list(var_kpia = fp_var_kpia))))
# > for(c in 1:length(var_kpia))
#         +   for(r in 1:nrow(dat.harm))
#                 +     dat.harm[r, fp_var_kpia[c]] = dat.harm[r, var_kpia[c]] * kpia[as.character(faktisk.aar)] / kpia[as.character(dat.harm[r, "aar"])]



df.test = dat.harm[,c("orgnr", "aar", "id", "idaar", "d_391", "d_DVxL", "d_ab")]
## Years in data set

