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

dat.harm$dr_aruter_sum = dat.harm$dr_antall.ruter


dr_harm.var_ruter = c("dr_antall_ruter", "d_score_bs100_pre", "dr_brgrad_gjsn",
                      "dr_he1", "dr_is_gjsn", "dr_k2lukk", "dr_s4", "dr_s7",
                      "dr_snog", "dr_temp", "dr_vr")
dr_test.var_ruter = c("dr_antall.ruter", "dr_snog", "dr_temp", "dr_vr")

rr_harm.var_ruter = c("rr_antall_ruter", "r_score_bs100_pre", "rr_he", "rr_s12")

df.test = dat.harm[,c("orgnr", "aar", "id", "idaar", "d_391", "d_DVxL", "d_ab", "dr_antall.ruter", "dr_aruter_sum", "dr_snog", "dr_temp", "dr_vr")]

test.var.sum = c("d_391", "d_DVxL", "d_ab", "dr_aruter_sum")

#Gjør L70-74 til en funksjon

test.til_fusj = filter(df.test, df.test$orgnr %in% fun.comps)

                
                
tvsd = as.data.frame(test.til_fusj %>%
                                group_by(aar) %>%
                                summarise_each(funs(sum), one_of(as.character(test.var.sum))))


tvsd$orgnr = 999999999
tvsd$id = 999
tvsd$idaar = as.numeric(paste(tvsd$id, tvsd$aar, sep = ""))

comp.info = c("orgnr", "aar", "id", "idaar")


tvvd1 = select(test.til_fusj, one_of(dr_test.var_ruter))
tvvd1$multip.col = tvvd1$dr_antall.ruter
tvvd1 = as.data.frame(bind_cols(tvvd1, select(test.til_fusj, one_of(comp.info))))

tvvd1[dr_test.var_ruter] = tvvd1[dr_test.var_ruter] * tvvd1$multip.col

tvvd2 = as.data.frame(tvvd1 %>%
                             group_by(aar) %>%
                             summarise_each(funs(sum), one_of(as.character(dr_test.var_ruter))))


tvvd2$id = 999
tvvd2$idaar = as.numeric(paste(tvsd$id, tvsd$aar, sep = ""))
tvvd2$id = NULL
tvvd2$aar = NULL
#df.test = bind_rows(df.test, tvsd)

tvsd = inner_join(tvsd, tvvd2, by = "idaar")
tvsd[dr_test.var_ruter] = tvsd[dr_test.var_ruter] / tvsd$dr_aruter_sum

df.test = bind_rows(df.test, tvsd)
df.test = df.test[!(df.test$orgnr %in% fun.comps),]
