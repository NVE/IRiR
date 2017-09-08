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
fun.comp1 = c(971592117)
#Selskap2 Andøy
fun.comp2 = c(983099807)

fun.comps = c(fun.comp1, fun.comp2)

#Navn på fusjonert selskap
comp.name = c("NordBal")


dat.harm$dr_aruter_sum = dat.harm$dr_antall_ruter
dat.harm$rr_aruter_sum = dat.harm$rr_antall_ruter

harm.var_sum = c("d_391", "d_DVxL", "d_ab", "d_abavs", "d_abbfv", "dr_aoey1", "d_avs",
                 "d_bfv", "d_grs", "d_hs", "d_hsjord", "d_hsll", "d_hssjo", "d_impl",
                 "d_kile", "d_lonn", "d_lonnakt", "d_nettap", "d_ns", "d_pensj", 
                 "d_pensjek", "d_skytelse", "d_utred", "r_391", "r_DVxL", "r_abavs", 
                 "r_abavs_melk", "r_abbfv", "r_abbfv_melk", "r_avs", "r_bfv", "r_impl",
                 "r_kile", "r_lonn", "r_lonnakt", "r_nettap", "r_pensj", "r_pensjek",
                 "r_utred", "r_vgrs", "r_vjord", "r_vluft", "r_vsjo", "s_391", "s_DVxL",
                 "s_avs", "s_bfv", "s_impl", "s_kile", "s_lonn", "s_lonnakt", "s_pensj",
                 "s_pensjek", "dr_aruter_sum",
                 "rr_aruter_sum") # Disse er fjernet "s_vgrs", "s_vjord", "s_vluft", "s_vsjo",
                # Flere variabler som mangler i harm.var_sum, må legges til etter første kjøring med separate selskap.


dr_harm.var_ruter = c("dr_antall_ruter", "dr_brgrad_gjsn",
                      "dr_he1", "dr_is_gjsn", "dr_k2lukk", "dr_s4", "dr_s7",
                      "dr_snog", "dr_temp", "dr_vr") # Mangler før full kjøring: "d_score_bs100_pre", 


rr_harm.var_ruter = c("rr_antall_ruter", "rr_he", "rr_s12") # Mangler før full kjøring "r_score_bs100_pre"

#Regner disse prisene som naive snitt
merge.pr = c( "omraadepris_t2") # "nettapspris_ir", --> Håndteres som en skalar

#Gjør L70-74 til en funksjon


md = filter(dat.harm, dat.harm$orgnr %in% fun.comps)
#test.til_fusj = filter(df.test, df.test$orgnr %in% fun.comps)

mds =   as.data.frame(md %>%
                              group_by(aar) %>%
                              summarise_each(funs(sum), one_of(as.character(harm.var_sum))))              
## Kan fjernes når alt ok                
# tvsd = as.data.frame(test.til_fusj %>%
#                                 group_by(aar) %>%
#                                 summarise_each(funs(sum), one_of(as.character(test.var.sum))))

mds$orgnr = 999999999
mds$id = 999
mds$idaar = as.numeric(paste(mds$id, mds$aar, sep = ""))

comp.info = c("orgnr", "aar", "id", "idaar")


d_mdw = select(md, one_of(dr_harm.var_ruter))
d_mdw$mutipl.col = d_mdw$dr_antall_ruter
d_mdw = as.data.frame(bind_cols(d_mdw, select(md, one_of(comp.info))))
d_mdw[dr_harm.var_ruter] = d_mdw[dr_harm.var_ruter] * d_mdw$mutipl.col

d_mdw.fm = as.data.frame(d_mdw %>%
                             group_by(aar) %>%
                             summarise_each(funs(sum), one_of(as.character(dr_harm.var_ruter))))


d_mdw.fm$id = 999
d_mdw.fm$idaar = as.numeric(paste(d_mdw.fm$id, d_mdw.fm$aar, sep = ""))
d_mdw.fm$id = NULL
d_mdw.fm$aar = NULL


mds = inner_join(mds, d_mdw.fm, by = "idaar")
mds[dr_harm.var_ruter] = mds[dr_harm.var_ruter] / mds$dr_aruter_sum


r_mdw = select(md, one_of(rr_harm.var_ruter))
r_mdw$mutipl.col = r_mdw$rr_antall_ruter
r_mdw = as.data.frame(bind_cols(r_mdw, select(md, one_of(comp.info))))
r_mdw[rr_harm.var_ruter] = r_mdw[rr_harm.var_ruter] * r_mdw$mutipl.col

r_mdw.fm = as.data.frame(r_mdw %>%
                                 group_by(aar) %>%
                                 summarise_each(funs(sum), one_of(as.character(rr_harm.var_ruter))))


r_mdw.fm$id = 999
r_mdw.fm$idaar = as.numeric(paste(r_mdw.fm$id, r_mdw.fm$aar, sep = ""))
r_mdw.fm$id = NULL
r_mdw.fm$aar = NULL

mds = inner_join(mds, r_mdw.fm, by = "idaar")
mds[rr_harm.var_ruter] = mds[rr_harm.var_ruter] / mds$rr_aruter_sum
mds$selskap = as.character(comp.name)
mds$navn = mds$selskap


mds$omraadepris_t2 = (md %>%
                        group_by(aar) %>%
                        summarise_each(funs(mean), one_of(as.character(merge.pr))))$omraadepris_t2

dat.harm = bind_rows(dat.harm, mds)
dat.harm = dat.harm[!(dat.harm$orgnr %in% fun.comps),]
rm(d_mdw, d_mdw.fm, mds, r_mdw, r_mdw.fm)

