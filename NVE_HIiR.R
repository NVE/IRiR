
# HIiR
# Script for Harmony Income Calculation

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
library(plyr)
library(dplyr)
library(outliers)
library(plot3D)
library(assertthat)

source("./R-script/functions_nve.R") # File containing functions created for/by NVE

# Avoid showing large numbers in scientific mode
options(scipen = 100)

#Defining parameters and importing data
source("./R-script/Harmony/H_0_1_Config_Assumptions_Data.R")

#Averages of interest and prices are used in Harmony Income calculation
HI.NVE.ir_2 =  


#Companies for merger
# Comp 1 
merg.comp1 = c(971592117)
# Comp 2
merg.comp2 = c(983099807)

merg.comps = c(merg.comp1, merg.comp2)

comp.name = c("NordBal")



dat.harm$ldz_n.mgc_sum = dat.harm$ldz_mgc # Number of map grid cells for "sum"-vector
dat.harm$rdz_n.mgc_sum = dat.harm$rdz_mgc

#Variables that are summed for all merging companies
harm.var_sum = c("ld_391", "ld_OPEXxS", "ld_sub", "ld_dep.gf", "ld_bv.gf", "ldz_isl", "ld_dep.sf",
                 "ld_bv.sf", "ld_gci", "ld_hv", "ld_hvug", "ld_hvoh", "ld_hvsc", "ld_impl",
                 "ld_cens", "ld_sal", "ld_sal.cap", "ld_nl", "ld_ss", "ld_pens",
                 "ld_pens.eq", "ldz_cmpp", "ld_cga", "rd_391", "rd_OPEXxS", "rd_dep.gf",
                 "rd_dep.gf_melk", "rd_bv.gf", "rd_bv.gf_melk", "rd_dep.sf", "rd_bv.sf", "rd_impl",
                 "rd_cens", "rd_sal", "rd_sal.cap", "rd_nl", "rd_pens", "rd_pens.eq",
                 "rd_cga", "rd_wv.ss", "rd_wv.uc", "rd_wv.ol", "rd_wv.sc", "t_391", "t_OPEXxS",
                 "t_dep.sf", "t_bv.sf", "t_impl", "t_cens", "t_sal", "t_sal.cap", "t_pens",
                 "t_pens.eq", "ldz_n.mgc_sum", "rdz_n.mgc_sum")


#variables weighted with number of map grid cells, local distribution grid
ldz_harm.var_gc = c("ldz_mgc", "ldz_lat.av",
                    "ldz_inc.av", "ldz_ice.av", "ldz_cod2c", "ldz_f4", "ldz_f7",
                    "ldz_snow", "ldz_temp", "ldz_wind")

#variables weighted with number of map grid cells, regional distribution grid
rdz_harm.var_gc = c("rdz_mgc", "rdz_inc.av", "rdz_f12")
                 
#Prices are calculated as naive averages
merge.pr = ("ap.t_2")

# Create data frame with observations for merging companies
md = filter(dat.harm, dat.harm$orgn %in% merg.comps)

# Create data frame with sum of variables in harm.var_sum, for merging companies
mds =   as.data.frame(md %>%
                              group_by(y) %>%
                              summarise_each(funs(sum), one_of(as.character(harm.var_sum))))  

mds$orgn = 999999999
mds$id = 999
mds$id.y = as.numeric(paste(mds$id, mds$y, sep = ""))

comp.info = c("orgn", "y", "id", "id.y")




ld_mdw = select(md, one_of(ldz_harm.var_gc))
ld_mdw$mutipl.col = ld_mdw$ldz_mgc
ld_mdw = as.data.frame(bind_cols(ld_mdw, select(md, one_of(comp.info))))
ld_mdw[ldz_harm.var_gc] = ld_mdw[ldz_harm.var_gc] * ld_mdw$mutipl.col

ld_mdw.fm = as.data.frame(ld_mdw %>%
                                  group_by(y) %>%
                                  summarise_each(funs(sum), one_of(as.character(ldz_harm.var_gc))))


ld_mdw.fm$id = 999
ld_mdw.fm$id.y = as.numeric(paste(ld_mdw.fm$id, ld_mdw.fm$y, sep = ""))
ld_mdw.fm$id = NULL
ld_mdw.fm$y = NULL

mds = inner_join(mds, ld_mdw.fm, by = "id.y")
mds[ldz_harm.var_gc] = mds[ldz_harm.var_gc] / mds$ldz_n.mgc_sum



rd_mdw = select(md, one_of(rdz_harm.var_gc))
rd_mdw$mutipl.col = rd_mdw$rdz_mgc
rd_mdw = as.data.frame(bind_cols(rd_mdw, select(md, one_of(comp.info))))
rd_mdw[rdz_harm.var_gc] = rd_mdw[rdz_harm.var_gc] * rd_mdw$mutipl.col

rd_mdw.fm = as.data.frame(rd_mdw %>%
                                  group_by(y) %>%
                                  summarise_each(funs(sum), one_of(as.character(rdz_harm.var_gc))))


rd_mdw.fm$id = 999
rd_mdw.fm$id.y = as.numeric(paste(rd_mdw.fm$id, rd_mdw.fm$y, sep = ""))
rd_mdw.fm$id = NULL
rd_mdw.fm$y = NULL

mds = inner_join(mds, rd_mdw.fm, by = "id.y")
mds[rdz_harm.var_gc] = mds[rdz_harm.var_gc] / mds$rdz_n.mgc_sum
mds$comp = as.character(comp.name)
mds$name = mds$comp

mds$ap.t_2 = (md %>%
                      group_by(y) %>%
                      summarise_each(funs(mean), one_of(as.character(merge.pr))))$ap.t_2

dat.harm = bind_rows(dat.harm, mds)
dat.harm = dat.harm[!(dat.harm$orgn %in% merg.comps),]
rm(ld_mdw, ld_mdw.fm, mds, rd_mdw, rd_mdw.fm)
