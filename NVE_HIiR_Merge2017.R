
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
my.path = "C:\\Users\\ens\\Dropbox\\Jobb\\GitHub\\IRiR"
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



# Bootstrap settings
BS.new = 1 # Dummy variable determining wether to calculate new bootstrap estimates (1) or reuse last calculation
BS.ite = 2000 # Number of iterations in bootstrap calculation

#Companies for merger
# Comp 1 
merg.comp1 = c(971029390) # Alta Kraftlag SA  # REPLACE ORGN in merg.comp1 & merg.comp2 with companies relevant for calculation.
# Comp 2
merg.comp2 = c(988807648) # Kragerø Energi AS # Merger of Alta Kraftlag SA and Kragerø Energi is only applied script demo purposes.


merg.comps = c(merg.comp1, merg.comp2)

comp.name = c("KrAlta Nett") # Insert name of merged company


fusjon_TEN = c(978631029, 980498646, 947576283, # Trønderenergi Nett (215), Selbu Energiverk (184), Kraftverkene i Orkla (288),
               947590618, 878631072) # Driva Kraftverk (271), Trønderenergi Kraft (484)


source("./R-script/functions_nve.R") # File containing functions created for/by NVE

# Avoid showing large numbers in scientific mode
options(scipen = 100)


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

#Defining parameters and importing data
source("./R-script/Harmony/H_0_1_Config_Assumptions_Data_Merge2017.R")

#Averages of interest and prices are used in Harmony Income calculation
NVE.ir.t_2 = mean(NVE.ir.new[names(NVE.ir.new) %in% y.avg])
NVE.ir.est = mean(NVE.ir.new[names(NVE.ir.new) %in% y.avg])
sysp.t_2 = mean(his.sysp[names(his.sysp) %in% y.avg])/1000

ir.dea                  = NVE.ir.t_2 # Average interest from years in y.avg
NVE.ir.RC               = NVE.ir.est # Average interest from years in y.avg
pnl.dea                 = sysp.t_2 # Average prices from years in y.avg --> suggestion last five years
pnl.rc                  = sysp.t_2 # Average prices from years in y.avg --> suggestion last five years


#Adapting to mergers involving Trønderenergi Nett AS (peer in Regional distribution)
TEN_data = merge_NVE(comps = fusjon_TEN, new.org = 991991991, new.id = 215, new.name = "Trønderenergi Nett AS (fusjonert)",
                     sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc,
                     rdz_weighted.var = rdz_harm.var_gc,merge.mean = merge.pr, df =dat)$mds

dat = bind_rows(dat, TEN_data)
dat = dat[!(dat$orgn %in% fusjon_TEN),]

dat = dat[order(dat$id.y),]

source("./R-script/Harmony/H_0_2_Calculated_Input_Values.R")

source("./R-script/0_3_Company_Selection.R")


#### Stage 1 - DEA ####
source("./R-script/1_0_DEA.R")

#### Stage 2 - Z factor adjustment using OLS ####
# As described in report 71/2012, see above
# Techincal description in: "Second stage adjustment for firm heterogeneity in DEA:
# A novel approach used in regulation of Norwegian electricity DSOs, H.M. Kvile, O. Kordahl, T. Langset & R. Amundsveen, 2014"
# ENG: http://bit.ly/2sH5oLV
source("./R-script/Harmony/H_2_0_Stage2_GeoCorrection_Pre.R")


#### Stage 3 - Cost norm calibration ####
# As described in circular 1/2013
# NOR http://webfileservice.nve.no/API/PublishedFiles/Download/201607005/1944365
# Based on analysis in report 11/2011
# NOR http://publikasjoner.nve.no/rapport/2011/rapport2011_21.pdf

source("./R-script/Harmony/H_3_0_Stage3_Calibration.R")


#### Companies exempted from DEA - Special models ####

source("./R-script/Harmony/H_Spec_OOTO-model.R")
source("./R-script/Harmony/H_Spec_AvEff-model.R")


#### Calculating Revenue caps ####
source("./R-script/Harmony/H_4_0_Revenue_Cap_Calculation.R")

source("./R-script/Harmony/H_analysis.R")

KeyFigOrg_Pre = (KeyFigorgn[ , ! apply(KeyFigorgn, 2, function (x) all(is.na(x)))])

comp_pre = dat[dat$orgn %in% merg.comps, ] # Finn ut hva jeg vil se her.
RC_pre = RevCap[, c("id", "orgn", "comp", "lrt_RAB.sf", "lrt_cost.RC", "lrt_cn.pre.recal", "lrt_RC.pre.recal")]
RC_pre$lrt_RC.pre.recal = round(RC_pre$lrt_RC.pre.recal)

rm(list = ls()[!ls() %in% c('comp_pre', 'RC_pre', 'BS.new', 'BS.ite', 'merg.comps', 'comp.name',
                            'harm.var_sum', 'ldz_harm.var_gc', 'rdz_harm.var_gc', 'merge.pr',
                            'fusjon_TEN', 'KeyFigOrg_Pre', 'start.time')])


disc.rate = 0.045
n.years = 30

source("./R-script/functions_nve.R") # File containing functions created for/by NVE

# Avoid showing large numbers in scientific mode
options(scipen = 100)

#Defining parameters and importing data
source("./R-script/Harmony/H_0_1_Config_Assumptions_Data_Merge2017.R")


# Removes ID of merging company/companies from rd_sep.eval,
# if one of merging companies was evaluated in seperate DEA-run.
# The groups for special treatment require manual quality assurance

rd_sep.eval = subset(rd_sep.eval, !(rd_sep.eval %in% unique(comp_pre$id)))

#Averages of interest and prices are used in Harmony Income calculation
NVE.ir.t_2 = mean(NVE.ir.new[names(NVE.ir.new) %in% y.avg])
NVE.ir.est = mean(NVE.ir.new[names(NVE.ir.new) %in% y.avg])
sysp.t_2 = mean(his.sysp[names(his.sysp) %in% y.avg])/1000

ir.dea                  = NVE.ir.t_2 # Average interest from years in y.avg
NVE.ir.RC               = NVE.ir.est # Average interest from years in y.avg
pnl.dea                 = sysp.t_2 #HARD CODED ONLY FOR QA, should be "sysp.t_2" # Average prices from years in y.avg --> suggestion last five years
pnl.rc                  = sysp.t_2 #HARD CODED ONLY FOR QA, should be "sysp.t_2" # Average prices from years in y.avg --> suggestion last five years

#Adapting to mergers involving Trønderenergi Nett AS (peer in Regional distribution)
TEN_data = merge_NVE(comps = fusjon_TEN, new.org = 991991991, new.id = 215, new.name = "Trønderenergi Nett AS (fusjonert)",
                     sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc,
                     rdz_weighted.var = rdz_harm.var_gc,merge.mean = merge.pr, df =dat)$mds

dat = bind_rows(dat, TEN_data)
dat = dat[!(dat$orgn %in% fusjon_TEN),]


### Merging companies in this analysis
new_data = merge_NVE(comps = merg.comps, new.org = 999999999, new.id = 999, new.name = comp.name,
                     sum_variables = harm.var_sum, ldz_weighted.var = ldz_harm.var_gc,
                     rdz_weighted.var = rdz_harm.var_gc,merge.mean = merge.pr, df =dat)$mds

dat = bind_rows(dat, new_data)
dat = dat[!(dat$orgn %in% merg.comps),]

dat = dat[order(dat$id.y),]

source("./R-script/Harmony/H_0_2_Calculated_Input_Values.R")

source("./R-script/0_3_Company_Selection.R")


#### Stage 1 - DEA ####
source("./R-script/1_0_DEA.R")

#### Stage 2 - Z factor adjustment using OLS ####
# As described in report 71/2012, see above
# Techincal description in: "Second stage adjustment for firm heterogeneity in DEA:
# A novel approach used in regulation of Norwegian electricity DSOs, H.M. Kvile, O. Kordahl, T. Langset & R. Amundsveen, 2014"
# ENG: http://bit.ly/2sH5oLV
source("./R-script/Harmony/H_2_0_Stage2_GeoCorrection_Post.R")


#### Stage 3 - Cost norm calibration ####
# As described in circular 1/2013
# NOR http://webfileservice.nve.no/API/PublishedFiles/Download/201607005/1944365
# Based on analysis in report 11/2011
# NOR http://publikasjoner.nve.no/rapport/2011/rapport2011_21.pdf

source("./R-script/Harmony/H_3_0_Stage3_Calibration.R")


#### Companies exempted from DEA - Special models ####

source("./R-script/Harmony/H_Spec_OOTO-model.R")
source("./R-script/Harmony/H_Spec_AvEff-model.R")


#### Calculating Revenue caps ####
source("./R-script/Harmony/H_4_0_Revenue_Cap_Calculation.R")

source("./R-script/Harmony/H_analysis_post.R")

KeyFigOrg_Post = (KeyFigorgn[ , ! apply(KeyFigorgn, 2, function (x) all(is.na(x)))])

KeyFigOrg = bind_rows(KeyFigOrg_Pre, KeyFigOrg_Post)

# Sum revenue cap for merging companies before merge
RC_sep.comp = sum(RC_pre[RC_pre$orgn %in% merg.comps, "lrt_RC.pre.recal"])

comp_post = dat[dat$id == 999, ] # Finn ut hva jeg vil se her.
RC_post = RevCap[, c("id", "orgn", "comp", "lrt_RAB.sf", "lrt_cost.RC", "lrt_cn.pre.recal", "lrt_RC.pre.recal")]
RC_post$lrt_RC.pre.recal = round(RC_post$lrt_RC.pre.recal)
RC_merg.comp = RC_post[RC_post$id == 999, "lrt_RC.pre.recal" ]

# Yearly income loss
y.inc.loss = RC_sep.comp - RC_merg.comp

HI = round(y.inc.loss + y.inc.loss*((1-((1/(1+disc.rate))^(n.years-1)))/disc.rate))

end.time =  Sys.time()
calc.time = end.time - start.time
calc.time 
y.inc.loss # Revenue Cap loss in revenue cap year. NB! Negative value means there is revenue cap gain, which does not yield a compensation
HI # Harmony Income, net present value of y.inc.loss over 30 years discounted with 4.5% real interest rate.
View(KeyFigOrg)