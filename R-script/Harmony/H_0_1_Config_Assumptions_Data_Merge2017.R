#### 0.1 Configuration, assumptions and data import in Harmony Income calculation####

#### Defining parameters in model####

# Parametre
#kraftpris = 0.26135
pnl.rc = 0.26655 
y.avg = 2012:2016
y.cb = 2016
y.rc = y.cb + 2
y.hist.pen = 2007:2013

#NVE interest rates - used in revenue cap calculation
# https://www.nve.no/energy-market-and-regulation/revenue-regulation/the-wacc-model/
# https://www.nve.no/elmarkedstilsynet-marked-og-monopol/okonomisk-regulering-av-nettselskap/reguleringsmodellen/referanserenten/
NVE.ir = c(0.0619, 0.0562, 0.0531, 0.0420, 0.0690, 0.0661, 0.0632, 0.0632, 0.0612, 0.0588) # 2015 & 2016 estimates pr 01.12.15
names(NVE.ir) = 2009:y.rc
NVE.ir.new = c(0.0808, 0.0767, 0.0764, 0.0718, 0.0690, 0.0661, 0.0632, 0.0632, 0.0612, 0.0588) # Calculated interest rates for 2009-2012
names(NVE.ir.new) = 2009:y.rc                                                        # based on interest model from 2013
his.sysp = c(312.80, 459.92, 393.46, 259.90, 310.99, 261.35, 208.13, 264.74)
names(his.sysp) = 2009:y.cb 


# Decision/Notice - different prices and interests are used
decision = 0 # 1 if decision mode, 0 notice mode

# # Notice
# NVE.ir.t_2 = NVE.ir[as.character(y.cb)]
# NVE.ir.est = NVE.ir[as.character(y.rc)]
# sysp.t_2 = 0.26135
# 
# # Decision
# NVE.ir.t = NVE.ir[as.character(y.rc)]

# Economic assumptions (? - Improve)
wcp = 1.01 # working capital premium
rho = 0.6 # Norm cost share in revenue cap 
gci.p = 1 # Price of grid components in interface, regional and local distribution grids

# 
# # Choosing correct interest rates and prices dependent on notice or decision mode
# if (decision == 1)  {
#         ir.dea                  = NVE.ir.t_2
#         NVE.ir.RC               = NVE.ir.t
#         pnl.dea                 = sysp.t_2
# } else {
#         ir.dea                  = NVE.ir.t_2
#         NVE.ir.RC               = NVE.ir.est
#         pnl.dea                 = sysp.t_2
# }

# Estimated cost for cost base year (y.cb)
# NOT RELEVANT FOR HI CALCULATION, Revenue cap prior to re-calibration is used
lrt_RC_dec.y.cb = 9265621 + 19666

#### Importerer data ####

# Read data set from csv-file
# Base-data with costs and assets
dat = read.csv("./Harmony/BaseData/BDnotice2018_HI_Merge2017.csv",sep=",")

# ID-er
id = read.csv("./Harmony/BaseData/HI_id_Merge2017.csv", sep = ",")
# Tilegner ID-er til Grunnlagsdata vha merge
dat$comp <- as.character(dat$comp)
id$name<- as.character(id$name)
dat = merge.data.frame(dat, id, by = "orgn", all.x = TRUE)

# Replace NA  values with zeros to have correct means for pensions costs
dat[is.na(dat)] = 0

# Create id.y og orgn.y variable
dat$id.y <- paste(dat$id, dat$y, sep="")
dat$orgn.y <- paste(dat$y, dat$orgn, sep="")
dat$id.y <- as.numeric(dat$id.y)
dat$orgn.y <- as.numeric(dat$orgn.y)

# Impute subscriber values for Mo Industripark (id 743)
dat$ld_sub[dat$id.y == 7432010] <- 248
dat$ld_sub[dat$id.y == 7432011] <- 246
dat$ld_sub[dat$id.y == 7432012] <- 246
dat$ld_sub[dat$id.y == 7432013] <- 245

# Sletter observasjoner fra Statnett
dat <- dat[!(dat$orgn==962986633),] 

# Check for companies without ID
missing.id <- dat[is.na(dat$id),]
missing.id[c("comp", "orgn")]
stopifnot(nrow(missing.id) == 0)
rm(missing.id, id)


# # Manually defining group memberships 
#
# # OOTO - Companies Out Of The Ordinary
# These companies are compared to their own historical performance, cost in y.cb vs five year historical average cost in y.avg
# Criterias in local distribution: Number of subscribers (ld_sub) < 500 or kilometers of high voltage grid (ld_hv) < 100
# Criterias in regional distribution/transmission: Sum of all cost weights (rd_vw.ol + rd_wv.uc + rd_wv.sc + rd_wv.ss) < 4000
# or 0 km high voltage grid in regional distribution (rd_vw.ol = 0)
#
## av.eff - Companies set to average efficiency
# Companies with structural breaks in data. (Improve?)
#
## sep.eval - Companies included in DEA but allowed to be their own peers
# Criterias in local distribution:
# Criterias in regional distribution: TOTEX < 15000, .... ( Improve )  

# Local Distribution Grid
ld_ooto <- (c(121, 167, 222, 512, 686, 743, 852)) 
ld_av.eff <- (c(10, 187, 294, 652)) # Companies set to average efficency
ld_sep.eval <- (c()) # Companies included in DEA, but only allowed to be peers for themselves
ld_no.rc <- (c(108, 134, 152, 307, 348, 521, 612, 638, 696, 524)) # Companies exluded from revenue cap calc


# Regional distribution grid (+ some transmission)
rd_ooto <- (c(10, 18,  41, 88, 135, 147, 156, 161, 162, 
              204, 222, 274, 287, 349, 447, 512, 
              659, 686, 852)) # 

rd_av.eff <- (c( 98, 116, 542, 685)) #

rd_sep.eval <- (c(7, 32, 37, 103, 106, 138, 164, 173,
                   206, 238, 295, 625, 669))  # Removed 184, 271, 288, 484 due to merger with TEN.


rd_no.rc  <- (c(35, 152, 307)) # 35 Drangedal har 0 i rd_TOTX 2016

# CPI
cpi = c(81.0, 82.3, 84.2, 84.8, 88.0, 89.9, 92.1, 93.3, 93.9, 95.9, 97.9, 100, 103.6, 105.8, 107.8)
names(cpi) = 2004:y.rc

cpi.l = c(64.4, 67.1, 70.3, 73.7, 77.8, 81.2, 84.7, 87.9, 90.6, 93.8, 97.1, 100, 102.8, 105.3, 108.4)
names(cpi.l) = 2004:y.rc

# Remove observations for LFK, Opplandskraft & Vinstra fra datasettet


# Data for Hammerfest
# Special treatment of CAPEX associated with LNG facility on Melkoya (Improve - ref?)
hfmo = read.csv("./Data/BaseData/Hammerfest_Melkoya.csv", sep = ",")
dat = merge.data.frame(dat, hfmo, by="id.y", all.x = TRUE)
dat$rd_bv.gf_melk[is.na(dat$rd_bv.gf_melk)] <- 0
dat$rd_dep.gf_melk[is.na(dat$rd_dep.gf_melk)] <- 0
dat$tempbv <- dat$rd_bv.gf
dat$tempdep <- dat$rd_dep.gf
dat$rd_bv.gf = dat$tempbv - dat$rd_bv.gf_melk
dat$rd_dep.gf = dat$tempdep - dat$rd_dep.gf_melk
dat$tempbv <- NULL
dat$tempdep <- NULL
rm(hfmo)


# Import area prices pr company from cost base year - area prices allready in base data
# ap.t_2 = read.csv("./data/Basedata/areaprices_t_2.csv", sep = ",")
# dat = merge.data.frame(dat, ap.t_2, by="id.y", all.x = TRUE)
# 
# dat$ap.t_2[dat$id.y==8722014] = 244.24
# dat$ap.t_2[dat$id.y==9002014] = 273.92
dat$ap.t_2 = dat$ap.t_2/1000

#CPI factors are used in calibration and revenue cap-calculations (part 4)
y.cb.cpi.l.factor = cpi.l[as.character(y.rc)]/cpi.l[as.character(y.cb)]
y.cb.cpi.factor = cpi[as.character(y.rc)]/cpi[as.character(y.cb)]
