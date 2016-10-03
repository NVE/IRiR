## IRiR


#### R-teknisk oppsett ####

#remove all objects from memory
remove(list=ls())
#get current working directory
getwd()
#set working directory to where the data file is located
#the address can be copied from the address bar in Windows Explorer
#remember to change "\" to "/" or "\\" 
#my.path = "C:\\users\\roam\\Dropbox\\IRcalc i R"
my.path = "\\\\nve\\fil\\etø\\Ansatte\\mohh\\IRiR"
setwd(my.path)
#load benchmarking package of Bogetoft & Otto
library(Benchmarking)
source("functions_nve.R")
#ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)


#### Importerer data ####

#read data set from csv-file
# Grunnlagsdata
dat = read.csv("./Data/Grunnlagsdata/Grunnlagsdata_faktiskvarsel.csv",sep=",")
# ID-er
id = read.csv("./Data/Grunnlagsdata/id.csv", sep = ",")
#Merger Id-er inn i Grunnlagsdata
dat = merge.data.frame(dat, id, by = "orgnr", all.x = TRUE)
dat$selskap <- as.character(dat$selskap)
dat$navn<- as.character(dat$navn)

#Legger manuelt til IDer til selskapene som mangler
#IDer basert på Stata-kode
#Angir ny ID for Gassco
dat$id[dat$orgnr == 983452841] = 900
dat$navn[dat$orgnr == 983452841] = "Gassco"
#Angir ny ID for Lyse sentralnett
dat$id[dat$orgnr == 996325458] = 872
dat$navn[dat$orgnr == 996325458] = "Lyse Sentralnett"
#Angir ny ID for Mørenett
dat$id[dat$orgnr == 912631532] = 460
dat$navn[dat$orgnr == 912631532] = "Morenett"

dat$idaar = paste(dat$id, dat$aar, sep="")
dat$orgnraar = paste(dat$aar, dat$orgnr, sep="")
dat$idaar = as.numeric(dat$idaar)
dat$orgnraar = as.numeric(dat$orgnraar)

##Sjekker om noen mangler id. .
manglende.id = dat.id[is.na(dat.id$id),]
manglende.id[c("selskap", "orgnr")]

# KPI-data
kpi = read.csv("./Data/Grunnlagsdata/KPIdata2016Varsel.csv", sep = ",")
        # legger til KPI-data for alle observasjoner i settet
dat = merge.data.frame(dat, kpi, by="aar", all.x = TRUE)

# Fikse på data for Hammerfest
hfmo = read.csv("./Data/Grunnlagsdata/Hammerfest_Melkoya.csv", sep = ",")
        # Inkluderer bokførte verdier for Hammerfest
dat = merge.data.frame(dat, hfmo, by="idaar", all.x = TRUE)
        ### Her er det klønete kode Endre kan hjelpe oss med
dat$r_abbfv_melk[is.na(dat$r_abbfv_melk)] = 0
dat$r_abavs_melk[is.na(dat$r_abavs_melk)] = 0
dat$tempbfv = dat$r_abbfv
dat$tempavs = dat$r_abavs
dat$r_abbfv = dat$tempbfv - dat$r_abbfv_melk
dat$r_abavs = dat$tempavs - dat$r_abavs_melk
dat$tempbfv = NULL
dat$tempavs = NULL

# Data fra Varsel 15
v15 = read.csv("./Data/Grunnlagsdata/Varsel15.csv", sep = ",")
v15dv = read.csv("./Data/Grunnlagsdata/dv_totxdea_varsel15.csv", sep = ",")

# Fjerner overflødig data
rm(manglende.id, id, hfmo)


#### Definerer parametre i analysen ####

kraftpris = 0.26135
snitt.aar = 2010:2014
faktisk.aar = 2014
IR.aar = faktisk.aar + 2

#Varsel/Vedtak
Vedtak = 0 # 1 ved vedtak, 0 ved varsel

#Varsel
nve.rente.t2 = 0.0661
nve.rente.estimert = 0.0639
systempris.t2 = 0.26135

#Vedtak
nve.rente.t = 0.0639

#Økonomiske forutsetniger
arb.kap.paaslag = 1.01
rho = 0.6
d_grs_pris = 1


#### TOTEX Beregninger ####

#compute totex for D-nett
dat$d_DV = dat$d_DVxL + dat$d_lonn - dat$d_lonnakt + dat$d_pensj + dat$d_pensjek - dat$d_impl
dat$d_AKG = (dat$d_bfv + dat$d_abbfv)*arb.kap.paaslag
dat$d_AVS = dat$d_avs + dat$d_abavs
dat$d_totco = dat$d_DV - dat$d_utred - dat$d_391 + dat$d_AKG*nve.rente.t2 + dat$d_AVS + dat$d_kile + dat$d_nettap*kraftpris

#compute totex for R-nett
dat$r_DV = dat$r_DVxL + dat$r_lonn - dat$r_lonnakt + dat$r_pensj + dat$r_pensjek - dat$r_impl
dat$r_AKG = (dat$r_bfv + dat$r_abbfv)*arb.kap.paaslag
dat$r_AVS = dat$r_avs + dat$r_abavs
dat$r_totco = dat$r_DV - dat$r_utred - dat$r_391 + dat$r_AKG*nve.rente.t2 + dat$r_AVS + dat$r_kile

#beregner snitt av kostnader og output 
#legger snitt-tallet inn i rad for faktisk år for hvert selskap, men oppretter ny kolonne

#snittdata for D-nett
for(i in which(dat$aar == faktisk.aar))
{
  dat[i,"d_totco_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_totco"])
  dat[i,"d_ab_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ab"])
  dat[i,"d_hs_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_hs"])
  dat[i,"d_ns_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ns"])
}

#snittdata for R-nett
for(i in which(dat$aar == faktisk.aar))
  {
  dat[i,"r_totco_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_totco"])
  dat[i,"r_vluft_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vluft"])
  dat[i,"r_vjord_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vjord"])
  dat[i,"r_vsjo_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vsjo"])
  dat[i,"r_vgrs_snitt"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vgrs"])
  }


#### Utvalg av selskaper ####

#diverse utvalg av selskaper, basert på orgnr
#de som skal måles
eval.r = dat$orgnr[dat$r_totco >= 7000 & dat$r_vluft > 0 & dat$aar == faktisk.aar]
#de som kan danne fronten
front.r = dat$orgnr[dat$r_totco >= 15000 & dat$aar == faktisk.aar]
#de som skal evalueres men ikke kan være på fronten
sep.eval.r = setdiff(eval.r,front.r)


#### Velger data for selskapene som skal evalueres ####
#faktiske data for selskaper som skal evalueres
x.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"r_totco"]
y.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]
z.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
kap.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_AKG")]
names(x.faktisk.r) = eval.r
rownames(y.faktisk.r) = eval.r
rownames(z.faktisk.r) = eval.r
names(kap.faktisk.r) = eval.r


#### Beregner snitt for selskapene som skal evalueres ####
#snittdata for selskaper som skal evalueres
x.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"r_totco_snitt"]
y.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_vluft_snitt","r_vjord_snitt","r_vsjo_snitt","r_vgrs_snitt")]
z.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
names(x.snitt.r) = eval.r
rownames(y.snitt.r) = eval.r
rownames(z.snitt.r) = eval.r


#### Trinn 1 - DEA-kjøringer ####

#hovedkjøring trinn 1
#merk at fronten defineres av de radene i x.snitt.r og y.snitt.r som tilvhører selskapene i front.r
res.tmp1 = dea(X=x.snitt.r,Y=y.snitt.r,XREF=x.snitt.r[as.character(front.r)],YREF=y.snitt.r[as.character(front.r),],RTS="crs")
#plot(sort(res.snitt.snitt.r$eff))

#spesialkjøring for selskaper som bare kan være front for seg selv
eff.snitt.snitt.r = res.tmp1$eff
lambda.snitt.snitt.r = cbind(res.tmp1$lambda,matrix(NA,nrow=nrow(res.tmp1$lambda),ncol=length(sep.eval.r)))
colnames(lambda.snitt.snitt.r) = c(front.r,sep.eval.r)
for(i in sep.eval.r)
  {
  res.tmp2 = dea(X=x.snitt.r,Y=y.snitt.r,RTS="crs",XREF=x.snitt.r[as.character(c(front.r,i))],YREF=y.snitt.r[as.character(c(front.r,i)),])
  eff.snitt.snitt.r[as.character(i)] = res.tmp2$eff[as.character(i)] 
  for(j in c(front.r,i))
    lambda.snitt.snitt.r[as.character(i),as.character(j)] = res.tmp2$lambda[as.character(i),paste("L_",as.character(j),sep="")]
  }
remove(res.tmp1)
remove(res.tmp2)


plot(sort(eff.snitt.snitt.r))
View(lambda.snitt.snitt.r)


####  Trinn 2 - RVK-justering vha regresjon ####

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

