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
my.path = "C:\\Users\\ens\\Jottacloud\\GitHub\\IRiR"
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
#Tilegner ID-er til Grunnlagsdata vha merge
dat.id = merge.data.frame(dat, id, by = "orgnr", all.x = TRUE)
dat.id$selskap <- as.character(dat.id$selskap)
dat.id$navn<- as.character(dat.id$navn)

#Legger manuelt til IDer til selskapene som mangler
#IDer basert på Stata-kode
#Angir ny ID for Gassco
dat.id$id[dat.id$orgnr == 983452841] <- 900
dat.id$navn[dat.id$orgnr == 983452841] <- "Gassco"
#Angir ny ID for Lyse sentralnett
dat.id$id[dat.id$orgnr == 996325458] <- 872
dat.id$navn[dat.id$orgnr == 996325458] <- "Lyse Sentralnett"
#Angir ny ID for Mørenett
dat.id$id[dat.id$orgnr == 912631532] <- 460
dat.id$navn[dat.id$orgnr == 912631532] <- "Morenett"

#lager idaar og orgnraar variabler og endrer type for disse
dat.id$idaar <- paste(dat.id$id, dat.id$aar, sep="")
dat.id$orgnraar <- paste(dat.id$aar, dat.id$orgnr, sep="")
dat.id$idaar <- as.numeric(dat.id$idaar)
dat.id$orgnraar <- as.numeric(dat.id$orgnraar)

# Endrer d_ab for MIP i direkte i datarket
dat.id$d_ab[dat.id$idaar == 7432010] <- 248
dat.id$d_ab[dat.id$idaar == 7432011] <- 246
dat.id$d_ab[dat.id$idaar == 7432012] <- 246
dat.id$d_ab[dat.id$idaar == 7432013] <- 245

dat.id <- dat.id[!(dat.id$orgnr==962986633),] ## Sletter observasjoner fra Statnett

##Sjekker om noen mangler id. .
manglende.id <- dat.id[is.na(dat.id$id),]
manglende.id[c("selskap", "orgnr")]

rm(manglende.id, id, dat)


#### Fjerner bestemte selskap fra datasettet basert på id ####


## Selskaper som av diverse årsaker er unntat vanlig DEA- eller IR-regulering
#Først for D-nett

d_spesial <- (c(10, 23, 108, 121, 167, 222, 512, 686, 743))

d_dea_til_gjsnitt <- (c(294, 652, 852))

d_dmuer <- (c())

d_ikkeIR <- (c(134, 348, 521, 612, 638, 696)) # IDene finnes ikke

#Deretter for RS-nett
r_spesial <- (c(10, 18, 35, 41, 88, 98, 106, 116, 135, 147, 156, 161, 162, 173,
                 184, 187, 204, 222, 238, 274, 287, 307, 343, 349, 484, 512, 549
                 , 659, 686, 743)) # prøver, 10, her 

r_separat_dmuer <- (c(7, 9, 14, 37, 62, 63, 65, 93, 103, 138, 146, 152, 164, 
                       197, 206, 251, 257, 271, 275, 288, 295, 447, 464, 591, 
                       625, 637, 669, 753))  # 14, 447, 753 

r_dea_til_gjsnitt <- (c(183, 685, 542, 852, 900, 872))

# KPI-data
kpi = read.csv("./Data/Grunnlagsdata/KPIdata2016Varsel.csv", sep = ",")
        # legger til KPI-data for alle observasjoner i settet
dat.id = merge.data.frame(dat.id, kpi, by="aar", all.x = TRUE)

# Data for Hammerfest
hfmo = read.csv("./Data/Grunnlagsdata/Hammerfest_Melkoya.csv", sep = ",")
        # Inkluderer bokførte verdier for Hammerfest
dat.id = merge.data.frame(dat.id, hfmo, by="idaar", all.x = TRUE)
### Her er det klønete kode Endre kan hjelpe oss med
dat.id$r_abbfv_melk[is.na(dat.id$r_abbfv_melk)] <- 0
dat.id$r_abavs_melk[is.na(dat.id$r_abavs_melk)] <- 0

dat.id$tempbfv <- dat.id$r_abbfv
dat.id$tempavs <- dat.id$r_abavs
dat.id$r_abbfv = dat.id$tempbfv - dat.id$r_abbfv_melk
dat.id$r_abavs = dat.id$tempavs - dat.id$r_abavs_melk
dat.id$tempbfv <- NULL
dat.id$tempavs <- NULL
rm(hfmo)

# Data fra Varsel 15
v15 = read.csv("./Data/Grunnlagsdata/Varsel15.csv", sep = ",")
v15dv = read.csv("./Data/Grunnlagsdata/dv_totxdea_varsel15.csv", sep = ",")


#### Definerer parametre i analysen ####
## parametre
kraftpris = 0.26135
snitt.aar = 2010:2014
faktisk.aar = 2014
IR.aar = faktisk.aar + 2

#Varsel/Vedtak

vedtak = 0 # 1 ved vedtak, 0 ved varsel

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


#### Beregner pensjonskostnader i faste priser ved hjelp av kpia####

dat.id$fp_d_pensj = dat.id$d_pensj*dat.id$kpia
dat.id$fp_d_pensjek = dat.id$d_pensjek * dat.id$kpia
dat.id$fp_d_impl = dat.id$d_impl * dat.id$kpia
dat.id$fp_r_pensj = dat.id$r_pensj*dat.id$kpia
dat.id$fp_r_pensjek = dat.id$r_pensjek * dat.id$kpia
dat.id$fp_r_impl = dat.id$r_impl * dat.id$kpia
dat.id$fp_s_pensj = dat.id$s_pensj*dat.id$kpia
dat.id$fp_s_pensjek = dat.id$s_pensjek * dat.id$kpia
dat.id$fp_s_impl = dat.id$s_impl * dat.id$kpia


#### Femårig snitt av pensjonskostnader i løpende priser til kostnadsgrunnlag ####

for(i in which(dat.id$aar %in% snitt.aar))
{
        dat.id[i,"av_d_pensj"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"d_pensj"], na.rm = T)
        dat.id[i,"av_d_pensjek"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"d_pensjek"], na.rm = T)
        dat.id[i,"av_d_impl"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"d_impl"], na.rm = T)
        dat.id[i,"av_r_pensj"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"r_pensj"], na.rm = T)
        dat.id[i,"av_r_pensjek"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"r_pensjek"], na.rm = T)
        dat.id[i,"av_r_impl"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"r_impl"], na.rm = T)
        dat.id[i,"av_s_pensj"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"s_pensj"], na.rm = T)
        dat.id[i,"av_s_pensjek"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"s_pensjek"], na.rm = T)
        dat.id[i,"av_s_impl"] = mean(dat.id[dat.id$orgnr == dat.id$orgnr[i] & dat.id$aar %in% snitt.aar,"s_impl"], na.rm = T)
}


#### Pensjonskostnadsgrunnlaget etablers for alle nettnivåer

dat.id$d_pensjkostgrlag = dat.id$av_d_pensj + dat.id$av_d_pensjek + dat.id$av_d_impl
dat.id$r_pensjkostgrlag = dat.id$av_r_pensj + dat.id$av_r_pensjek + dat.id$av_r_impl
dat.id$s_pensjkostgrlag = dat.id$av_s_pensj + dat.id$av_s_pensjek + dat.id$av_s_impl

#### TOTEX Beregninger ####

## Etablerer dummy for om selskapet kan være mønsterselskap (brukes i trinn 1)
dat.id$frontlov = "."

## Velger deretter korrekte renter avhengig om kjøringen er i varsel eller vedtaksmodus
if (vedtak == 1)  {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.t
        nettapspris.dea = systempris.t2
} else {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.estimert
        nettapspri.dea = systempris.t2
}



#compute totex for D-nett

d_DV = dat.id$d_DVxL+dat.id$d_lonn-dat.id$d_lonnakt+dat.id$d_pensjkostgrlag # Har erstattet noe her ihht Roars kode
d_AKG = (dat.id$d_bfv+dat.id$d_abbfv)*arb.kap.paaslag
d_AVS = dat.id$d_avs+dat.id$d_abavs
d_totco = d_DV-dat.id$d_utred-dat.id$d_391+d_AKG*nve.rente.t2+d_AVS+dat.id$d_kile+dat.id$d_nettap*kraftpris

#compute totex for R-nett
r_DV = dat.id$r_DVxL+dat.id$r_lonn-dat.id$r_lonnakt+dat.id$r_pensjkostgrlag # Har erstattet noe her ihht Roars kode
r_AKG = (dat.id$r_bfv+dat.id$r_abbfv)*arb.kap.paaslag
r_AVS = dat.id$r_avs+dat.id$r_abavs
r_totco = r_DV-dat.id$r_utred-dat.id$r_391+r_AKG*nve.rente.t2+r_AVS+dat.id$r_kile
  #+dat.id$r_nettap*kraftpris (nettap skal ikke være med i R-nett)

#nytt datasett som inneholder alle variabler
dat = cbind(dat.id,r_DV,r_AKG,r_AVS,r_totco,d_DV,d_AKG,d_AVS,d_totco)
rm (dat.id)


## Beregner gjennomsnittsfront
        # Først opprettes verdier i faste priser
        # Variabler som justeres med vanlig KPI

dat$fp_d_kile = dat$d_kile * dat$kpi
dat$fp_r_kile = dat$d_kile * dat$kpi
dat$fp_s_kile = dat$s_kile * dat$kpi

        #Variabler som justeres med KPI for varer og tjenester med arbeidslønn
        # som dominerende prisfaktor. Tabell 03363. 
dat$fp_d_DV = dat$d_DV*dat$kpia
dat$fp_r_DV = dat$r_DV*dat$kpia
dat$fp_s_DV = dat$s_DV*dat$kpia

dat$fp_d_391 = dat$d_391*dat$kpia
dat$fp_r_391 = dat$r_391*dat$kpia
dat$fp_s_391 = dat$s_391*dat$kpia

dat$fp_d_dv = dat$d_dv*dat$kpia # Feiler pga måten vi beregner TOTEX kontra Roar
dat$fp_d_utred = dat$d_utred*dat$kpia

dat$fp_r_dv = dat$r_dv*dat$kpia # Feiler pga måten vi beregner TOTEX kontra Roar
dat$fp_r_utred = dat$r_utred*dat$kpia

dat$fp_s_dv = dat$s_dv*dat$kpia # Feiler pga måten vi beregner TOTEX kontra Roar


#beregner snitt av kostnader og output 
#legger snitt-tallet inn i rad for faktisk år for hvert selskap, men oppretter ny kolonne
#snittdata for D-nett
for(i in which(dat$aar == faktisk.aar))
{
  dat[i,"av_d_totco"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_totco"])
  dat[i,"av_d_ab"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ab"])
  dat[i,"av_d_hs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_hs"])
  dat[i,"av_d_ns"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ns"])
}

#snittdata for R-nett
for(i in which(dat$aar == faktisk.aar))
  {
  dat[i,"av_r_totco"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_totco"])
  dat[i,"av_r_vluft"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vluft"])
  dat[i,"av_r_vjord"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vjord"])
  dat[i,"av_r_vsjo"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vsjo"])
  dat[i,"av_r_vgrs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vgrs"])
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
x.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"av_r_totco"]
y.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("av_r_vluft","av_r_vjord","av_r_vsjo","av_r_vgrs")]
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

