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
my.path = "C:\\Users\\MOHH\\Documents\\GitHub\\IRiR"
setwd(my.path)
# Load benchmarking package of Bogetoft & Otto
library(Benchmarking)
library(xlsx)
source("functions_nve.R")
# Ønsker å vise store tall som fulle verdier, ikke som potenser
options(scipen = 100)


#### Importerer data ####

# Read data set from csv-file
# Grunnlagsdata
dat = read.csv("./Data/Grunnlagsdata/Grunnlagsdata_faktiskvarsel.csv",sep=",")
# ID-er
id = read.csv("./Data/Grunnlagsdata/id.csv", sep = ",")
# Tilegner ID-er til Grunnlagsdata vha merge
dat = merge.data.frame(dat, id, by = "orgnr", all.x = TRUE)
dat$selskap <- as.character(dat$selskap)
dat$navn<- as.character(dat$navn)

# Legger manuelt til IDer til selskapene som mangler
# IDer basert på Stata-kode
# Angir ny ID for Gassco
dat$id[dat$orgnr == 983452841] <- 900
dat$navn[dat$orgnr == 983452841] <- "Gassco"
# Angir ny ID for Lyse sentralnett
dat$id[dat$orgnr == 996325458] <- 872
dat$navn[dat$orgnr == 996325458] <- "Lyse Sentralnett"
# Angir ny ID for Mørenett
dat$id[dat$orgnr == 912631532] <- 460
dat$navn[dat$orgnr == 912631532] <- "Morenett"

# Lager idaar og orgnraar variabler og endrer type for disse
dat$idaar <- paste(dat$id, dat$aar, sep="")
dat$orgnraar <- paste(dat$aar, dat$orgnr, sep="")
dat$idaar <- as.numeric(dat$idaar)
dat$orgnraar <- as.numeric(dat$orgnraar)

# Endrer d_ab for MIP i direkte i datarket
dat$d_ab[dat$idaar == 7432010] <- 248
dat$d_ab[dat$idaar == 7432011] <- 246
dat$d_ab[dat$idaar == 7432012] <- 246
dat$d_ab[dat$idaar == 7432013] <- 245

# Sletter observasjoner fra Statnett
dat <- dat[!(dat$orgnr==962986633),] 

# Sjekker om noen selskaper mangler id
manglende.id <- dat[is.na(dat$id),]
manglende.id[c("selskap", "orgnr")]

rm(manglende.id, id)


#### Fjerner bestemte selskap fra datasettet basert på id ####

## Selskaper som av diverse årsaker er unntat vanlig DEA- eller IR-regulering
# D-nett
d_spesial <- (c(10, 108, 121, 167, 222, 512, 686, 743))
d_dea_til_gjsnitt <- (c(187, 294, 652, 852))
d_dmuer <- (c())
d_ikkeIR <- (c(134, 348, 521, 612, 638, 696)) # IDene finnes ikke

# R/S-nett
r_spesial <- (c(10, 18, 35, 41, 88, 98, 106, 116, 135, 147, 156, 161, 162, 173,
                 184, 187, 204, 222, 238, 274, 287, 307, 343, 349, 484, 512, 549
                 , 659, 686, 743)) # prøver, 10, her 
r_separat_dmuer <- (c(7, 9, 14, 37, 62, 63, 65, 93, 103, 138, 146, 152, 164, 
                       197, 206, 251, 257, 271, 275, 288, 295, 447, 464, 591, 
                       625, 637, 669, 753))  # 14, 447, 753 
r_dea_til_gjsnitt <- (c(183, 685, 542, 852, 900, 872))

# KPI-data
kpi = read.csv("./Data/Grunnlagsdata/KPIdata2016Varsel.csv", sep = ",")
# Legger til KPI-data for alle observasjoner i settet
dat = merge.data.frame(dat, kpi, by="aar", all.x = TRUE)

# Data for Hammerfest
hfmo = read.csv("./Data/Grunnlagsdata/Hammerfest_Melkoya.csv", sep = ",")
# Inkluderer bokførte verdier for Hammerfest
dat = merge.data.frame(dat, hfmo, by="idaar", all.x = TRUE)
### Her er det klønete kode Endre kan hjelpe oss med
dat$r_abbfv_melk[is.na(dat$r_abbfv_melk)] <- 0
dat$r_abavs_melk[is.na(dat$r_abavs_melk)] <- 0

dat$tempbfv <- dat$r_abbfv
dat$tempavs <- dat$r_abavs
dat$r_abbfv = dat$tempbfv - dat$r_abbfv_melk
dat$r_abavs = dat$tempavs - dat$r_abavs_melk
dat$tempbfv <- NULL
dat$tempavs <- NULL
rm(hfmo)

# Data fra Varsel 15
v15 = read.csv("./Data/Grunnlagsdata/Varsel15.csv", sep = ",")
v15dv = read.csv("./Data/Grunnlagsdata/dv_totxdea_varsel15.csv", sep = ",")


#### Definerer parametre i analysen ####

# Parametre
kraftpris = 0.26135
snitt.aar = 2010:2014
faktisk.aar = 2014
IR.aar = faktisk.aar + 2

# Varsel/Vedtak
vedtak = 0 # 1 ved vedtak, 0 ved varsel

# Varsel
nve.rente.t2 = 0.0661
nve.rente.estimert = 0.0639
systempris.t2 = 0.26135

# Vedtak
nve.rente.t = 0.0639

# Økonomiske forutsetniger
arb.kap.paaslag = 1.01
rho = 0.6
d_grs_pris = 1


#### Pensjonskostnadskjøret ####

dat$fp_d_pensj = dat$d_pensj*dat$kpia
dat$fp_d_pensjek = dat$d_pensjek * dat$kpia
dat$fp_d_impl = dat$d_impl * dat$kpia
dat$fp_r_pensj = dat$r_pensj*dat$kpia
dat$fp_r_pensjek = dat$r_pensjek * dat$kpia
dat$fp_r_impl = dat$r_impl * dat$kpia
dat$fp_s_pensj = dat$s_pensj*dat$kpia
dat$fp_s_pensjek = dat$s_pensjek * dat$kpia
dat$fp_s_impl = dat$s_impl * dat$kpia


## Må beregne "historiske verdier" for perioden 2007-2013
hist.pensj.aar = 2007:2013

dat$temp.d.p = dat$fp_d_pensj
dat$temp.d.pk = dat$fp_d_pensjek
dat$temp.d.i = dat$fp_d_impl
dat$temp.r.p = dat$fp_r_pensj
dat$temp.r.pk = dat$fp_r_pensjek
dat$temp.r.i = dat$fp_r_impl
dat$temp.s.p = dat$fp_s_pensj
dat$temp.s.pk = dat$fp_s_pensjek
dat$temp.s.i = dat$fp_s_impl

for(i in which(dat$aar %in% hist.pensj.aar))
{
dat[i,"fp_d_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.p"], na.rm = T)
dat[i,"fp_d_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.pk"], na.rm = T)
dat[i,"fp_d_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.i"], na.rm = T)
dat[i,"fp_r_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.p"], na.rm = T)
dat[i,"fp_r_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.pk"], na.rm = T)
dat[i,"fp_r_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.i"], na.rm = T)
dat[i,"fp_s_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.p"], na.rm = T)
dat[i,"fp_s_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.pk"], na.rm = T)
dat[i,"fp_s_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.i"], na.rm = T)
}

dat$temp.d.p <-NULL
dat$temp.d.pk <-NULL
dat$temp.d.i <-NULL
dat$temp.r.p <-NULL
dat$temp.r.pk <-NULL
dat$temp.r.i <-NULL
dat$temp.s.p <-NULL
dat$temp.s.pk <-NULL
dat$temp.s.i <-NULL



# Femårig snitt av pensjonskostnader i faste priser til kostnadsgrunnlag
for(i in which(dat$aar %in% snitt.aar))
{
        dat[i,"av_fp_d_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_pensj"], na.rm = T)
        dat[i,"av_fp_d_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_pensjek"], na.rm = T)
        dat[i,"av_fp_d_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_impl"], na.rm = T)
        dat[i,"av_fp_r_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_pensj"], na.rm = T)
        dat[i,"av_fp_r_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_pensjek"], na.rm = T)
        dat[i,"av_fp_r_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_impl"], na.rm = T)
        dat[i,"av_fp_s_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_pensj"], na.rm = T)
        dat[i,"av_fp_s_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_pensjek"], na.rm = T)
        dat[i,"av_fp_s_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_impl"], na.rm = T)
}

# Pensjonskostnadsgrunnlaget etablers for alle nettnivåer
dat$d_pensjkostgrlag = (dat$av_fp_d_pensj + dat$av_fp_d_pensjek - dat$av_fp_d_impl) / dat$kpia
dat$r_pensjkostgrlag = (dat$av_fp_r_pensj + dat$av_fp_r_pensjek - dat$av_fp_r_impl) / dat$kpia
dat$s_pensjkostgrlag = (dat$av_fp_s_pensj + dat$av_fp_s_pensjek - dat$av_fp_s_impl) / dat$kpia


#### TOTEX Beregninger ####

# Konstruerer frontlov-variabel
dat$frontlov = "."

# Velger deretter korrekte renter avhengig om kjøringen er i varsel eller vedtaksmodus
if (vedtak == 1)  {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.t
        nettapspris.dea = systempris.t2
} else {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.estimert
        nettapspris.dea = systempris.t2
}

# Compute totx for D-nett
dat$d_dv = dat$d_DVxL+dat$d_lonn-dat$d_lonnakt+dat$d_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering
dat$d_dv_2012 = dat$d_DVxL +dat$d_lonn - dat$d_lonnakt + dat$d_pensj + dat$d_pensjek
        # Må også ta hensyn til grensesnittkostnadene
dat$d_grs.dummy <- 1

for (i in which(dat$id %in% d_dea_til_gjsnitt)){
        dat[i,"d_grs.dummy"]  = 0 
}

for (i in which(dat$id %in% d_spesial)){
        dat[i,"d_grs.dummy"]  = 0 
}

# Priser inn grensesnittvariabelen. Her med faktor 1.11, ukjent rasjonale bak denne verdien
dat$d_grs.cost <- 0
grs.kostfaktor = 1.11

dat$d_grs.cost <- ifelse(dat$d_grs.dummy==1,dat$d_grs*grs.kostfaktor, 0)

dat$d_DV =  dat$d_dv- dat$d_391- dat$d_utred  
dat$d_akg =  dat$d_bfv*arb.kap.paaslag
dat$d_abakg =  dat$d_abbfv*arb.kap.paaslag
dat$d_AKG =  dat$d_akg + dat$d_abakg
dat$d_AVS =  dat$d_avs + dat$d_abavs
dat$d_nettapkr = dat$d_nettap*nettapspris.dea
dat$d_TOTXDEA =  dat$d_DV+( dat$d_AKG*rente.dea)+ dat$d_AVS + dat$d_kile + dat$d_nettapkr - dat$d_grs.cost



# Compute totx for R-nett
dat$r_dv = dat$r_DVxL+dat$r_lonn-dat$r_lonnakt+dat$r_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering                
dat$r_dv_2012 = dat$r_DVxL +dat$r_lonn - dat$r_lonnakt + dat$r_pensj + dat$r_pensjek

dat$r_DV = dat$r_dv - dat$r_391- dat$r_utred
dat$r_akg = (dat$r_bfv*arb.kap.paaslag)
dat$r_abakg =  dat$r_abbfv*arb.kap.paaslag
dat$r_AKG =  dat$r_akg + dat$r_abakg
dat$r_AVS = dat$r_avs + dat$r_abavs
dat$r_TOTXDEA = dat$r_DV + ( dat$r_AKG*rente.dea) + dat$r_AVS+ dat$r_kile

# Compute totx for S-nett
dat$s_dv = dat$s_DVxL + dat$s_lonn - dat$s_lonnakt + dat$s_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering 
dat$s_dv_2012 = dat$s_DVxL + dat$s_lonn - dat$s_lonnakt +dat$s_pensj + dat$s_pensjek 
dat$s_DV = dat$s_dv - dat$s_391
dat$s_akg = (dat$s_bfv*arb.kap.paaslag) 
dat$s_AKG =  dat$s_akg 
dat$s_AVS = dat$s_avs 
dat$s_TOTXDEA = dat$s_DV + ( dat$s_AKG*rente.dea) + dat$s_AVS+ dat$s_kile


# Beregner gjennomsnittsfront
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

dat$fp_d_dv = dat$d_dv*dat$kpia 
dat$fp_d_utred = dat$d_utred*dat$kpia

dat$fp_r_dv = dat$r_dv*dat$kpia
dat$fp_r_utred = dat$r_utred*dat$kpia

dat$fp_s_dv = dat$s_dv*dat$kpia 

# TOTX til snittfront
# Setter alle NA-observasjoner til 0
#dat[is.na(dat)] = 0

     # D-nett
dat$fp_d_TOTXDEA = dat$fp_d_DV + dat$d_AKG*rente.dea + dat$d_AVS + dat$fp_d_kile + 
        dat$d_nettapkr - dat$d_grs.cost
dat$sf_d_DV = dat$d_DV

     # R-nett
dat$fp_r_TOTXDEA = dat$fp_r_DV + dat$r_AKG*rente.dea + dat$r_AVS + dat$fp_r_kile
dat$sf_r_vluft = dat$r_vluft
dat$sf_r_vjord = dat$r_vjord
dat$sf_r_vsjo = dat$r_vsjo
dat$rd_vgrs =  dat$d_grs +dat$r_vgrs
dat$sf_rd_vgrs = dat$d_grs +dat$r_vgrs
dat$sf_r_vgrs =dat$r_vgrs

# Beregner snitt av kostnader og output 
# Legger snitt-tallet inn i rad for faktisk år for hvert selskap, men oppretter ny kolonne
# Snittfront data for D-nett
for(i in which(dat$aar == faktisk.aar))
{
  dat[i,"sf_d_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_TOTXDEA"])
  dat[i,"sf_d_ab"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ab"])
  dat[i,"sf_d_hs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_hs"])
  dat[i,"sf_d_ns"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ns"])
}

# Snittfront data for R-nett
for(i in which(dat$aar == faktisk.aar))
  {
  dat[i,"sf_r_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_TOTXDEA"])
  dat[i,"sf_r_vluft"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vluft"])
  dat[i,"sf_r_vjord"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vjord"])
  dat[i,"sf_r_vsjo"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vsjo"])
  dat[i,"sf_r_vgrs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vgrs"])
  }

# for(i in which(dat$aar == faktisk.aar))
# {
#         dat[i,"av_d_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_TOTXDEA"])
#         dat[i,"av_d_ab"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ab"])
#         dat[i,"av_d_hs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_hs"])
#         dat[i,"av_d_ns"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ns"])
# }
# 
# for(i in which(dat$aar == faktisk.aar))
# {
#         dat[i,"av_r_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_TOTXDEA"])
#         dat[i,"av_r_vluft"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vluft"])
#         dat[i,"av_r_vjord"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vjord"])
#         dat[i,"av_r_vsjo"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vsjo"])
#         dat[i,"av_r_vgrs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vgrs"])
# }


#### Utvalg av selskaper ####

# # Endre metoden
# # Diverse utvalg av selskaper, basert på orgnr
# # De som skal måles:
# eval.r = dat$orgnr[dat$r_TOTXDEA >= 7000 & dat$r_vluft > 0 & dat$aar == faktisk.aar]
# # De som kan danne fronten:
# front.r = dat$orgnr[dat$r_TOTXDEA >= 15000 & dat$aar == faktisk.aar]
# # De som skal evalueres, men ikke kan være på fronten:
# sep.eval.r = setdiff(eval.r,front.r)

# Roar metoden
  # D-nett
dat$d_tilDEA = 0
dat$d_tilDEA <- ifelse(dat$d_TOTXDEA > 0, dat$d_tilDEA <- 1, dat$d_tilDEA <- 0)

for (i in which(dat$id %in% d_dea_til_gjsnitt)){
        dat[i,"d_tilDEA"]  = 0 
}

for (i in which(dat$id %in% d_spesial)){  
        dat[i,"d_tilDEA"]  = 0   
} 

for (i in which(dat$id %in% d_ikkeIR)){  
        dat[i,"d_tilDEA"]  = 0   
} 

d_tilDEA <- dat[dat$d_tilDEA==1,] 

  # R-nett
dat$r_tilDEA = 0  
dat$r_tilDEA <- ifelse(dat$r_TOTXDEA > 0, dat$r_tilDEA <- 1, dat$r_tilDEA <- 0) 

for (i in which(dat$id %in% r_dea_til_gjsnitt)){  
        dat[i,"r_tilDEA"]  = 0  
}

for (i in which(dat$id %in% r_spesial)){  
        dat[i,"r_tilDEA"]  = 0   
}  

r_tilDEA <- dat[dat$r_tilDEA == 1,]


#### Velger data for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres  
  # D-nett  
x.sf.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_TOTXDEA")]  
y.sf.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_ab","sf_d_hs","sf_d_ns")]  
names(x.sf.d) = dat #Fungerer ikke  
rownames(y.sf.d) = dat  

  # R-nett  
x.sf.r = dat[dat$orgnr %in% r_tilDEA & dat$aar == faktisk.aar,"sf_r_TOTXDEA"]  
y.sf.r = dat[dat$orgnr %in% r_tilDEA & dat$aar == faktisk.aar,c("sf_r_vluft","sf_r_vjord","sf_r_vsjo","sf_r_vgrs")]  
names(x.sf.r) = dat  
rownames(y.sf.r) = dat 

# Faktiske data for selskaper som skal evalueres
x.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"r_TOTXDEA"]
y.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]
z.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
kap.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_AKG")]
names(x.faktisk.r) = eval.r
rownames(y.faktisk.r) = eval.r
rownames(z.faktisk.r) = eval.r
names(kap.faktisk.r) = eval.r


#### Beregner snitt for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres
x.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"av_r_TOTXDEA"]
y.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("av_r_vluft","av_r_vjord","av_r_vsjo","av_r_vgrs")]
z.snitt.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
names(x.snitt.r) = eval.r
rownames(y.snitt.r) = eval.r
rownames(z.snitt.r) = eval.r


#### Trinn 1 - DEA-kjøringer ####

# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.r og y.snitt.r som tilvhører selskapene i front.r
res.tmp1 = dea(X=x.snitt.r,Y=y.snitt.r,XREF=x.snitt.r[as.character(front.r)],YREF=y.snitt.r[as.character(front.r),],RTS="crs")
#plot(sort(res.snitt.snitt.r$eff))

# Spesialkjøring for selskaper som bare kan være front for seg selv
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


#### Export til Excel trinn 1 ####

# Gir manglende observasjoner en verdi lik null
dat[is.na(dat)] <- 0

# OBSOBS: sjekk at datarammene inneholder riktige variabler når alle variabler er laget

# Konstruerer datarammer til Excel-arkene for D-nett trinn 1
d_grunnlagsdata_trinn1 = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_dv, dat$d_391, 
                                    dat$d_utred, dat$d_DV, dat$d_AKG, dat$d_abakg, dat$d_akg, dat$d_avs, 
                                    dat$d_abavs, dat$d_avs, dat$d_kile, dat$d_nettap, 
                                    dat$d_nettapkr, dat$d_grs.cost, dat$d_TOTXDEA, dat$d_ab, 
                                    dat$d_hs, dat$d_ns)

d_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_DVxL, dat$d_lonn, 
                         dat$d_lonnakt, dat$d_pensj,  dat$fp_d_pensj, 
                         dat$d_pensjek, dat$d_impl, dat$fp_d_impl, dat$av_fp_d_pensj, 
                         dat$fp_d_pensjek, dat$av_fp_d_pensjek, 
                         dat$av_fp_d_impl, dat$d_pensjkostgrlag, dat$d_dv, dat$d_dv_2012)

d_gjsnittfront = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$fp_d_dv, dat$fp_d_391, 
                            dat$fp_d_DV, dat$fp_d_kile, dat$av_d_TOTXDEA, dat$av_d_ab, 
                            dat$av_d_hs, dat$av_d_ns) 
#                           dat$d_snittfront*)
# 
# d_DEAdata = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$frontlov, 
#                        dat$d_snittfront_d_TOTXDEA, dat$d_snittfront_d_ab, dat$d_snittfront_d_hs, 
#                        d_snittfront_d_ns)
# 
# d_vektberegning = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_frontlov_hoved, 
#                              dat$frontlov, dat$d_vekt*, dat$d_normkostandel*, dat$d_kostbidrag*)
# 
# d_DEAresultater = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_frontlov_hoved, 
#                              dat$d_score_snittfront, dat$d_score_spesial, dat$d_dea_til2trinn)

d_grunnlagsdata_trinn1 = d_grunnlagsdata_trinn1[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
d_forslagDV = d_forslagDV[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
d_gjsnittfront = d_gjsnittfront[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_DEAdata = d_DEAdata[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_vektberegning = d_vektberegning[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_DEAresultater = d_DEAresultater[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]

# Konstruerer datarammer til Excel-arkene for R-nett trinn 1
r_grunnlagsdata_trinn1 = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_akg, 
                                    dat$r_abakg, dat$r_AKG, dat$r_avs, dat$r_abavs, dat$r_AVS, 
                                    dat$r_kile, dat$r_TOTXDEA, dat$r_vluft, dat$r_vjord,
                                    dat$r_vsjo, dat$r_vgrs)

r_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_DVxL, dat$r_lonn, 
                         dat$r_lonnakt, dat$r_pensj, dat$fp_r_pensj, dat$av_fp_r_pensj, 
                         dat$r_pensjek, dat$fp_r_pensjek, dat$av_fp_r_pensjek, dat$r_impl, 
                         dat$fp_r_impl, dat$av_fp_r_impl, dat$r_pensjkostgrlag, dat$r_dv, 
                         dat$r_dv_2012)

r_gjsnittfront = data.frame(dat$idaar, dat$idaar, dat$selskap, dat$fp_r_dv, dat$fp_r_391, 
                            dat$fp_r_utred, dat$fp_r_DV, dat$fp_r_kile, dat$av_r_vsjo, 
                            dat$av_r_vluft, dat$av_r_vjord, dat$av_r_vgrs, dat$av_r_TOTXDEA) 
#                           dat$r_snittfront*)

# r_DEAdata = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$frontlov, dat$r_snittfront*)
# 
# r_vektberegning = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_frontlov_hoved, 
#                              dat$frontlov, dat$r_vekt*, dat$r_normkostandel*, dat$r_kostbidrag*)
# 
# r_DEAresultater = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_frontlov_hoved, 
#                              dat$r_score_snittfront, dat$r_score_spesial, dat$r_dea_til2trinn)
# 
# s_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$s_DVxL, dat$s_lonn, 
#                          dat$s_lonnakt, dat$s_pensj, dat$fp_s_pensj_faktisk, dat$fp_s_pensj, 
#                          dat$av_fp_s_pensj, dat$s_pensjek, dat$fp_s_pensjek, 
#                          dat$fp_s_pensjek_faktisk, dat$av_fp_s_pensjek, dat$s_impl, 
#                          dat$fp_s_impl_faktisk, dat$fp_s_impl, dat$av_fp_s_impl, 
#                          dat$s_pensjkostgrlag, dat$s_dv, dat$s_dv_2012)

r_grunnlagsdata_trinn1 = r_grunnlagsdata_trinn1[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
r_forslagDV = r_forslagDV[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
r_gjsnittfront = r_gjsnittfront[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_DEAdata = r_DEAdata[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_vektberegning = r_vektberegning[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_DEAresultater = r_DEAresultater[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]


# Lager Excel-filer
DataDEA1D = createWorkbook(type="xlsx")
side1D = createSheet(wb = DataDEA1D, sheetName = "d_grunnlagsdata_trinn1")
side2D = createSheet(wb = DataDEA1D, sheetName = "d_forslagDV")
side3D = createSheet(wb = DataDEA1D, sheetName = "d_gjsnittfront")
# side4D = createSheet(wb = Data og DEA 1D, sheetName = "d_dataDEA")
# side5D = createSheet(wb = Data og DEA 1D, sheetName = "d_vektberegning")
# side6D = createSheet(wb = Data og DEA 1D, sheetName = "d_DEAresultater")
addDataFrame(x = d_grunnlagsdata_trinn1, sheet = side1D)
addDataFrame(x = d_forslagDV, sheet = side2D)
addDataFrame(x = d_gjsnittfront, sheet = side3D)
# addDataFrame(x = d_DEAdata, sheet = side4D)
# addDataFrame(x = d_vektberegning, sheet = side5D)
# addDataFrame(x = d_DEAresultater, sheet = side6D)
saveWorkbook(DataDEA1D, "Data og DEAresultater trinn 1 Distribusjonsnett.xlsx")
# 
DataDEA1R = createWorkbook()
side1R = createSheet(wb = DataDEA1R, sheetName = "r_grunnlagsdata_trinn1")
side2R = createSheet(wb = DataDEA1R, sheetName = "r_forslagDV")
side3R = createSheet(wb = DataDEA1R, sheetName = "r_gjsnittfront")
# side4R = createSheet(wb = DataDEA1R, sheetName = "r_dataDEA")
# side5R = createSheet(wb = DataDEA1R, sheetName = "r_vektberegning")
# side6R = createSheet(wb = DataDEA1R, sheetName = "r_DEAresultater")
addDataFrame(x = r_grunnlagsdata_trinn1, sheet = side1R)
addDataFrame(x = r_forslagDV, sheet = side2R)
addDataFrame(x = r_gjsnittfront, sheet = side3R)
# addDataFrame(x = r_DEAdata, sheet = side4R)
# addDataFrame(x = r_vektberegning, sheet = side5R)
# addDataFrame(x = r_DEAresultater, sheet = side6R)
saveWorkbook(DataDEA1R, "Data og DEAresultater trinn 1 Regionalnett.xlsx")
# 
# DataS = createWorkbook()
# side1S = createSheet(wb=Data S, sheetName = "s_forslagDV")
# addDataFrame(x = s_forslagDV, sheet = side1S)
# saveWorkbook(DataS, "Data Sentralnett")

#Runder av data til DEA til "hele tusen"  
x.sf.d$sf_d_TOTXDEA <- round(x.sf.d$sf_d_TOTXDEA, digits = 0)  
y.sf.d$sf_d_ab  <- round(y.sf.d$sf_d_ab, digits = 0)  
y.sf.d$sf_d_ns <- round(y.sf.d$sf_d_ns, digits = 0)  
y.sf.d$sf_d_hs <- round(y.sf.d$sf_d_hs, digits = 0) 


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

