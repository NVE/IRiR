#### Oppsett, forutsetninger og dataimport ####

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
d_separat_dmuer <- (c())
d_ikkeIR <- (c(134, 348, 521, 612, 638, 696)) # IDene finnes ikke

# R/S-nett
r_spesial <- (c(10, 18, 35, 41, 88, 98, 106, 116, 135, 147, 156, 161, 162, 173,
                184, 187, 204, 222, 238, 274, 287, 307, 343, 349, 447, 484, 512, 549
                , 659, 686, 743)) # prøver, 10, her 
r_separat_dmuer <- (c(7, 9, 14, 37, 62, 63, 65, 93, 103, 138, 146, 152, 164, 
                      197, 206, 251, 257, 271, 275, 288, 295, 464, 591, 
                      625, 637, 669, 753))  # 14, 753 
r_dea_til_gjsnitt <- (c(183, 685, 542, 852, 900, 872))


# KPI-data
kpi = read.csv("./Data/Grunnlagsdata/KPIdata2016Varsel.csv", sep = ",")
# Legger til KPI-data for alle observasjoner i settet
dat = merge.data.frame(dat, kpi, by="aar", all.x = TRUE)

kpia2014 = 209.5
kpia2015 = 215.4
kpia2016 = 221.6
curr_aar_kpiafaktor = kpia2016/kpia2014 # Hardkodet til bruk i trinn 3

kpi2014 = 136.9 
kpi2015 = 139.7 
kpi2016 = 143.8
curr_aar_kpifaktor = kpi2016/kpi2014 # Hardkodet til bruk i trinn 3

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
nettapspris.ir = 0.20133 
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