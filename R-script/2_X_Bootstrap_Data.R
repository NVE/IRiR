#### Import Bootstrap data

#### D-nett  ####
#Laster inn bootstrap resultater fra ekstern app 
d_bs = read.csv("./Data/Bootstrap/d_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i d_tilDEA

d_tilDEA = merge.data.frame(d_tilDEA, d_bs, by="idaar", all.x = T)


# manglende.bs <- dat[is.na(d_tilDEA$correst),]
#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
#manglende.bs[c("selskap", "orgnr")] 
#Legg inn logisk sjekk på at den er tom
#rm(manglende.bs)
#Legg inn logisk sjekk på at den er tom

#endrer navn på variablene importert fra bootstrap
#"e3"-indikerer hvilke forutsetninger som er valgt i Fritsch

colnames(d_tilDEA)[colnames(d_tilDEA)=="estimate"] <- "d_bs_est_e3"
colnames(d_tilDEA)[colnames(d_tilDEA)=="correst"] <- "d_bs_correst_e3"

d_tilDEA$d_score_bs100 = d_tilDEA$d_bs_correst_e3

d_tilDEA[is.na(d_tilDEA$d_skytelse)] = 0
#Litt usikker på om dette er ok.


#Lager ny variabel lik som småkraftytelse ihht Roars script
d_tilDEA$dr_sky = d_tilDEA$d_skytelse


d_tilDEA$d_dea_til2trinn = d_tilDEA$d_f_sf_eff

d_tilDEA$d_knorm = d_tilDEA$d_TOTXDEA * d_tilDEA$d_dea_til2trinn


d_tilDEA$d_skala = d_tilDEA$d_knorm #Bruker norm som skaleringsfaktor

#Beregner rammevilkårsvariabler
d_tilDEA$d_hsluft = d_tilDEA$d_hs - d_tilDEA$d_hsjord - d_tilDEA$d_hssjo
# d_tilDEA$d_hssjo = d_tilDEA$d_hssjou6kv + d_tilDEA$d_hssjoo6kv
d_tilDEA$d_hs_jsl = d_tilDEA$d_hsjord + d_tilDEA$d_hssjo + d_tilDEA$d_hsluft # Disse variablene er HS.
d_tilDEA$dr_hsjordand = d_tilDEA$d_hsjord / d_tilDEA$d_hs_jsl
d_tilDEA$dr_hssjoand = d_tilDEA$d_hssjo / d_tilDEA$d_hs_jsl
d_tilDEA$dr_hskabelandel = (d_tilDEA$d_hsjord + d_tilDEA$d_hssjo) / d_tilDEA$d_hs_jsl


d_tilDEA$dr_aoey1sz = d_tilDEA$d_aoey1 / d_tilDEA$d_skala
colnames(d_tilDEA)[colnames(d_tilDEA)=="d_aoey1"] <- "dr_aoey1"

d_tilDEA$dr_skysz = d_tilDEA$d_skytelse / d_tilDEA$d_skala

d_tilDEA$dr_vr2 = d_tilDEA$dr_vr * d_tilDEA$dr_vr
d_tilDEA$dr_vr2_k2lukk = d_tilDEA$dr_vr2 / d_tilDEA$dr_k2lukk

#### R-nett  ####


#Laster inn bootstrap resultater fra ekstern app 
r_bs = read.csv("./Data/Bootstrap/r_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i r_tilDEA

r_tilDEA = merge.data.frame(r_tilDEA, r_bs, by="idaar", all.x = T)


manglende.r.bs <- dat[is.na(r_tilDEA$correst),]

#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
manglende.r.bs[c("selskap", "orgnr")]
#Legg inn logisk sjekk på at den er tom

rm(manglende.r.bs)

#endrer navn på variablene importert fra bootstrap
#"e3"-indikerer hvilke forutsetninger som er valgt i Fritsch

colnames(r_tilDEA)[colnames(r_tilDEA)=="estimate"] <- "r_bs_est_e3"
colnames(r_tilDEA)[colnames(r_tilDEA)=="correst"] <- "r_bs_correst_e3"

r_tilDEA$r_score_bs100 = r_tilDEA$r_bs_correst_e3

#Geovariabel i R-nettet
#rr_s12, skogvariabel
#rr_he, gjsn.helning
#rr_antall_ruter, antall ruter med Rnett