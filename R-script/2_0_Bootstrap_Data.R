#2_0 Importerer Bootstrap Data og genererer RVK-variabler
#Laster inn bootstrap resultater fra ekstern app 
d_bs = read.csv("./Data/Bootstrap/d_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i d_tilDEA

d_tilDEA = merge.data.frame(d_tilDEA, d_bs, by="idaar", all.x = T)


manglende.bs <- dat[is.na(d_tilDEA$correst),]
#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
manglende.bs[c("selskap", "orgnr")] 

rm(d_bs)

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
write.csv(d_tilDEA, file="tmp1.csv")
