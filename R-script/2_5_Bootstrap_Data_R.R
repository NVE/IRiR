#2_5 Importerer Bootstrap Data og genererer RVK-variabler
#Laster inn bootstrap resultater fra ekstern app 
r_bs = read.csv("./Data/Bootstrap/r_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i r_tilDEA

r_tilDEA = merge.data.frame(r_tilDEA, r_bs, by="idaar", all.x = T)


manglende.bs <- dat[is.na(r_tilDEA$correst),]
#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
manglende.bs[c("selskap", "orgnr")]
#Legg inn logisk sjekk på at den er tom
rm(manglende.bs)

#endrer navn på variablene importert fra bootstrap
#"e3"-indikerer hvilke forutsetninger som er valgt i Fritsch

colnames(r_tilDEA)[colnames(r_tilDEA)=="estimate"] <- "r_bs_est_e3"
colnames(r_tilDEA)[colnames(r_tilDEA)=="correst"] <- "r_bs_correst_e3"

r_tilDEA$r_score_bs100 = r_tilDEA$r_bs_correst_e3

#Geovariabel i R-nettet
#rr_s12, skogvariabel
#rr_he, gjsn.helning
#rr_antall_ruter, antall ruter med Rnett




write.csv(r_tilDEA, file="tmp1.csv")