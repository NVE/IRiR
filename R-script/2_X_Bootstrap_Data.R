#### Import Bootstrap data from Frisch DEA - TEMP

#### Local Distribution data-import ####
#Laster inn bootstrap resultater fra ekstern app 
ld_bs = read.csv("./Data/Bootstrap/ld_bs_031215.csv",sep=",")

#Merging bootstrap corrected scores into ld_EVAL

ld_EVAL = merge.data.frame(ld_EVAL, ld_bs, by="id.y", all.x = T)


colnames(ld_EVAL)[colnames(ld_EVAL)=="estimate"] <- "ld_bs.est"
colnames(ld_EVAL)[colnames(ld_EVAL)=="correst"] <- "ld_bs.correst"

ld_EVAL$ld_eff.bs.is2.cZ = ld_EVAL$ld_bs.correst

ld_EVAL[is.na(ld_EVAL$ldz_cmpp)] = 0


# Create new variable according to Stata script - Why? ( Improve )
ld_EVAL$ldz_cmppII = ld_EVAL$ldz_cmpp


ld_EVAL$ld_eff.bs.is2.aZ = ld_EVAL$ld_eff.s1.cb

ld_EVAL$ld_cnorm = ld_EVAL$ld_TOTXDEA * ld_EVAL$ld_eff.bs.is2.aZ


ld_EVAL$ld_scale = ld_EVAL$ld_cnorm #Bruker norm som skaleringsfaktor

#Beregner rammevilkårsvariabler
ld_EVAL$ld_hvol = ld_EVAL$ld_hv - ld_EVAL$ld_hvug - ld_EVAL$ld_hvsc
# ld_EVAL$ld_hvsc = ld_EVAL$d_hssjou6kv + ld_EVAL$d_hssjoo6kv
ld_EVAL$ld_hv_uso = ld_EVAL$ld_hvug + ld_EVAL$ld_hvsc + ld_EVAL$ld_hvol # Disse variablene er HS.
ld_EVAL$ldz_hvug.s = ld_EVAL$ld_hvug / ld_EVAL$ld_hv_uso # ( Improve - Replace ld_hv_uso with ld_hv? )
ld_EVAL$ldz_hvsc.s = ld_EVAL$ld_hvsc / ld_EVAL$ld_hv_uso
ld_EVAL$ldz_hvc.s = (ld_EVAL$ld_hvug + ld_EVAL$ld_hvsc) / ld_EVAL$ld_hv_uso


ld_EVAL$ldz_isl.sz = ld_EVAL$ldz_isl / ld_EVAL$ld_scale

ld_EVAL$ldz_cmpp.sz = ld_EVAL$ldz_cmpp / ld_EVAL$ld_scale

ld_EVAL$ldz_wind2 = ld_EVAL$ldz_wind * ld_EVAL$ldz_wind
ld_EVAL$ldz_wind2_cod = ld_EVAL$ldz_wind2 / ld_EVAL$ldz_cod2c

#### R-nett  ####


#Laster inn bootstrap resultater fra ekstern app 
r_bs = read.csv("./Data/Bootstrap/r_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i r_tilDEA

r_tilDEA = merge.data.frame(r_tilDEA, r_bs, by="id.y", all.x = T)


manglende.r.bs <- dat[is.na(r_tilDEA$correst),]

#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
manglende.r.bs[c("selskap", "orgnr")]
#Legg inn logisk sjekk på at den er tom

rm(manglende.r.bs)

#endrer navn på variablene importert fra bootstrap
#"e3"-indikerer hvilke forutsetningReplace ld_hv_uso with ld_hv? er som er valgt i Fritsch

colnames(r_tilDEA)[colnames(r_tilDEA)=="estimate"] <- "r_bs_est_e3"
colnames(r_tilDEA)[colnames(r_tilDEA)=="correst"] <- "r_bs_correst_e3"

r_tilDEA$r_score_bs100 = r_tilDEA$r_bs_correst_e3

#Geovariabel i R-nettet
#rr_s12, skogvariabel
#rr_he, gjsn.helning
#rr_antall_ruter, antall ruter med Rnett