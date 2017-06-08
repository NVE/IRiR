#### Import Bootstrap data from Frisch DEA - TEMP

#### Local Distribution data-import ####
#Load bootstrap corrected efficency scores from Frisch DEA
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

#Calculate z-var imputs ( Improve - move to 2_0 ? ) 
ld_EVAL$ld_hvol = ld_EVAL$ld_hv - ld_EVAL$ld_hvug - ld_EVAL$ld_hvsc
# ld_EVAL$ld_hvsc = ld_EVAL$d_hssjou6kv + ld_EVAL$d_hssjoo6kv
ld_EVAL$ld_hv_uso = ld_EVAL$ld_hvug + ld_EVAL$ld_hvsc + ld_EVAL$ld_hvol 
ld_EVAL$ldz_hvug.s = ld_EVAL$ld_hvug / ld_EVAL$ld_hv_uso # ( Improve - Replace ld_hv_uso with ld_hv? )
ld_EVAL$ldz_hvsc.s = ld_EVAL$ld_hvsc / ld_EVAL$ld_hv_uso
ld_EVAL$ldz_hvc.s = (ld_EVAL$ld_hvug + ld_EVAL$ld_hvsc) / ld_EVAL$ld_hv_uso


ld_EVAL$ldz_isl.sz = ld_EVAL$ldz_isl / ld_EVAL$ld_scale

ld_EVAL$ldz_cmpp.sz = ld_EVAL$ldz_cmpp / ld_EVAL$ld_scale

ld_EVAL$ldz_wind2 = ld_EVAL$ldz_wind * ld_EVAL$ldz_wind
ld_EVAL$ldz_wind2_cod = ld_EVAL$ldz_wind2 / ld_EVAL$ldz_cod2c

#### Regional Distribution Data Import  ####
#Load bootstrap corrected efficency scores from Frisch DEA
rd_bs = read.csv("./Data/Bootstrap/r_bs_031215.csv",sep=",")

#Merger disse estimerte DEA-resultatene med selskapene i rd_EVAL

rd_EVAL = merge.data.frame(rd_EVAL, rd_bs, by="id.y", all.x = T)


missing.rd.bs <- dat[is.na(rd_EVAL$correst),]

#Får du følgende melding, "<0 rows> (or 0-length row.names)", har alle selskap fått
#importert Bootstrap-estimat for snitt mot snitt beregnet escore
missing.rd.bs[c("selskap", "orgnr")]
#Legg inn logisk sjekk på at den er tom

rm(missing.rd.bs)

colnames(rd_EVAL)[colnames(rd_EVAL)=="estimate"] <- "rd_bs.est"
colnames(rd_EVAL)[colnames(rd_EVAL)=="correst"] <- "rd_bs.correst"

rd_EVAL$rd_eff.bs.is2.cZ = rd_EVAL$rd_bs.correst

#Geovariabel i R-nettet
#rr_s12, skogvariabel
#rr_he, gjsn.helning
#rr_antall_ruter, antall ruter med Rnett