#### Import Bootstrap data from Frisch DEA - TEMP ####

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

#### Regional Distribution - Bootstrap ####

rd_bs = as.data.frame(dea.boot(X =as.matrix(X.avg.rd[as.character(rd_eval)]), Y=as.matrix(Y.avg.rd[as.character(rd_eval),]),
                       NREP = 2000, RTS="crs", ORIENTATION = "in", alpha=0.1)$eff.bc)
names(rd_bs) = "rd_bs.correst"
rd_bs$id = add_rownames(rd_bs,"id")

rd_EVAL = merge.data.frame(rd_EVAL, rd_bs, by="id.y", all.x = T)


rd_EVAL$rd_eff.bs.is2.cZ = rd_EVAL$rd_bs.correst

#Geovariabel i R-nettet
#rr_s12, skogvariabel
#rr_he, gjsn.helning
#rr_antall_ruter, antall ruter med Rnett