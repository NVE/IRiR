#### 2.0 Adjusts efficency scores for environmental factors - Stage 2 ####

#### Local Distribution - Bootstrap####
if(BS.new == 1) {
  ld_bs = as.data.frame(dea.boot(X=as.matrix(X.avg.ld[as.character(ld_eval)]), Y=as.matrix(Y.avg.ld[as.character(ld_eval),]),
                               NREP = BS.ite, RTS="crs", ORIENTATION = "in", alpha=0.1)$eff.bc)
} else {
  ld_bs = read.csv("./Data/Bootstrap/ld_bs.csv",sep=",")
}


if(BS.new == 1 ) {
  names(ld_bs) = "ld_bs.correst"
  ld_bs = add_rownames(ld_bs, "id")
  write.csv(ld_bs, "./Data/Bootstrap/ld_bs.csv", row.names = FALSE)
}

#Merging bootstrap corrected scores into ld_EVAL

ld_EVAL = merge.data.frame(ld_EVAL, ld_bs, by="id", all.x = T)

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
if(BS.new == 1) {
  rd_bs = as.data.frame(dea.boot(X=as.matrix(X.avg.rd[as.character(rd_eval)]), Y=as.matrix(Y.avg.rd[as.character(rd_eval),]),
                                 NREP = BS.ite, RTS="crs", ORIENTATION = "in", alpha=0.1)$eff.bc)
} else {
  rd_bs = read.csv("./Data/Bootstrap/rd_bs.csv",sep=",")
}


if(BS.new == 1 ) {
  names(rd_bs) = "rd_bs.correst"
  rd_bs = add_rownames(rd_bs, "id")
  write.csv(rd_bs, "./Data/Bootstrap/rd_bs.csv", row.names = FALSE)
}

rd_EVAL = merge.data.frame(rd_EVAL, rd_bs, by="id", all.x = T)


rd_EVAL$rd_eff.bs.is2.cZ = rd_EVAL$rd_bs.correst

#Geovariabel i R-nettet
#rr_f12, Forest variable
#rr_he, Average inclination
#rr_antall_ruter, number of mapgrid squares with rd grid



# Estimate Z-variables for Local distribution ---------------------------
# Estimates "Fjellbekk" - variable adjusting for mountain environments
Geo1.comp = cbind(ld_EVAL[,c("ldz_inc.av", "ldz_f7", "ldz_cmpp.sz")])
ld_EVAL$ldz_Geo1 = z.est(geovar.in = Geo1.comp, restricted.obs = Geo1.comp)
rm(Geo1.comp)

# Estimates "Øyvind" - variable adjusting for coastal environments
Geo2.comp = cbind(ld_EVAL[,c("ldz_wind2_cod", "ldz_isl.sz", "ldz_hvsc.s")])
ld_EVAL$ldz_Geo2 = z.est(geovar.in = Geo2.comp, restricted.obs = Geo2.comp)*-1
rm(Geo2.comp)

#Estimates Frost - variable adjusting for very cold environments (and annoying Disney songs?)
ld_EVAL$ldz_tempneg = ld_EVAL$ldz_temp*-1
#Replace all average latitude observations below 65.9 with 65.9
ld_EVAL$ldz_lat.av = pmax(ld_EVAL$ldz_lat.av, 65.9)
Geo3.comp =cbind(ld_EVAL[,c("ldz_snow", "ldz_lat.av", "ldz_ice.av", "ldz_tempneg")])
ld_EVAL$ldz_Geo3 = z.est(geovar.in = Geo3.comp, restricted.obs = Geo3.comp)
rm(Geo3.comp)


# Bootstraped efficiency scores needs to be named vector in the Zvar1-function to
# ensure that correct companies are included in regression. Companies in ld_sep.eval-group
# and companies identified as outliers by BACON-test (see functions) are omitted from regression
ld_eff.bs = ld_EVAL$ld_eff.bs.is2.cZ
names(ld_eff.bs) = names(X.avg.ld)

#Calculates Geo coefficients (Z variable coefficients) for Local distribution
Geovar.ldz = cbind(ld_EVAL[,c("ldz_hvug.s","ldz_f4", "ldz_Geo1", "ldz_Geo2", "ldz_Geo3")])
ldz.coeff = Zvar1(x=X.avg.ld,z=Geovar.ldz,eff=ld_eff.bs,
                  lambda = ld_lambda.avg,
                  id = names(X.avg.ld),
                  id.out = as.character(ld_sep.eval))$coeff
ldz.coeff


#### Z variable adjustment - Local distribution #### 
# Adjusts efficiency scores from stage 1, using difference in Z value relative to target unit
# See functions_nve.R for further details. Local distribution grid
ld_EVAL$ld_eff.s2.cb = Zvar2(x = X.avg.ld, eff = eff.cb.avg.ld, id = names(X.avg.ld),
                             lambda = ld_lambda, coeff = ldz.coeff, z = Geovar.ldz)$eff.corr

#Estimate Z-variables for Regional distribution ---------------------------

GeoR.comp = cbind(rd_EVAL[,c("rdz_f12", "rdz_inc.av")])
row.names(GeoR.comp) = names(X.avg.rd)
GeoR.tech = GeoR.comp[as.character(rd_eval),]
#Estimates Helskog
rd_EVAL$rdz_Geo1 = z.est(geovar.in = GeoR.tech, restricted.obs = GeoR.comp)
Geovar.r = cbind(rd_EVAL[,c("rdz_Geo1")])
#Remove companies not included in bootstrap for regional distribution grid
rd_eff.bs = rd_EVAL$rd_eff.bs.is2.cZ
names(rd_eff.bs) = names(X.avg.rd)
rd_eff.bs = rd_eff.bs[!is.na(rd_eff.bs)]

rdz.coeff = Zvar1(x=X.avg.rd,z=Geovar.r,eff=rd_eff.bs,
                  lambda = rd_lambda.avg,
                  id = names(X.avg.rd),
                  id.out = as.character(rd_sep.eval))$coeff


names(rdz.coeff)[2] = "rdz_Geo1"
rdz.coeff
#### Z variable adjustment - Regional distribution #### 
#Adjusts efficiency scores from stage 1, using difference in Z value relative to target unit
# See functions_nve.R for further details. Regional distribution grid
rd_EVAL$rd_eff.s2.cb = Zvar2(x = X.avg.rd, eff=eff.cb.avg.rd, id=names(X.avg.rd),
                             lambda = rd_lambda, coeff=rdz.coeff, z=Geovar.r)$eff.corr

