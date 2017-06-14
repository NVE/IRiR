#### 2.0 Adjusts efficency scores for environmental factors - Stage 2 ####

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

#Adjusts efficiency scores from stage 1, using difference in Z value relative to target unit
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

#Adjusts efficiency scores from stage 1, using difference in Z value relative to target unit
# See functions_nve.R for further details. Regional distribution grid
rd_EVAL$rd_eff.s2.cb = Zvar2(x = X.avg.rd, eff=eff.cb.avg.rd, id=names(X.avg.rd),
                                lambda = rd_lambda, coeff=rdz.coeff, z=Geovar.r)$eff.corr
                                
                                
        lambda = r_lambda, coeff=rd.coeff, z=Geovar.r)$eff.corr