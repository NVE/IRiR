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
# See functions_nve.R for further details.
ld_EVAL$ld_eff.s2.cb = Zvar2(x = X.avg.ld, eff = eff.cb.avg.ld, id = names(X.avg.ld),
                                lambda = ld_lambda, coeff = ldz.coeff, z = Geovar.ldz)$eff.corr

#Estimate Z-variables for Regional distribution ---------------------------

GeoR.comp = cbind(rd_EVAL[,c("rdz_f12", "rdz_inc.av")])
row.names(GeoR.comp) = names(X.avg.rd)
GeoR.tech = GeoR.comp[as.character(rd_eval),]
#Estimates Helskog
rd_EVAL$rr_Geo1 = z.est(geovar.in = GeoR.tech, restricted.obs = GeoR.comp)
Geovar.r = cbind(rd_EVAL[,c("rr_Geo1")])
#Fjerner selskaper med NA-verdier i dr-bs
r_eff.bs = rd_EVAL$r_score_bs100
names(r_eff.bs) = names(X.avg.rd)
r_eff.bs = r_eff.bs[!is.na(r_eff.bs)]

r.coeff = Zvar1(x=X.avg.rd,z=Geovar.r,eff=r_eff.bs,
               lambda = r_lambda.snitt,
               id = names(X.avg.rd),
               id.out = as.character(r_separat_dmuer))$coeff


names(r.coeff)[2] = "rr_Geo1"
r.coeff
#Utfører rammevilkårskorrigering for D-nett
rd_EVAL$r_deares_tilkal = Zvar2(x = X.avg.rd, eff=eff.faktisk.snitt.r, id=names(X.avg.rd),
        lambda = r_lambda, coeff=r.coeff, z=Geovar.r)$eff.corr