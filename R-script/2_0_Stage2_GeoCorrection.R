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
ld_eff.bs = ld_EVAL$lld_eff.bs.is2.cZ
names(ld_eff.bs) = names(X.avg.ld)

#Calculates Geo coefficients (Z variable coefficients) for Local distribution
Geovar.ldz = cbind(ld_EVAL[,c("ldz_hvug.s","ldz_f4", "ldz_Geo1", "ldz_Geo2", "ldz_Geo3")])
ldz.coeff = Zvar1(x=X.avg.ld,z=Geovar.ldz,eff=ld_eff.bs,
               lambda = ld_lambda.avg,
               id = names(X.avg.ld),
               id.out = as.character(ld_sep.eval))$coeff
ldz.coeff
#Utfører faktisk rammevilkårsjustering for D-nett
ld_EVAL$d_deares_til_kal = rvk2(x = X.avg.ld, eff = eff.faktisk.snitt.d, id = names(X.avg.ld),
                                lambda = d_lambda, coeff = ldz.coeff, z = Geovar.ldz)$eff.corr

#R-nett -----------------------------------------------------------------------

GeoR.comp = cbind(r_tilDEA[,c("rr_s12", "rr_he")])
row.names(GeoR.comp) = names(x.snitt.r)
GeoR.tech = GeoR.comp[as.character(r_normal),]
#Estimates Helskog
r_tilDEA$rr_Geo1 = z.est(geovar.in = GeoR.tech, restricted.obs = GeoR.comp)
Geovar.r = cbind(r_tilDEA[,c("rr_Geo1")])
#Fjerner selskaper med NA-verdier i dr-bs
r_eff.bs = r_tilDEA$r_score_bs100
names(r_eff.bs) = names(x.snitt.r)
r_eff.bs = r_eff.bs[!is.na(r_eff.bs)]

r.coeff = Zvar1(x=x.snitt.r,z=Geovar.r,eff=r_eff.bs,
               lambda = r_lambda.snitt,
               id = names(x.snitt.r),
               id.out = as.character(r_separat_dmuer))$coeff


names(r.coeff)[2] = "rr_Geo1"
r.coeff
#Utfører rammevilkårskorrigering for D-nett
r_tilDEA$r_deares_tilkal = rvk2(x = x.snitt.r, eff=eff.faktisk.snitt.r, id=names(x.snitt.r),
        lambda = r_lambda, coeff=r.coeff, z=Geovar.r)$eff.corr