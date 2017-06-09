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

#Estimates Frost
ld_EVAL$ldz_tempneg = ld_EVAL$ldz_temp*-1
#Setter alle verdier i snittbreddegrad under 65,9 til 65,9
ld_EVAL$ldz_lat.av = pmax(ld_EVAL$ldz_lat.av, 65.9)
Geo3.comp =cbind(ld_EVAL[,c("ldz_snow", "ldz_lat.av", "ldz_ice.av", "ldz_tempneg")])
ld_EVAL$dr_Geo3 = z.est(geovar.in = Geo3.comp, restricted.obs = Geo3.comp)
rm(Geo3.comp)


#effektivitetsscores må være en vektor med navn inn i funksjonen
d_eff.bs = ld_EVAL$d_score_bs100
names(d_eff.bs) = names(x.snitt.d)


#Beregner Geo-koeffisen1ter for D-nett
Geovar.d = cbind(ld_EVAL[,c("dr_hsjordand","dr_s4", "ldz_Geo1", "ldz_Geo2", "dr_Geo3")])
d.coeff = rvk1(x=x.snitt.d,z=Geovar.d,eff=d_eff.bs,
               lambda = d_lambda.snitt,
               id = names(x.snitt.d),
               id.ut = as.character(d_separat_dmuer))$coeff
d.coeff
#Utfører faktisk rammevilkårsjustering for D-nett
ld_EVAL$d_deares_til_kal = rvk2(x = x.snitt.d, eff = eff.faktisk.snitt.d, id = names(x.snitt.d),
                                lambda = d_lambda, coeff = d.coeff, z = Geovar.d)$eff.corr

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

r.coeff = rvk1(x=x.snitt.r,z=Geovar.r,eff=r_eff.bs,
               lambda = r_lambda.snitt,
               id = names(x.snitt.r),
               id.ut = as.character(r_separat_dmuer))$coeff


names(r.coeff)[2] = "rr_Geo1"
r.coeff
#Utfører rammevilkårskorrigering for D-nett
r_tilDEA$r_deares_tilkal = rvk2(x = x.snitt.r, eff=eff.faktisk.snitt.r, id=names(x.snitt.r),
        lambda = r_lambda, coeff=r.coeff, z=Geovar.r)$eff.corr