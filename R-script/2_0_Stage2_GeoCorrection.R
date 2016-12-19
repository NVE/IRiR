# Stage 2 - Efficiency correction of efficeincy for environmental effects

#D-nett ------------------------------------------------------------------------
# Estimerer Fjellbekk
Geo1.comp = cbind(d_tilDEA[,c("dr_he1", "dr_s7", "dr_skysz")])
d_tilDEA$dr_Geo1 = z.est(geovar.in = Geo1.comp, restricted.obs = Geo1.comp)
rm(Geo1.comp)

# Estimerer Øyvind
Geo2.comp = cbind(d_tilDEA[,c("dr_vr2_k2lukk", "dr_aoey1sz", "dr_hssjoand")])
d_tilDEA$dr_Geo2 = z.est(geovar.in = Geo2.comp, restricted.obs = Geo2.comp)*-1
rm(Geo2.comp)

#Estimerer Frost
d_tilDEA$dr_tempneg = d_tilDEA$dr_temp*-1
#Setter alle verdier i snittbreddegrad under 65,9 til 65,9
d_tilDEA$dr_brgrad_gjsn = pmax(d_tilDEA$dr_brgrad_gjsn, 65.9)
Geo3.comp =cbind(d_tilDEA[,c("dr_snog", "dr_brgrad_gjsn", "dr_is_gjsn", "dr_tempneg")])
d_tilDEA$dr_Geo3 = z.est(geovar.in = Geo3.comp, restricted.obs = Geo3.comp)
rm(Geo3.comp)

#Beregner Geo-koeffisen1ter for D-nett
Geovar.d = cbind(d_tilDEA[,c("dr_hsjordand","dr_s4", "dr_Geo1", "dr_Geo2", "dr_Geo3")])
d.coeff = rvk1(x=x.snitt.d,z=Geovar.d,d_tilDEA$d_bs_correst_e3,
               lambda = d_lambda.snitt,
               id = names(x.snitt.d),
               id.ut = as.character(d_separat_dmuer))$coeff
d.coeff
#Utfører faktisk rammevilkårsjustering for D-nett
rvk2(x = x.snitt.d, eff = eff.faktisk.snitt.d, id = names(x.snitt.d),
     lambda = d_lambda, coeff = d.coeff, z = Geovar.d)

#R-nett -----------------------------------------------------------------------

GeoR.comp = cbind(r_tilDEA[,c("rr_s12", "rr_he")])
row.names(GeoR.comp) = names(x.snitt.r)
GeoR.tech = GeoR.comp[as.character(r_normal),]
#Estimerer Helskog
r_tilDEA$rr_Geo1 = z.est(geovar.in = GeoR.tech, restricted.obs = GeoR.comp)
