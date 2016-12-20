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

#effektivitetsscores må være en vektor med navn inn i funksjonen
d_eff.bs = d_tilDEA$d_score_bs100
names(d_eff.bs) = names(x.snitt.d)


#Beregner Geo-koeffisen1ter for D-nett
Geovar.d = cbind(d_tilDEA[,c("dr_hsjordand","dr_s4", "dr_Geo1", "dr_Geo2", "dr_Geo3")])
d.coeff = rvk1(x=x.snitt.d,z=Geovar.d,eff=d_eff.bs,
               lambda = d_lambda.snitt,
               id = names(x.snitt.d),
               id.ut = as.character(d_separat_dmuer))$coeff
d.coeff
#Utfører faktisk rammevilkårsjustering for D-nett
d_tilDEA$d_deares_til_kal = rvk2(x = x.snitt.d, eff = eff.faktisk.snitt.d, id = names(x.snitt.d),
     lambda = d_lambda, coeff = d.coeff, z = Geovar.d)$eff.corr

#R-nett -----------------------------------------------------------------------

GeoR.comp = cbind(r_tilDEA[,c("rr_s12", "rr_he")])
row.names(GeoR.comp) = names(x.snitt.r)
GeoR.tech = GeoR.comp[as.character(r_normal),]
#Estimerer Helskog
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
