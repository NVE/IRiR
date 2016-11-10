## Beregner Mønsterselskapets rammevilkår

#sf_ er fortsatt forkortelse for verdier for snittfront, dette gjelder ogs
#DFs som merkes med "snitt"

#Lager ny dataframe av kun normkostandeler for D-nett

d_normkostandeler = dplyr::select(d_vekter.faktisk, contains("d_normk"))
d_normkostandeler.snitt = dplyr::select(d_vekter.snitt, contains("sf_d_normk"))
#Velger observerte RVK for referentene.
##OBS! Her må vi finne en mer dynamisk måte å velge referentene på

#Lager dataframe med kun observasjoner for D-nett
d_ref.data = dplyr::filter(d_tilDEA, d_tilDEA$id %in% d_ref)

# Trekker ut referentenes verdier for jordkabel
d_ref.d_mon.hsjordand = d_ref.data$dr_hsjordand
#transponerer matrisen med normkostandeler
d_normkost.temp = t(d_normkostandeler)
d_normkost.snitt.temp = t(d_normkostandeler.snitt)
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
d_mon_dr_hsjordand = colSums(d_ref.d_mon.hsjordand*d_normkost.temp)
d_mon_sf_dr_hsjordand = colSums(d_ref.d_mon.hsjordand*d_normkost.snitt.temp)
#legger verdiene i egen frame før de kobles på d_tilDEA
df.d_mon_dr_hsjordand = data.frame(d_mon_dr_hsjordand)
df.d_mon_sf_dr_hsjordand = data.frame(d_mon_sf_dr_hsjordand)
d_tilDEA = cbind.data.frame(d_tilDEA, df.d_mon_dr_hsjordand, df.d_mon_sf_dr_hsjordand)

rm(d_ref.d_mon.hsjordand, df.d_mon_dr_hsjordand, d_mon_dr_hsjordand, df.d_mon_sf_dr_hsjordand, d_mon_sf_dr_hsjordand)

#trekker ut referentes verdier for barskog
d_ref.d_mon.s4 = d_ref.data$dr_s4
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
d_mon_dr_s4 = colSums(d_ref.d_mon.s4*d_normkost.temp)
d_mon_sf_dr_s4 = colSums(d_ref.d_mon.s4*d_normkost.snitt.temp)
df.d_mon_dr_s4 = data.frame(d_mon_dr_s4)
df.d_mon_sf_dr_s4 = data.frame(d_mon_sf_dr_s4)
#legger verdiene i egen frame før de kobles på d_tilDEA
d_tilDEA = cbind.data.frame(d_tilDEA, df.d_mon_dr_s4, df.d_mon_sf_dr_s4)

rm(d_ref.d_mon.s4, df.d_mon_dr_s4, d_mon_dr_s4, df.d_mon_sf_dr_s4, d_mon_sf_dr_s4)

#trekker ut referentes verdier for Geo1
d_ref.d_mon.Geo1 = d_ref.data$dr_Geo1
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
d_mon_dr_Geo1 = colSums(d_ref.d_mon.Geo1*d_normkost.temp)
df.d_mon_dr_Geo1 = data.frame(d_mon_dr_Geo1)
d_mon_sf_dr_Geo1 = colSums(d_ref.d_mon.Geo1*d_normkost.snitt.temp)
df.d_mon_sf_dr_Geo1 = data.frame(d_mon_sf_dr_Geo1)
#legger verdiene i egen frame før de kobles på d_tilDEA
d_tilDEA = cbind.data.frame(d_tilDEA, df.d_mon_dr_Geo1, df.d_mon_sf_dr_Geo1)

rm(d_ref.d_mon.Geo1, df.d_mon_dr_Geo1, d_mon_dr_Geo1, df.d_mon_sf_dr_Geo1, d_mon_sf_dr_Geo1)

#trekker ut referentes verdier for Geo2
d_ref.d_mon.Geo2 = d_ref.data$dr_Geo2
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
d_mon_dr_Geo2 = colSums(d_ref.d_mon.Geo2*d_normkost.temp)
df.d_mon_dr_Geo2 = data.frame(d_mon_dr_Geo2)
d_mon_sf_dr_Geo2 = colSums(d_ref.d_mon.Geo2*d_normkost.snitt.temp)
df.d_mon_sf_dr_Geo2 = data.frame(d_mon_sf_dr_Geo2)
#legger verdiene i egen frame før de kobles på d_tilDEA
d_tilDEA = cbind.data.frame(d_tilDEA, df.d_mon_dr_Geo2, df.d_mon_sf_dr_Geo2)

rm(d_ref.d_mon.Geo2, df.d_mon_dr_Geo2, d_mon_dr_Geo2, df.d_mon_sf_dr_Geo2, d_mon_sf_dr_Geo2)

#trekker ut referentes verdier for Geo3
d_ref.d_mon.Geo3 = d_ref.data$dr_Geo3
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
d_mon_dr_Geo3 = colSums(d_ref.d_mon.Geo3*d_normkost.temp)
df.d_mon_dr_Geo3 = data.frame(d_mon_dr_Geo3)
d_mon_sf_dr_Geo3 = colSums(d_ref.d_mon.Geo3*d_normkost.snitt.temp)
df.d_mon_sf_dr_Geo3 = data.frame(d_mon_sf_dr_Geo3)
#legger verdiene i egen frame før de kobles på d_tilDEA
d_tilDEA = cbind.data.frame(d_tilDEA, df.d_mon_dr_Geo3, df.d_mon_sf_dr_Geo3)

rm(d_ref.d_mon.Geo3, df.d_mon_dr_Geo3, d_mon_dr_Geo3, df.d_mon_sf_dr_Geo3, d_mon_sf_dr_Geo3)

## Beregner differanse mellom selskapets rvk og mønsterselskapets rvk
d_tilDEA$dm_dr_hsjordand = d_tilDEA$dr_hsjordand - d_tilDEA$d_mon_dr_hsjordand
d_tilDEA$dm_sf_dr_hsjordand = d_tilDEA$dr_hsjordand - d_tilDEA$d_mon_sf_dr_hsjordand
d_tilDEA$dm_dr_s4 = d_tilDEA$dr_s4 - d_tilDEA$d_mon_dr_s4
d_tilDEA$dm_sf_dr_s4 = d_tilDEA$dr_s4 - d_tilDEA$d_mon_sf_dr_s4
d_tilDEA$dm_dr_Geo1 = d_tilDEA$dr_Geo1 - d_tilDEA$d_mon_dr_Geo1
d_tilDEA$dm_sf_dr_Geo1 = d_tilDEA$dr_Geo1 - d_tilDEA$d_mon_sf_dr_Geo1
d_tilDEA$dm_dr_Geo2 = d_tilDEA$dr_Geo2 - d_tilDEA$d_mon_dr_Geo2
d_tilDEA$dm_sf_dr_Geo2 = d_tilDEA$dr_Geo2 - d_tilDEA$d_mon_sf_dr_Geo2
d_tilDEA$dm_dr_Geo3 = d_tilDEA$dr_Geo3 - d_tilDEA$d_mon_dr_Geo3
d_tilDEA$dm_sf_dr_Geo3 = d_tilDEA$dr_Geo3 - d_tilDEA$d_mon_sf_dr_Geo3

# d_score_bs100 "DEAresultat snitt mot snitt i trinn 2"
# label var dm_dr_hsjordand "Diff mønster jordkabelandel regresjon"
# label var dm_dr_s4 "Diff mønster barskog regresjon"
# label var dm_dr_Geo1 "Diff mønster Geo1 Fjellbekk regresjon"  //fjellbekk
# label var dm_dr_Geo2 "Diff mønster Geo2 regresjon Øyvind "	//NyOyvind
# label var dm_dr_Geo3 "Diff mønster Geo3 regresjon Frost"

trinn2_reg = lm(d_score_bs100 ~ dm_sf_dr_hsjordand + 
                        dm_sf_dr_s4 + dm_sf_dr_Geo1 + 
                        dm_sf_dr_Geo2 + dm_sf_dr_Geo3, data = subset(d_tilDEA, idaar!= 1812014))

summary(trinn2_reg)