## Beregner Mønsterselskapets rammevilkår

#Lager ny dataframe av kun normkostandeler for D-nett

d_normkostandeler = dplyr::select(d_vekter.faktisk, contains("d_normk"))
#Velger observerte RVK for referentene.
##OBS! Her må vi finne en mer dynamisk måte å velge referentene på

#Lager dataframe med kun observasjoner for D-nett
d_ref.data = dplyr::filter(d_tilDEA, d_tilDEA$id %in% d_ref)

# Trekker ut referentenes verdier for jordkabel
d_ref.dm.hsjordand = d_ref.data$dr_hsjordand
#transponerer matrisen med normkostandeler
d_normkost.temp = t(d_normkostandeler)
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
dm_dr_hsjordand = colSums(d_ref.dm.hsjordand*d_normkost.temp)
#legger verdiene i egen frame før de kobles på d_tilDEA
df.dm_dr_hsjordand = data.frame(dm_dr_hsjordand)
d_tilDEA = cbind.data.frame(d_tilDEA, df.dm_dr_hsjordand)

rm(df.dm_dr_hsjordand, dm_dr_hsjordand)

#trekker ut referentes verdier for barskog
d_ref.dm.s4 = d_ref.data$dr_s4
#Regner produktsummer pr kolonne, hver kolonne representerer da hvert selskap
dm_dr_s4 = colSums(d_ref.dm.s4*d_normkost.temp)
df.dm_dr_s4 = data.frame(dm_dr_s4)
#legger verdiene i egen frame før de kobles på d_tilDEA
d_tilDEA = cbind.data.frame(d_tilDEA, df.dm_dr_s4)

rm(df.dm_dr_s4, dm_dr_s4)