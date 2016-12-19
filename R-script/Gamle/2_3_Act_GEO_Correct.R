#Denne filen beregner da det faktiske rammevilkåret pr selskap
# d_rvk = Sum RVK trinn2 Dnett
#Denne baseres på årets verdier kontra reg som skjer basert på snitt
d_tilDEA$d_rvk = d_par_Geo3*d_tilDEA$dm_dr_Geo3 + d_par_hsjordand*d_tilDEA$dm_dr_hsjordand +
        d_par_s4*d_tilDEA$dm_dr_s4 + d_par_Geo1*d_tilDEA$dm_dr_Geo1 + d_par_Geo2*d_tilDEA$dm_dr_Geo2

# d_avrvk = sum RVK trinn2 Dnett basert på diff snitt dm
d_tilDEA$d_avrvk = d_par_Geo3*d_tilDEA$dm_sf_dr_Geo3 + d_par_hsjordand*d_tilDEA$dm_sf_dr_hsjordand +
        d_par_s4*d_tilDEA$dm_sf_dr_s4 + d_par_Geo1*d_tilDEA$dm_sf_dr_Geo1 + d_par_Geo2*d_tilDEA$dm_sf_dr_Geo2

#rvk-verdiene beregnes basert på årets verdier

#RVK jordkabelandel
d_tilDEA$d_rvk_hsjordand = d_par_hsjordand*d_tilDEA$dm_dr_hsjordand
#RVK barskog
d_tilDEA$d_rvk_s4 = d_par_s4*d_tilDEA$dm_dr_s4
#RVK Geo1 Fjellbekk
d_tilDEA$d_rvk_Geo1 = d_par_Geo1*d_tilDEA$dm_dr_Geo1
#RVK Geo2 Øyvind
d_tilDEA$d_rvk_Geo2 = d_par_Geo2*d_tilDEA$dm_dr_Geo2        
#RVK Geo 3 Frost
d_tilDEA$d_rvk_Geo3 = d_par_Geo3*d_tilDEA$dm_dr_Geo3

#DEA-resultat til kalibrering er årets eff mot at snitt definerer teknologien, trukket fra sum rvk.
d_tilDEA$d_deares_til_kal = d_tilDEA$d_dea_til2trinn - d_tilDEA$d_rvk