## DEL 3 KALIBRERING AV DEA-RESULTATER etter avkastningsgrunnlag (AKG)

## Beskrivelse:
# 1. dummy for hvilke dnett selskaper som inngår i kalibrering
# 2. DEA-resultat, bruker "d_deares_til_kal"
# 3. Kostnadsgrunnlag
# 4. Kostnadsnorm (2 x 3)
# 5. Sum Kostnadsgrunnlag
# 6. Sum Kostnadsnorm
# 7. Beregner avkastningsgrunnlag
# 8. beregner sum avkastningsgrunnlag
# 9. Beregner så kalibrert kostnadsnorm
# 10. beregner kalibrert DEA-resultat ved å dele 7. på 3. 


# ad 3:
d_tilDEA$d_kostnadsgrlag <- ((d_tilDEA$d_DV*curr_aar_kpiafaktor) + (d_tilDEA$d_akg*nve.rente.t) + 
                              d_tilDEA$d_avs + (d_tilDEA$d_kile*curr_aar_kpifaktor) + 
                             (d_tilDEA$d_nettap*nettapspris.ir) - (d_tilDEA$d_grs.cost*curr_aar_kpifaktor))
        #OOTO-modell
d_OOTO$d_kostnadsgrlag <- ((d_OOTO$d_DV*curr_aar_kpiafaktor) + (d_OOTO$d_akg*nve.rente.t) + 
                                   d_OOTO$d_avs + (d_OOTO$d_kile*curr_aar_kpifaktor) + 
                                   (d_OOTO$d_nettap*nettapspris.ir) - (d_OOTO$d_grs.cost*curr_aar_kpifaktor))

# ad 4:
d_tilDEA$d_kostnadsnorm_tilkal <- (d_tilDEA$d_deares_til_kal * d_tilDEA$d_kostnadsgrlag)
        #OOTO
d_OOTO$d_kostnadsnorm_tilkal <- (d_OOTO$d_deares_til_kal * d_OOTO$d_kostnadsgrlag)
# ad 5: - gjøres kun for selskaper som er i tilDEA
d_kostnadsgrlag_sum <- sum(d_tilDEA$d_kostnadsgrlag)

# ad 6: gjøres kun for selskaper som er i tilDEA
d_kostnadsnorm_sum <- sum(d_tilDEA$d_kostnadsnorm_tilkal)

# ad 7:
d_tilDEA$d_akg_kal <- d_tilDEA$d_akg
d_OOTO$d_akg_kal <- d_OOTO$d_akg

# ad 8: gjøres kun for selskaper som er i tilDEA
d_akg_kal_sum <- sum(d_tilDEA$d_akg_kal)

# ad 9:
d_tilDEA$d_kostnadsnorm_kalAKG <- (d_tilDEA$d_kostnadsnorm_tilkal + (d_kostnadsgrlag_sum - d_kostnadsnorm_sum) * 
        (d_tilDEA$d_akg_kal/d_akg_kal_sum))
d_tilDEA$d_tillegginorm = ((d_kostnadsgrlag_sum - d_kostnadsnorm_sum)*(d_tilDEA$d_akg_kal/d_akg_kal_sum))
        #Ingen tillegg i norm for OOTO-model selskapene
d_OOTO$d_kostnadsnorm_kalAKG = d_OOTO$d_kostnadsnorm_tilkal

# ad 10:
d_tilDEA$d_escore_etter_kalAKG <- (d_tilDEA$d_kostnadsnorm_kalAKG/d_tilDEA$d_kostnadsgrlag) 
d_gjsnitt_eff = d_kostnadsnorm_sum/d_kostnadsgrlag_sum

# mohh: Vi må gjøre noe med selskapene til gjennomsnitt og spesial. Disse skal få score = 1 for 
# variabelen d_escore_etter_kalAKG.

# ens: Usikker på dette ifht d_escore_etter_kalAKG, i Stata er disse . eller "NA".
# utover dette er verdier for selskaper i spesial-modell nå inkludert.
     