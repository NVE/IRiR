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


# ad 3: trenger kanskje ikke gange med kpi siden det er 2014 tall?
d_tilDEA$d_kostnadsgrlag <- ((d_tilDEA$d_DV*d_tilDEA$kpia) + (d_tilDEA$d_akg*nve.rente.t) + 
                              d_tilDEA$d_avs + (d_tilDEA$d_kile*d_tilDEA$kpi) + 
                             (d_tilDEA$d_nettap*kraftpris) - (d_tilDEA$d_grs.cost*d_tilDEA$kpi))   
     
# ad 4:
#dat$kostnadsnorm_tilkal <- (dat$d_deares_til_kal * dat$d_kostnadsgrlag)
d_tilDEA$kostnadsnorm_tilkal.snitt <- (d_tilDEA$d_sf_eff * d_tilDEA$d_kostnadsgrlag)
#d_tilDEA$kostnadsnorm_tilkal.faktisk <- (d_tilDEA$d_f_sf_eff*dat$d_kostnadsgrlag)

# ad 5:
d_kostnadsgrlag_sum <- sum(d_tilDEA$d_kostnadsgrlag)

# ad 6:
d_kostnadsnorm_tilkal.snitt <- sum(d_tilDEA$kostnadsnorm_tilkal.snitt)
d_kostnadsnorm_tilkal.faktisk <- sum(d_tilDEA$kostnadsnorm_tilkal.faktisk)

# ad 7:
d_tilDEA$d_akg_kal <- d_tilDEA$d_akg # Avkastningsgrunnlag Dnett

# ad 8:
d_akg_kal_sum <- sum(d_tilDEA$d_akg_kal)

# ad 9:
d_tilDEA$d_kostnadsnorm_kalAKG <- (d_tilDEA$d_kostnadsnorm_tilkal + ((d_tilDEA$d_kostnadsgrlag_sum - d_tilDEA$d_kostnadsnorm_sum) * 
        (d_tilDEA$d_akg_kal/d_tilDEA$d_akg_kal_sum))) 

# ad 10:
dat$d_escore_etter_kalAKG <- (d_tilDEA$d_kostnadsnorm_kalAKG/dat$d_kostnadsgrlag) 




     