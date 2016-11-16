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

# ad 1:
dat$d_selskap_tilkal_dummy <- 0 # Hvis 1, skal kalibreres i Dnett
for (i in which(dat$aar == faktisk.aar & dat$d_tilDEA == 1)){
        dat[i,"Hvis 1, skal kalibreres i Dnett"]  = 1 
}

dat$kal_selskap_tilkal_dummy <- 0
for (i in which(dat$d_selskap_tilkal_dummy == 1)){
        dat[i,"Hvis 1, selskap inngår i kalibreringsgrunnlag"] = 1
}

# ad 2:
# ad 3:
dat$d_kostnadsgrlag <- 0
for (i in which(dat$aar == faktisk.aar)){
        ((dat$d_DV*dat$kpia) + (dat$d_akg*nve.rente.t) + dat$d_avs + 
        (dat$d_kile*dat$kpi) + (dat$d_nettap*kraftpris) - 
        (dat$d_grs.cost*dat$kpi))   
}
     
# ad 4:
#dat$kostnadsnorm_tilkal <- (dat$d_deares_til_kal * dat$d_kostnadsgrlag)

# ad 5:


# ad 6:
# ad 7:
dat$d_akg_kal <- dat$d_akg # Avkastningsgrunnlag Dnett

# ad 8:
# ad 9:
# ad 10: