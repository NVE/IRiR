#### Beregner og velger data for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres  
        # D-nett  
x.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_TOTXDEA")]  
y.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_ab","sf_d_hs","sf_d_ns")]  



        # R-nett  
x.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"sf_r_TOTXDEA"]  
y.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_r_vluft","sf_r_vjord","sf_r_vsjo","sf_r_vgrs")]  



# Faktiske data for selskaper som skal evalueres
        # D-nett
x.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,"d_TOTXDEA"]
y.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("d_ab","d_hs","d_ns")]

        # R-nett
x.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"r_TOTXDEA"]
y.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]
z.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
kap.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("r_AKG")]
