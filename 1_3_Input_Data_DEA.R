#### Beregner og velger data for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres  
# D-nett  
x.sf.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_TOTXDEA")]  
y.sf.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_ab","sf_d_hs","sf_d_ns")]  
names(x.sf.d) = dat #Fungerer ikke  
rownames(y.sf.d) = dat  

# R-nett  
x.sf.r = dat[dat$orgnr %in% r_tilDEA & dat$aar == faktisk.aar,"sf_r_TOTXDEA"]  
y.sf.r = dat[dat$orgnr %in% r_tilDEA & dat$aar == faktisk.aar,c("sf_r_vluft","sf_r_vjord","sf_r_vsjo","sf_r_vgrs")]  
names(x.sf.r) = dat  
rownames(y.sf.r) = dat 

# Faktiske data for selskaper som skal evalueres
x.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,"r_TOTXDEA"]
y.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]
z.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("rr_he","rr_s12")]
kap.faktisk.r = dat[dat$orgnr %in% eval.r & dat$aar == faktisk.aar,c("r_AKG")]
names(x.faktisk.r) = eval.r
rownames(y.faktisk.r) = eval.r
rownames(z.faktisk.r) = eval.r
names(kap.faktisk.r) = eval.r