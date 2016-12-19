#### Beregner og velger data for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres  
        # D-nett  
x.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_TOTXDEA")]  
y.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_ab","sf_d_hs","sf_d_ns")]  
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.snitt.d) = d_DEA_id
rownames(y.snitt.d) = d_DEA_id


        # R-nett  
x.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"sf_r_TOTXDEA"]  
y.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_r_vluft","sf_r_vjord","sf_r_vsjo","sf_r_vgrs")]  
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.snitt.r) = r_DEA_id
rownames(y.snitt.r) = r_DEA_id


# Faktiske data for selskaper som skal evalueres
        # D-nett
x.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,"d_TOTXDEA"]
y.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("d_ab","d_hs","d_ns")]
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.faktisk.d) = d_DEA_id
rownames(y.faktisk.d) = d_DEA_id
        
        # R-nett
x.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"r_TOTXDEA"]
rownames(x.faktisk.r)
y.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]

#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.faktisk.r) = r_DEA_id
rownames(y.faktisk.r) = r_DEA_id

#Runder av data til DEA til "hele tusen"  
#Disse får definere teknologien (fronten) i hovedkjøringen
x.snitt.d <- round(x.snitt.d, digits = 0)  
y.snitt.d$sf_d_ab  <- round(y.snitt.d$sf_d_ab, digits = 0)  
y.snitt.d$sf_d_ns <- round(y.snitt.d$sf_d_ns, digits = 0)  
y.snitt.d$sf_d_hs <- round(y.snitt.d$sf_d_hs, digits = 0)

#Disse måles mot fronten og gir gjeldende DEA-score
x.faktisk.d <- round(x.faktisk.d, digits = 0) 
y.faktisk.d$d_ab  <- round(y.faktisk.d$d_ab, digits = 0)  
y.faktisk.d$d_ns <- round(y.faktisk.d$d_ns, digits = 0)  
y.faktisk.d$d_hs <- round(y.faktisk.d$d_hs, digits = 0) 

#Runder av data til DEA til "hele tusen"  
#Disse får definere teknologien (fronten) i hovedkjøringen
x.snitt.r <- round(x.snitt.r, digits = 0)  
y.snitt.r$sf_r_vluft  <- round(y.snitt.r$sf_r_vluft, digits = 0)  
y.snitt.r$sf_r_vjord <- round(y.snitt.r$sf_r_vjord, digits = 0)  
y.snitt.r$sf_r_vsjo <- round(y.snitt.r$sf_r_vsjo, digits = 0)
y.snitt.r$sf_r_vgrs <- round(y.snitt.r$sf_r_vgrs, digits = 0)

#Disse måles mot fronten og gir gjeldende DEA-score
x.faktisk.r <- round(x.faktisk.r, digits = 0) 
y.faktisk.r$r_vluft  <- round(y.faktisk.r$r_vluft, digits = 0)  
y.faktisk.r$r_vjord <- round(y.faktisk.r$r_vjord, digits = 0)  
y.faktisk.r$r_vsjo <- round(y.faktisk.r$r_vsjo, digits = 0)
y.faktisk.r$r_vgrs <- round(y.faktisk.r$r_vgrs, digits = 0)