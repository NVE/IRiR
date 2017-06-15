# 3.1 - OOTO model for companies Out Of The Ordinary
# Companies in 
## Selskaper som av diverse årsaker er unntat vanlig DEA- eller IR-regulering
# D-nett
# d_spesial <- (c(10, 108, 121, 167, 222, 512, 686, 743))

dat$d_OOTO = 0


for (i in which(dat$id %in% d_spesial)){  
        dat[i,"d_OOTO"]  = 1   
} 

for (i in which(dat$aar %in% snitt.aar)){
        d_OOTO <-dat[dat$d_OOTO==1,]
}

#Dataframe med alle selskaper som skal være med i standard DEA-modell D-nettt
d_OOTO <- subset.data.frame(d_OOTO, aar == faktisk.aar)

#disse verdiene er antakelig hentet fra REN. opko funnet i notat fra 2009
d_pris_ab = 12
d_pris_hs = 419
d_pris_ns = 147

#runder av snitt verdier for oppgaver og kostnader
d_OOTO$sf_d_ab  <- round(d_OOTO$sf_d_ab, digits = 0)  
d_OOTO$sf_d_ns <- round(d_OOTO$sf_d_ns, digits = 0)  
d_OOTO$sf_d_hs <- round(d_OOTO$sf_d_hs, digits = 0) 
d_OOTO$sf_d_TOTXDEA <- round(d_OOTO$sf_d_TOTXDEA, digits = 0) 

#d_oppgave = oppgave i D-nett
#faktisk
d_OOTO$d_oppgave = d_OOTO$d_ab*d_pris_ab + d_OOTO$d_hs*d_pris_hs + d_OOTO$d_ns*d_pris_ns
#snitt
d_OOTO$d_sf_d_oppgave = d_OOTO$sf_d_ab*d_pris_ab + d_OOTO$sf_d_hs*d_pris_hs + d_OOTO$sf_d_ns*d_pris_ns

#Beregner oppgaver pr krone
d_OOTO$d_oppgave_kr = d_OOTO$d_oppgave/d_OOTO$d_TOTXDEA
d_OOTO$d_sf_d_oppgave_kr = d_OOTO$d_sf_d_oppgave/d_OOTO$sf_d_TOTXDEA

#Beregner "DEA-resultat" til kalibrering for OOTO
d_OOTO$d_effscore = d_OOTO$d_oppgave_kr/d_OOTO$d_sf_d_oppgave_kr

# Selskaper utenfor DEA
d_OOTO$d_kostnadsgrlag = ((d_OOTO$fp_d_DV*faktisk.aar.kpiafaktor) + (d_OOTO$d_akg*nve.rente.t) + 
                                  d_OOTO$d_avs + (d_OOTO$d_kile*faktisk.aar.kpifaktor) + 
                                  (d_OOTO$d_nettap*nettapspris.ir) - (d_OOTO$d_grs.cost*faktisk.aar.kpifaktor))
# Kostnadsnorm for selskaper i OOTO-model får variabel navn som indikerer at
# de inngår i kalibrering, selvom dette egentlig ikke er tilfelle. Dette gjøres
# for å "samle trådene" i IR-beregningen
d_OOTO$d_cost_norm.calRAB = d_OOTO$d_kostnadsgrlag*d_OOTO$d_effscore


## R-nett
#r_spesial <- (c(10, 18, 35, 41, 88, 98, 106, 135, 147, 156, 161, 162, 173, 184, 
#204, 222, 238, 274, 287, 307, 343, 349, 447, 484, 512, 
#659, 686, 743)) # prøver, 10, her 

dat$r_OOTO = 0


for (i in which(dat$id %in% r_spesial)){  
        dat[i,"r_OOTO"]  = 1   
} 

#Spesialbehandling av ider 35, 162 og 173---------------------------------------

r_OOTO.spes = (c(35, 162, 173))
# Femårige snittverdier (totalkostnad og outputs)
var_sf_r_OOTO = c("r_TOTXDEA", "r_vluft", "r_vjord", "r_vsjo", "r_vgrs")
sf = paste("sf_", var_sf, sep="")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_sf), dimnames = list(var_sf = sf))))
for(c in 1:length(var_sf))
        for(r in 1:nrow(dat))
                if (dat[r,"aar"] %in% 2011:2014 & dat[r,"id"] %in% r_OOTO.spes)
                        dat[r, sf[c]] = mean(dat[dat$orgnr == dat$orgnr[r] & dat$aar %in% 2011:2014, var_sf[c]], na.rm = T)
#Normal OOTO fortsetter---------------------------------------------------------


for (i in which(dat$aar %in% snitt.aar)){
        r_OOTO <-dat[dat$r_OOTO==1,]
}


#Dataframe med alle selskaper som skal være med i spesialmodell R-nettt
r_OOTO <- subset.data.frame(r_OOTO, aar == faktisk.aar)


#runder av snitt verdier for oppgaver og kostnader
r_OOTO$sf_r_vluft  <- round(r_OOTO$sf_r_vluft, digits = 0)  
r_OOTO$sf_r_vjord <- round(r_OOTO$sf_r_vjord, digits = 0)  
r_OOTO$sf_r_vsjo <- round(r_OOTO$sf_r_vsjo, digits = 0)
r_OOTO$sf_r_vgrs <- round(r_OOTO$sf_r_vgrs, digits = 0)
r_OOTO$sf_r_TOTXDEA <- round(r_OOTO$sf_r_TOTXDEA, digits = 0)

#r_oppgave = oppgave i D-nett
#faktisk
r_OOTO$r_oppgave = r_OOTO$r_vluft + r_OOTO$r_vjord + r_OOTO$r_vsjo + r_OOTO$r_vgrs 
#snitt
r_OOTO$r_sf_r_oppgave = r_OOTO$sf_r_vluft + r_OOTO$sf_r_vjord + r_OOTO$sf_r_vsjo + r_OOTO$sf_r_vgrs

#Beregner oppgaver pr krone
r_OOTO$r_oppgave_kr = r_OOTO$r_oppgave/r_OOTO$r_TOTXDEA
r_OOTO$r_sf_r_oppgave_kr = r_OOTO$r_sf_r_oppgave/r_OOTO$sf_r_TOTXDEA

#Beregner effektivitetsscore
r_OOTO$r_effscore = r_OOTO$r_oppgave_kr/r_OOTO$r_sf_r_oppgave_kr

# Selskaper utenfor DEA
r_OOTO$r_kostnadsgrlag = (r_OOTO$fp_r_DV*faktisk.aar.kpiafaktor) + (r_OOTO$r_akg*nve.rente.t) + 
                                  r_OOTO$r_avs + (r_OOTO$r_kile*faktisk.aar.kpifaktor)

r_OOTO$r_cost_norm.calRAB = r_OOTO$r_kostnadsgrlag * r_OOTO$r_effscore