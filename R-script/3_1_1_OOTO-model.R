#3_1_1 Out of the Ordinary (OOTO-model)

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

#disse verdiene er antakelig hentet fra REN. opkto funnet i notat fra 2009
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
d_OOTO$d_deares_til_kal = d_OOTO$d_oppgave_kr/d_OOTO$d_sf_d_oppgave_kr