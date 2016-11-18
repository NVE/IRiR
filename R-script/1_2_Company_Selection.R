#### Utvalg av selskaper ####


# # Endre metoden
# # Diverse utvalg av selskaper, basert på orgnr
# # De som skal måles:
# eval.r = dat$orgnr[dat$r_TOTXDEA >= 7000 & dat$r_vluft > 0 & dat$aar == faktisk.aar]
# # De som kan danne fronten:
# front.r = dat$orgnr[dat$r_TOTXDEA >= 15000 & dat$aar == faktisk.aar]
# # De som skal evalueres, men ikke kan være på fronten:
# sep.eval.r = setdiff(eval.r,front.r)

# Roar metoden
### Denne metoden baserer seg på å angi dummyverdier for selskapene som skal
### være med i DEA basert på listene av selskap i fil "1_0_Config_Assumptions_Data.R"
# D-nett
dat$d_tilDEA = 0
dat$d_tilDEA <- ifelse(dat$d_TOTXDEA > 0, dat$d_tilDEA <- 1, dat$d_tilDEA <- 0)

# Basert på gruppene

for (i in which(dat$id %in% d_dea_til_gjsnitt)){
        dat[i,"d_tilDEA"]  = 0 
}

for (i in which(dat$id %in% d_spesial)){  
        dat[i,"d_tilDEA"]  = 0   
} 

for (i in which(dat$id %in% d_ikkeIR)){  
        dat[i,"d_tilDEA"]  = 0   
} 

for (i in which(dat$aar %in% snitt.aar)){
        d_tilDEA <-dat[dat$d_tilDEA==1,]
}


#Dataframe med alle selskaper som skal være med i standard DEA-modell D-nettt
d_tilDEA <- subset.data.frame(d_tilDEA, !is.na(aar) & aar==faktisk.aar)
#Hjelpe-vektor som kun inneholder ider, sortert etter samme rekkefølge som ovenstående frame
#Brukes for å gi ider til verdier i DEA
d_DEA_id <- d_tilDEA$id

# R-nett
dat$r_tilDEA = 0  
dat$r_tilDEA <- ifelse(dat$r_TOTXDEA > 0, dat$r_tilDEA <- 1, dat$r_tilDEA <- 0) 

for (i in which(dat$id %in% r_dea_til_gjsnitt)){  
        dat[i,"r_tilDEA"]  = 0  
}

for (i in which(dat$id %in% r_spesial)){  
        dat[i,"r_tilDEA"]  = 0   
}  

r_tilDEA <- dat[dat$r_tilDEA == 1,]
#Dataframe med alle selskaper som skal være med i standard DEA-modell R-nettt
r_tilDEA <- subset.data.frame(r_tilDEA, !is.na(aar) & aar==faktisk.aar)
#Brukes for å gi ider til verdier i DEA
r_DEA_id <- r_tilDEA$id