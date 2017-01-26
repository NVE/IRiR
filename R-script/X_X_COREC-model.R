## COREC (cost recovery) - model ##
# Contidon: Incomplete or structural brakes in data
#D-nett
dat$d_COREC = 0


for (i in which(dat$id %in% d_dea_til_gjsnitt & dat$aar==faktisk.aar)){  
        dat[i,"d_COREC"]  = 1   
} 

#Lager dataframe med d_COREC-verdien lik 1
for (i in which(dat$aar %in% faktisk.aar)){
        d_COREC <-dat[dat$d_COREC==1,]
}


d_COREC$d_kostnadsgrlag = ((d_COREC$fp_d_DV*faktisk.aar.kpiafaktor) + (d_COREC$d_akg*nve.rente.t) + 
                                   d_COREC$d_avs + (d_COREC$d_kile*faktisk.aar.kpifaktor) + 
                                   (d_COREC$d_nettap*nettapspris.ir) - (d_COREC$d_grs.cost*faktisk.aar.kpifaktor))

#Kostnadsdekning gir at kostnadsgrlag = kostnadsnorm

d_COREC$d_cost_norm.calRAB = d_COREC$d_kostnadsgrlag
#R-nett

dat$r_COREC = 0


for (i in which(dat$id %in% r_dea_til_gjsnitt & dat$aar==faktisk.aar)){  
        dat[i,"r_COREC"]  = 1   
} 

#Lager dataframe med r_COREC-verdien lik 1
for (i in which(dat$aar %in% faktisk.aar)){
        r_COREC <-dat[dat$r_COREC==1,]
}


r_COREC$r_kostnadsgrlag = (r_COREC$fp_r_DV*faktisk.aar.kpiafaktor) + (r_COREC$r_akg*nve.rente.t) + 
        r_COREC$r_avs + (r_COREC$r_kile*faktisk.aar.kpifaktor)

r_COREC$r_cost_norm.calRAB = r_COREC$r_kostnadsgrlag