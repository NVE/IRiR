#### 1.1 Calculates Input Values for DEA ####

#### Pensjonskostnadskjøret ####


dat$fp_d_pensj = dat$d_pensj*dat$kpia
dat$fp_d_pensjek = dat$d_pensjek * dat$kpia
dat$fp_d_impl = dat$d_impl * dat$kpia
dat$fp_r_pensj = dat$r_pensj*dat$kpia
dat$fp_r_pensjek = dat$r_pensjek * dat$kpia
dat$fp_r_impl = dat$r_impl * dat$kpia
dat$fp_s_pensj = dat$s_pensj*dat$kpia
dat$fp_s_pensjek = dat$s_pensjek * dat$kpia
dat$fp_s_impl = dat$s_impl * dat$kpia


## Må beregne "historiske verdier" for perioden 2007-2013
hist.pensj.aar = 2007:2013

dat$temp.d.p = dat$fp_d_pensj
dat$temp.d.pk = dat$fp_d_pensjek
dat$temp.d.i = dat$fp_d_impl
dat$temp.r.p = dat$fp_r_pensj
dat$temp.r.pk = dat$fp_r_pensjek
dat$temp.r.i = dat$fp_r_impl
dat$temp.s.p = dat$fp_s_pensj
dat$temp.s.pk = dat$fp_s_pensjek
dat$temp.s.i = dat$fp_s_impl

for(i in which(dat$aar %in% hist.pensj.aar))
{
        dat[i,"fp_d_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.p"], na.rm = T)
        dat[i,"fp_d_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.pk"], na.rm = T)
        dat[i,"fp_d_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.d.i"], na.rm = T)
        dat[i,"fp_r_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.p"], na.rm = T)
        dat[i,"fp_r_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.pk"], na.rm = T)
        dat[i,"fp_r_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.r.i"], na.rm = T)
        dat[i,"fp_s_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.p"], na.rm = T)
        dat[i,"fp_s_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.pk"], na.rm = T)
        dat[i,"fp_s_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% hist.pensj.aar,"temp.s.i"], na.rm = T)
}

dat$temp.d.p <-NULL
dat$temp.d.pk <-NULL
dat$temp.d.i <-NULL
dat$temp.r.p <-NULL
dat$temp.r.pk <-NULL
dat$temp.r.i <-NULL
dat$temp.s.p <-NULL
dat$temp.s.pk <-NULL
dat$temp.s.i <-NULL



# Femårig snitt av pensjonskostnader i faste priser til kostnadsgrunnlag
for(i in which(dat$aar %in% snitt.aar))
{
        dat[i,"av_fp_d_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_pensj"], na.rm = T)
        dat[i,"av_fp_d_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_pensjek"], na.rm = T)
        dat[i,"av_fp_d_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_impl"], na.rm = T)
        dat[i,"av_fp_r_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_pensj"], na.rm = T)
        dat[i,"av_fp_r_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_pensjek"], na.rm = T)
        dat[i,"av_fp_r_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_impl"], na.rm = T)
        dat[i,"av_fp_s_pensj"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_pensj"], na.rm = T)
        dat[i,"av_fp_s_pensjek"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_pensjek"], na.rm = T)
        dat[i,"av_fp_s_impl"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_s_impl"], na.rm = T)
        dat[i,"av_d_grs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_grs"], na.rm = T)
}

# Pensjonskostnadsgrunnlaget etablers for alle nettnivåer
dat$d_pensjkostgrlag = (dat$av_fp_d_pensj + dat$av_fp_d_pensjek - dat$av_fp_d_impl) / dat$kpia
dat$r_pensjkostgrlag = (dat$av_fp_r_pensj + dat$av_fp_r_pensjek - dat$av_fp_r_impl) / dat$kpia
dat$s_pensjkostgrlag = (dat$av_fp_s_pensj + dat$av_fp_s_pensjek - dat$av_fp_s_impl) / dat$kpia


#### TOTEX Beregninger ####

# Konstruerer frontlov-variabel
dat$frontlov = "."

# Velger deretter korrekte renter avhengig om kjøringen er i varsel eller vedtaksmodus
if (vedtak == 1)  {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.t
        nettapspris.dea = systempris.t2
} else {
        rente.dea = nve.rente.t2
        rente.ir = nve.rente.estimert
        nettapspris.dea = systempris.t2
}

# Compute totx for D-nett
dat$d_dv = dat$d_DVxL+dat$d_lonn-dat$d_lonnakt+dat$d_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering
dat$d_dv_2012 = dat$d_DVxL +dat$d_lonn - dat$d_lonnakt + dat$d_pensj + dat$d_pensjek
# Må også ta hensyn til grensesnittkostnadene
dat$d_grs.dummy <- 1

for (i in which(dat$id %in% d_dea_til_gjsnitt)){
        dat[i,"d_grs.dummy"]  = 0 
}

for (i in which(dat$id %in% d_spesial)){
        dat[i,"d_grs.dummy"]  = 0 
}

# Priser inn grensesnittvariabelen. Her med faktor 1.11, ukjent rasjonale bak denne verdien
dat$d_grs.cost <- 0
grs.kostfaktor = 1

dat$d_grs.cost <- ifelse(dat$d_grs.dummy==1,dat$d_grs*grs.kostfaktor, 0)

dat$d_DV =  dat$d_dv- dat$d_391- dat$d_utred  
dat$d_akg =  dat$d_bfv*arb.kap.paaslag
dat$d_abakg =  dat$d_abbfv*arb.kap.paaslag
dat$d_AKG =  dat$d_akg + dat$d_abakg
dat$d_AVS =  dat$d_avs + dat$d_abavs
dat$d_nettapkr = dat$d_nettap*nettapspris.dea
dat$d_TOTXDEA =  dat$d_DV+( dat$d_AKG*rente.dea)+ dat$d_AVS + dat$d_kile + dat$d_nettapkr - dat$d_grs.cost



# Compute totx for R-nett
dat$r_dv = dat$r_DVxL+dat$r_lonn-dat$r_lonnakt+dat$r_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering                
dat$r_dv_2012 = dat$r_DVxL +dat$r_lonn - dat$r_lonnakt + dat$r_pensj + dat$r_pensjek

dat$r_DV = dat$r_dv - dat$r_391- dat$r_utred
dat$r_akg = (dat$r_bfv*arb.kap.paaslag)
dat$r_abakg =  dat$r_abbfv*arb.kap.paaslag
dat$r_AKG =  dat$r_akg + dat$r_abakg
dat$r_AVS = dat$r_avs + dat$r_abavs
dat$r_TOTXDEA = dat$r_DV + ( dat$r_AKG*rente.dea) + dat$r_AVS+ dat$r_kile

# Compute totx for S-nett
dat$s_dv = dat$s_DVxL + dat$s_lonn - dat$s_lonnakt + dat$s_pensjkostgrlag
# Beregner også dv på gæmlemåten for kalibrering 
dat$s_dv_2012 = dat$s_DVxL + dat$s_lonn - dat$s_lonnakt +dat$s_pensj + dat$s_pensjek 
dat$s_DV = dat$s_dv - dat$s_391
dat$s_akg = (dat$s_bfv*arb.kap.paaslag) 
dat$s_AKG =  dat$s_akg 
dat$s_AVS = dat$s_avs 
dat$s_TOTXDEA = dat$s_DV + ( dat$s_AKG*rente.dea) + dat$s_AVS+ dat$s_kile


# Beregner gjennomsnittsfront
# Først opprettes verdier i faste priser
# Variabler som justeres med vanlig KPI
dat$fp_d_kile = dat$d_kile * dat$kpi
dat$fp_r_kile = dat$r_kile * dat$kpi
dat$fp_s_kile = dat$s_kile * dat$kpi

#Variabler som justeres med KPI for varer og tjenester med arbeidslønn
# som dominerende prisfaktor. Tabell 03363. 
dat$fp_d_DV = dat$d_DV*dat$kpia
dat$fp_r_DV = dat$r_DV*dat$kpia
dat$fp_s_DV = dat$s_DV*dat$kpia

dat$fp_d_391 = dat$d_391*dat$kpia
dat$fp_r_391 = dat$r_391*dat$kpia
dat$fp_s_391 = dat$s_391*dat$kpia

dat$fp_d_dv = dat$d_dv*dat$kpia 
dat$fp_d_utred = dat$d_utred*dat$kpia

dat$fp_r_dv = dat$r_dv*dat$kpia
dat$fp_r_utred = dat$r_utred*dat$kpia

dat$fp_s_dv = dat$s_dv*dat$kpia 

# TOTX til snittfront
# Setter alle NA-observasjoner til 0
#dat[is.na(dat)] = 0

# D-nett
dat$fp_d_TOTXDEA = dat$fp_d_DV + dat$d_AKG*rente.dea + dat$d_AVS + dat$fp_d_kile + 
        dat$d_nettapkr - dat$d_grs.cost
dat$sf_d_DV = dat$d_DV

# R-nett
dat$fp_r_TOTXDEA = dat$fp_r_DV + dat$r_AKG*rente.dea + dat$r_AVS + dat$fp_r_kile
dat$sf_r_vluft = dat$r_vluft
dat$sf_r_vjord = dat$r_vjord
dat$sf_r_vsjo = dat$r_vsjo
dat$rd_vgrs =  dat$d_grs +dat$r_vgrs
dat$sf_rd_vgrs = dat$d_grs +dat$r_vgrs
dat$sf_r_vgrs =dat$r_vgrs

# Beregner snitt av kostnader og output 
# Legger snitt-tallet inn i rad for faktisk år for hvert selskap, men oppretter ny kolonne
# Snittfront data for D-nett
for(i in which(dat$aar == faktisk.aar))
{
        dat[i,"sf_d_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_d_TOTXDEA"])
        dat[i,"sf_d_ab"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ab"])
        dat[i,"sf_d_hs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_hs"])
        dat[i,"sf_d_ns"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"d_ns"])
}

# Snittfront data for R-nett
for(i in which(dat$aar == faktisk.aar))
{
        dat[i,"sf_r_TOTXDEA"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"fp_r_TOTXDEA"])
        dat[i,"sf_r_vluft"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vluft"])
        dat[i,"sf_r_vjord"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vjord"])
        dat[i,"sf_r_vsjo"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vsjo"])
        dat[i,"sf_r_vgrs"] = mean(dat[dat$orgnr == dat$orgnr[i] & dat$aar %in% snitt.aar,"r_vgrs"])
}
