#### 1.1 Calculates Input Values for DEA ####


#### Grensesnittvariabel ####
# Må også ta hensyn til grensesnittkostnadene
dat$d_grs.dummy = 1
for (i in which(dat$id %in% d_dea_til_gjsnitt | dat$id %in% d_spesial)){
        dat[i, "d_grs.dummy"] = 0 
}

# Priser inn grensesnittvariabelen
dat$d_grs.cost = 0
dat$d_grs.cost = ifelse(dat$d_grs.dummy == 1, dat$d_grs * grs_pris, 0)

#### KPI ####
# KPI-justering
var_kpi = c("d_kile", "r_kile", "s_kile")
fp_var_kpi = paste("fp_", var_kpi, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_kpi), dimnames = list(var_kpi = fp_var_kpi))))
for(c in 1:length(var_kpi))
        for(r in 1:nrow(dat))
                dat[r, fp_var_kpi[c]] = dat[r, var_kpi[c]] * kpi[as.character(faktisk.aar)] / kpi[as.character(dat[r, "aar"])]

# KPIA-justering
var_kpia = c("d_391", "r_391", "s_391", "d_pensj", "r_pensj", "s_pensj", "d_pensjek", "r_pensjek", "s_pensjek", 
             "d_impl", "r_impl", "s_impl", "d_DVxL", "d_utred", "r_DVxL", "r_utred", "s_DVxL", "d_lonn", 
             "d_lonnakt", "r_lonn", "r_lonnakt", "s_lonn", "s_lonnakt")
fp_var_kpia = paste("fp_", var_kpia, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_kpia), dimnames = list(var_kpia = fp_var_kpia))))
for(c in 1:length(var_kpia))
        for(r in 1:nrow(dat))
                dat[r, fp_var_kpia[c]] = dat[r, var_kpia[c]] * kpia[as.character(faktisk.aar)] / kpia[as.character(dat[r, "aar"])]


#### Pensjonskostnadsgrunnlag ####
# Historiske snittverdier for 2007-2013 (kun for pensjon)
var_hist = c("fp_d_pensj", "fp_d_pensjek", "fp_d_impl", "fp_r_pensj", "fp_r_pensjek", "fp_r_impl", "fp_s_pensj", "fp_s_pensjek", "fp_s_impl")
names_hist = c("d_pensj", "d_pensjek", "d_impl", "r_pensj", "r_pensjek", "r_impl", "s_pensj", "s_pensjek", "s_impl")
hist = paste("hist_", names_hist, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_hist), dimnames = list(var_hist = hist))))
for(c in 1:length(var_hist))
        for(r in 1:nrow(dat)){
                if (dat[r, "aar"] <= 2013)
                        dat[r, hist[c]] = mean(dat[dat$orgnr == dat$orgnr[r] & dat$aar %in% hist.pensj.aar,var_hist[c]], na.rm = T)
                if (dat[r, "aar"] > 2013)
                        dat[r, hist[c]] = dat[r, var_hist[c]]
}

# Femårige snittverdier (kun for pensjon)
var_avg = c("hist_d_pensj", "hist_d_pensjek", "hist_d_impl", "hist_r_pensj", "hist_r_pensjek", "hist_r_impl", "hist_s_pensj", "hist_s_pensjek", "hist_s_impl", "d_grs")
names_avg = c("d_pensj", "d_pensjek", "d_impl", "r_pensj", "r_pensjek", "r_impl", "s_pensj", "s_pensjek", "s_impl", "d_grs")
avg = paste("av_", names_avg,sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_avg), dimnames = list(var_avg = avg))))
for(c in 1:length(var_avg))
        for(r in 1:nrow(dat))
                if (dat[r,"aar"] %in% snitt.aar)
                        dat[r, avg[c]] = mean(dat[dat$orgnr == dat$orgnr[r] & dat$aar %in% snitt.aar,var_avg[c]], na.rm = T)

# Pensjonskostnadsgrunnlag
dat$fp_d_pensjkostgrlag = dat$av_d_pensj + dat$av_d_pensjek - dat$av_d_impl
dat$fp_r_pensjkostgrlag = dat$av_r_pensj + dat$av_r_pensjek - dat$av_r_impl
dat$fp_s_pensjkostgrlag = dat$av_s_pensj + dat$av_s_pensjek - dat$av_s_impl


#### Kostnadsgrunnlag ####
# Compute TOTXDEA for D-nett
dat$fp_d_dv     = dat$fp_d_DVxL + dat$fp_d_lonn - dat$fp_d_lonnakt + dat$fp_d_pensjkostgrlag
dat$fp_d_DV     = dat$fp_d_dv - dat$fp_d_391 - dat$fp_d_utred
dat$d_akg       = dat$d_bfv * arb.kap.paaslag
dat$d_abakg     = dat$d_abbfv * arb.kap.paaslag
dat$d_AKG       = dat$d_akg + dat$d_abakg
dat$d_AVS       = dat$d_avs + dat$d_abavs
dat$d_nettapkr  = dat$d_nettap * nettapspris.dea
dat$d_TOTXDEA   = dat$fp_d_DV + (dat$d_AKG * rente.dea) + dat$d_AVS + dat$fp_d_kile + dat$d_nettapkr - dat$d_grs.cost

# Compute TOTXDEA for R-nett
dat$fp_r_dv     = dat$fp_r_DVxL + dat$fp_r_lonn - dat$fp_r_lonnakt + dat$fp_r_pensjkostgrlag 
dat$fp_r_DV     = dat$fp_r_dv - dat$fp_r_391 - dat$fp_r_utred
dat$r_akg       = dat$r_bfv * arb.kap.paaslag
dat$r_abakg     = dat$r_abbfv * arb.kap.paaslag
dat$r_AKG       = dat$r_akg + dat$r_abakg
dat$r_AVS       = dat$r_avs + dat$r_abavs
dat$r_TOTXDEA   = dat$fp_r_DV + (dat$r_AKG * rente.dea) + dat$r_AVS + dat$fp_r_kile

# Compute TOTXDEA for S-nett
dat$fp_s_dv     = dat$fp_s_DVxL + dat$fp_s_lonn - dat$fp_s_lonnakt + dat$fp_s_pensjkostgrlag
dat$fp_s_DV    = dat$fp_s_dv - dat$fp_s_391
dat$s_akg       = dat$s_bfv * arb.kap.paaslag 
dat$s_TOTXDEA   = dat$fp_s_DV + (dat$s_akg * rente.dea) + dat$s_avs + dat$fp_s_kile

# Femårige snittverdier (totalkostnad og outputs)
var_sf = c("d_TOTXDEA", "d_ab", "d_hs", "d_ns", "r_TOTXDEA", "r_vluft", "r_vjord", "r_vsjo", "r_vgrs")
sf = paste("sf_", var_sf, sep="")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_sf), dimnames = list(var_sf = sf))))
for(c in 1:length(var_sf))
        for(r in 1:nrow(dat))
                if (dat[r,"aar"] %in% snitt.aar)
                        dat[r, sf[c]] = mean(dat[dat$orgnr == dat$orgnr[r] & dat$aar %in% snitt.aar, var_sf[c]], na.rm = T)

