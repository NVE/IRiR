#Revenue cap calculations

#Beregner DV på gamlemåten for konsistens i beregniger
dat$d_dv_2012 = dat$d_DVxL + dat$d_lonn - dat$d_lonnakt + dat$d_pensj + 
                dat$d_pensjek

dat$r_dv_2012 = dat$r_DVxL + dat$r_lonn - dat$r_lonnakt + dat$r_pensj + 
        dat$r_pensjek

dat$s_dv_2012 = dat$s_DVxL + dat$s_lonn - dat$s_lonnakt + dat$s_pensj + 
        dat$s_pensjek

tilIR = subset.data.frame(dat, subset = aar == faktisk.aar,
                select=c(selskap, id, aar, idaar, fp_d_DV, fp_r_DV, fp_s_DV,
                         d_dv_2012, r_dv_2012, s_dv_2012,
                         fp_d_kile, fp_r_kile, fp_s_kile,
                         d_akg, r_akg, s_akg, # Dette er sum av verdier i fp
                         d_avs, r_avs, s_avs, # Dette er sum av verdier i fp
                         fp_d_391,fp_r_391, fp_s_391,
                         fp_d_utred, fp_r_utred,
                         d_nettap, omraadepris_t2))

#Henter inn data fra kalibrering
d_tilDEA$d_cost_norm.calRAB = d_calib$cost_norm.calRAB
r_tilDEA$r_cost_norm.calRAB = r_calib$cost_norm.calRAB

#Sum kostnader
#D-nett
tilIR$d_sum.cost = tilIR$fp_d_DV*faktisk.aar.kpiafaktor + 
                        tilIR$fp_d_kile*faktisk.aar.kpifaktor + tilIR$d_avs +
                        tilIR$d_nettap*nettapspris.ir
#R-nett
tilIR$r_sum.cost = tilIR$fp_r_DV*faktisk.aar.kpiafaktor +
                        tilIR$fp_r_kile*faktisk.aar.kpifaktor + tilIR$r_avs
#S-nett
tilIR$s_sum.cost = tilIR$fp_s_DV*faktisk.aar.kpiafaktor + 
                        tilIR$fp_s_kile*faktisk.aar.kpifaktor + tilIR$s_avs

#Sum alle nettnivåer
tilIR$drs_sum.cost = tilIR$d_sum.cost + tilIR$r_sum.cost + tilIR$s_sum.cost


#Kostnadsgrunnlag til IR
tilIR$d_cost.RC = tilIR$fp_d_DV*faktisk.aar.kpiafaktor + 
                        tilIR$fp_d_kile*faktisk.aar.kpifaktor + tilIR$d_avs +
                        tilIR$d_akg*rente.ir + tilIR$d_nettap*nettapspris.ir


tilIR$r_cost.RC = tilIR$fp_r_DV*faktisk.aar.kpiafaktor + 
                        tilIR$fp_r_kile*faktisk.aar.kpifaktor + tilIR$r_avs +
                        tilIR$r_akg*rente.ir

tilIR$s_cost.RC = tilIR$fp_s_DV*faktisk.aar.kpiafaktor + 
                        tilIR$fp_s_kile*faktisk.aar.kpifaktor + tilIR$s_avs +
                        tilIR$s_akg*rente.ir

tilIR$drs_cost.RC =  tilIR$d_cost.RC + tilIR$r_cost.RC + tilIR$s_cost.RC

# Faktiske genererte kostnader alle nettnivåer, ekslusive nettap R-nett
# samt kostnader ved utredning og KDS i R-nett
tilIR$drs_cost.RC.ex.ntR.og.utrR = round(tilIR$drs_cost.RC - tilIR$fp_r_utred, digits = 0)

#Faktiske kostnader i "faktisk.aar"

tilIR$drs_cost.faktisk = (tilIR$d_dv_2012 - tilIR$fp_d_391 - tilIR$fp_d_utred) +
                        (tilIR$r_dv_2012 - tilIR$fp_r_391 - tilIR$fp_r_utred) +
                        (tilIR$s_dv_2012 - tilIR$fp_s_391) +
                        (tilIR$fp_d_kile + tilIR$fp_r_kile + tilIR$fp_s_kile) +
                        tilIR$fp_d_utred + tilIR$fp_r_utred +
                        tilIR$d_nettap*tilIR$omraadepris_t2


## Deretter skal normkostnader beregnes
d_cncR = cbind(d_tilDEA[,c("id", "d_cost_norm.calRAB")])
r_cncR = cbind(r_tilDEA[,c("id", "r_cost_norm.calRAB")])

tilIR  = dplyr::full_join(tilIR, d_cncR, by="id")
tilIR  = dplyr::full_join(tilIR, r_cncR, by="id")
rm(d_cncR, r_cncR)
