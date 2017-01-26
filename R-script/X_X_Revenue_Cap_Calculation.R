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
                         d_nettap, r_nettap,
                         d_grs.cost, omraadepris_t2))

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
                        tilIR$fp_r_utred*faktisk.aar.kpiafaktor +
                        tilIR$fp_r_kile*faktisk.aar.kpifaktor + tilIR$r_avs +
                        tilIR$r_nettap*nettapspris.ir
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
                        tilIR$fp_r_utred*faktisk.aar.kpiafaktor +
                        tilIR$fp_r_kile*faktisk.aar.kpifaktor + tilIR$r_avs +
                        tilIR$r_akg*rente.ir + tilIR$r_nettap*nettapspris.ir

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
                        (tilIR$d_nettap + tilIR$r_nettap)*tilIR$omraadepris_t2

#Regner total sum av faktiske kostnader i "faktisk.aar", avrundet for konsistens med Stata
drs_TOT.cost.faktisk = round(sum(tilIR$drs_cost.faktisk), digits = 0)

## Deretter skal normkostnader beregnes
d_cncR = cbind(d_tilDEA[,c("id", "d_cost_norm.calRAB")])
r_cncR = cbind(r_tilDEA[,c("id", "r_cost_norm.calRAB")])

tilIR  = dplyr::full_join(tilIR, d_cncR, by="id")
tilIR  = dplyr::full_join(tilIR, r_cncR, by="id")
rm(d_cncR, r_cncR)

#Henter inn kostnadsnormer fra OOTO-model. disse gis navn som indikerer
#at de er inkludert i kalibreringen selvom de ikke er det. Dette for å få
#de inn i samme rad som normale selskaper når IR beregnes

d_OOTO.cncR = cbind(d_OOTO[,c("id", "d_cost_norm.calRAB")])


#Legger til kostnadsnormer fra OOTO i tilIR basert på matchende ider
tilIR$d_cost_norm.calRAB[is.na(tilIR$d_cost_norm.calRAB)] = 
        d_OOTO.cncR$d_cost_norm.calRAB[match(tilIR$id[is.na(tilIR$d_cost_norm.calRAB)],d_OOTO.cncR$id)]

rm(d_OOTO.cncR)

r_OOTO.cncR = cbind(r_OOTO[,c("id", "r_cost_norm.calRAB")])


tilIR$r_cost_norm.calRAB[is.na(tilIR$r_cost_norm.calRAB)] = 
        r_OOTO.cncR$r_cost_norm.calRAB[match(tilIR$id[is.na(tilIR$r_cost_norm.calRAB)],r_OOTO.cncR$id)]

rm(r_OOTO.cncR)

#Setter kostnadsnorm til 0 for alle selskaper som ikke har kostnader i gitt
# nettnivå.

tilIR = within(tilIR, d_cost_norm.calRAB[is.na(d_cost_norm.calRAB)==TRUE & fp_d_DV == 0] <- 0)
tilIR = within(tilIR, r_cost_norm.calRAB[is.na(r_cost_norm.calRAB)==TRUE & fp_r_DV == 0] <- 0)


#Legger til kostnadsnormer fra COREC-modellen i tilIR basert på matchende ider
tilIR$d_cost_norm.calRAB[is.na(tilIR$d_cost_norm.calRAB)] = 
        d_COREC$d_cost_norm.calRAB[match(tilIR$id[is.na(tilIR$d_cost_norm.calRAB)], d_COREC$id)]

tilIR$r_cost_norm.calRAB[is.na(tilIR$r_cost_norm.calRAB)] = 
        r_COREC$r_cost_norm.calRAB[match(tilIR$id[is.na(tilIR$r_cost_norm.calRAB)], r_COREC$id)]


#Kostnadsnormer
tilIR$d_cost_norm.precal = tilIR$d_cost_norm.calRAB + (tilIR$d_grs.cost*faktisk.aar.kpifaktor)
tilIR$r_cost_norm.precal = tilIR$r_cost_norm.calRAB + (tilIR$r_nettap*nettapspris.ir) +
                            tilIR$fp_r_utred*faktisk.aar.kpiafaktor
#Kostnadsdekning for alt S-nett
tilIR$s_cost_norm.precal = tilIR$s_cost.RC

tilIR$drs_cost_norm.precal = tilIR$d_cost_norm.precal + tilIR$r_cost_norm.precal + tilIR$s_cost_norm.precal

####INNTEKTSRAMMENE BEREGNES!

tilIR$drs_IR.precal = (1-rho)*tilIR$drs_cost.RC + rho*(tilIR$drs_cost_norm.precal) #Inntektsramme før kalibrering
tilIR$drs_DR.precal = tilIR$drs_IR.precal - tilIR$drs_sum.cost #Driftsresultat før kalibrering
tilIR$drs_AVK.precal = tilIR$drs_DR.precal / (tilIR$d_akg + tilIR$r_akg + tilIR$s_akg) #Avkastning før kalibrering

#### Så rekalibreres det slik at sum inntektsramme er lik kostnadsgrunnlaget
drs_TOTIR.precal = sum(tilIR$drs_IR.precal)
drs_TOTCost.precal = sum(tilIR$drs_cost.RC)
tilIR$drs_RAB = tilIR$d_akg + tilIR$r_akg + tilIR$s_akg
drs_TOTRAB = sum(tilIR$drs_RAB)

drs_recal.fact1 = (drs_TOTIR.precal - drs_TOTCost.precal) / drs_TOTRAB
drs_recal.norm = (drs_TOTIR.precal - drs_TOTCost.precal)

drs_recal.TOT = drs_IR_vedtak.faktisk.aar - drs_TOT.cost.faktisk #Totalt beløp til rekalibrering eks renter
drs_recal.fact2 = (drs_recal.TOT*(1+nve_rente[as.character(faktisk.aar)])*(1+nve_rente[as.character(faktisk.aar+1)])) / drs_TOTRAB

drs_recal.TOT.int = drs_recal.TOT*(1+nve_rente[as.character(faktisk.aar)])*(1+nve_rente[as.character(faktisk.aar+1)]) # beløp til rekal. inkl renter
#Kostnadsnorm etter kalibrering
tilIR$drs_cost_norm.poscal = tilIR$drs_cost_norm.precal - ((tilIR$drs_RAB * (drs_recal.fact1 + drs_recal.fact2)) / rho)
#Inntektsramme etter kalibrering
tilIR$drs_IR.poscal = (1 - rho) * tilIR$drs_cost.RC + rho * tilIR$drs_cost_norm.poscal
drs_TOTIR.poscal = sum(tilIR$drs_IR.poscal)
#Driftsresultat etter kalibrering
tilIR$drs_DR.poscal = tilIR$drs_IR.poscal - tilIR$drs_sum.cost
#Avkastning etter kalibrering
tilIR$drs_AVK.poscal = tilIR$drs_DR.poscal / tilIR$drs_RAB