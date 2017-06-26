#Revenue cap calculations

#Beregner DV på gamlemåten for konsistens i beregniger
dat$ld_opex_2012 = dat$ld_OPEXxS + dat$ld_sal - dat$ld_sal.cap + dat$ld_pens + 
        dat$ld_pens.eq

dat$rd_opex_2012 = dat$rd_OPEXxS + dat$rd_sal - dat$rd_sal.cap + dat$rd_pens + 
        dat$rd_pens.eq

dat$t_opex_2012 = dat$t_OPEXxS + dat$t_sal - dat$t_sal.cap + dat$t_pens + 
        dat$t_pens.eq

RevCap = subset.data.frame(dat, subset = y == y.cb,
                          select=c(comp, id, y, id.y,
                                   fp_ld_OPEX, fp_rd_OPEX, fp_t_OPEX,
                                   ld_opex_2012, rd_opex_2012, t_opex_2012,
                                   fp_ld_cens, fp_rd_cens, fp_t_cens,
                                   ld_rab.sf, rd_rab.sf, t_rab.sf,
                                   ld_dep.sf, rd_dep.sf, t_dep.sf,
                                   fp_ld_391,fp_rd_391, fp_t_391,
                                   fp_ld_cga, fp_rd_cga,
                                   ld_nl, rd_nl,
                                   ld_gci.cost, ap.t_2))

#Compiling data from calibration (Improve - move to 3_0 ? )
ld_EVAL$ld_cn.cal.RAB = ld_calib$cost_norm.calRAB
rd_EVAL$rd_cn.cal.RAB = rd_calib$cost_norm.calRAB

#Calculating total costs
#Local distribution
RevCap$ld_sum.cost = RevCap$fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_ld_cens*y.cb.cpi.factor + RevCap$ld_dep.sf +
                        RevCap$ld_nl*pnl.rc
#Regional distribution
RevCap$rd_sum.cost = RevCap$fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cens*y.cb.cpi.factor + RevCap$rd_dep.sf +
                        RevCap$rd_nl*pnl.rc
#Transmission grid
RevCap$t_sum.cost = RevCap$fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_t_cens*y.cb.cpi.factor + RevCap$t_dep.sf

#Sum alle nettnivåer
RevCap$lrt_sum.cost = RevCap$ld_sum.cost + RevCap$rd_sum.cost + RevCap$t_sum.cost


#Kostnadsgrunnlag til IR
RevCap$ld_cost.RC = RevCap$fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_ld_cens*y.cb.cpi.factor + RevCap$ld_dep.sf +
                        RevCap$d_akg*rente.ir + RevCap$ld_nl*pnl.rc


RevCap$rd_cost.RC = RevCap$fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cens*y.cb.cpi.factor + RevCap$rd_dep.sf +
                        RevCap$r_akg*rente.ir + RevCap$rd_nl*pnl.rc

RevCap$t_cost.RC = RevCap$fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_t_cens*y.cb.cpi.factor + RevCap$t_dep.sf +
                        RevCap$s_akg*rente.ir

RevCap$lrt_cost.RC =  RevCap$d_cost.RC + RevCap$r_cost.RC + RevCap$s_cost.RC

# Faktiske genererte kostnader alle nettnivåer, ekslusive nettap R-nett
# samt kostnader ved utredning og KDS i R-nett
RevCap$lrt_cost.RC.ex.ntR.og.utrR = round(RevCap$lrt_cost.RC - RevCap$fp_rd_cga, digits = 0)

#Faktiske kostnader i "faktisk.aar"

RevCap$lrt_cost.faktisk = (RevCap$d_dv_2012 - RevCap$fp_d_391 - RevCap$fp_d_utred) +
                        (RevCap$r_dv_2012 - RevCap$fp_r_391 - RevCap$fp_rd_cga) +
                        (RevCap$s_dv_2012 - RevCap$fp_s_391) +
                        (RevCap$fp_ld_cens + RevCap$fp_rd_cens + RevCap$fp_t_cens) +
                        RevCap$fp_d_utred + RevCap$fp_rd_cga +
                        (RevCap$ld_nl + RevCap$rd_nl)*RevCap$omraadepris_t2

#Regner total sum av faktiske kostnader i "faktisk.aar", avrundet for konsistens med Stata
lrt_TOT.cost.faktisk = round(sum(RevCap$lrt_cost.faktisk), digits = 0)

## Deretter skal normkostnader beregnes
d_cncR = cbind(ld_EVAL[,c("id", "d_cost_norm.calRAB")])
r_cncR = cbind(rd_EVAL[,c("id", "r_cost_norm.calRAB")])

RevCap  = dplyr::full_join(RevCap, d_cncR, by="id")
RevCap  = dplyr::full_join(RevCap, r_cncR, by="id")
rm(d_cncR, r_cncR)

#Henter inn kostnadsnormer fra OOTO-model. disse gis navn som indikerer
#at de er inkludert i kalibreringen selvom de ikke er det. Dette for å få
#de inn i samme rad som normale selskaper når IR beregnes

d_OOTO.cncR = cbind(d_OOTO[,c("id", "d_cost_norm.calRAB")])


#Legger til kostnadsnormer fra OOTO i RevCap basert på matchende ider
RevCap$d_cost_norm.calRAB[is.na(RevCap$d_cost_norm.calRAB)] = 
        d_OOTO.cncR$d_cost_norm.calRAB[match(RevCap$id[is.na(RevCap$d_cost_norm.calRAB)],d_OOTO.cncR$id)]

rm(d_OOTO.cncR)

r_OOTO.cncR = cbind(r_OOTO[,c("id", "r_cost_norm.calRAB")])


RevCap$r_cost_norm.calRAB[is.na(RevCap$r_cost_norm.calRAB)] = 
        r_OOTO.cncR$r_cost_norm.calRAB[match(RevCap$id[is.na(RevCap$r_cost_norm.calRAB)],r_OOTO.cncR$id)]

rm(r_OOTO.cncR)

#Setter kostnadsnorm til 0 for alle selskaper som ikke har kostnader i gitt
# nettnivå.

RevCap = within(RevCap, d_cost_norm.calRAB[is.na(d_cost_norm.calRAB)==TRUE & fp_ld_OPEX == 0] <- 0)
RevCap = within(RevCap, r_cost_norm.calRAB[is.na(r_cost_norm.calRAB)==TRUE & fp_rd_OPEX == 0] <- 0)


#Legger til kostnadsnormer fra COREC-modellen i RevCap basert på matchende ider
RevCap$d_cost_norm.calRAB[is.na(RevCap$d_cost_norm.calRAB)] = 
        d_COREC$d_cost_norm.calRAB[match(RevCap$id[is.na(RevCap$d_cost_norm.calRAB)], d_COREC$id)]

RevCap$r_cost_norm.calRAB[is.na(RevCap$r_cost_norm.calRAB)] = 
        r_COREC$r_cost_norm.calRAB[match(RevCap$id[is.na(RevCap$r_cost_norm.calRAB)], r_COREC$id)]


#Kostnadsnormer
RevCap$d_cost_norm.precal = RevCap$d_cost_norm.calRAB + (RevCap$d_grs.cost*y.cb.cpi.factor)
RevCap$r_cost_norm.precal = RevCap$r_cost_norm.calRAB + (RevCap$rd_nl*pnl.rc) +
                            RevCap$fp_rd_cga*y.cb.cpi.l.factor
#Kostnadsdekning for alt S-nett
RevCap$s_cost_norm.precal = RevCap$s_cost.RC

RevCap$lrt_cost_norm.precal = RevCap$d_cost_norm.precal + RevCap$r_cost_norm.precal + RevCap$s_cost_norm.precal

####INNTEKTSRAMMENE BEREGNES!

RevCap$lrt_IR.precal = (1-rho)*RevCap$lrt_cost.RC + rho*(RevCap$lrt_cost_norm.precal) #Inntektsramme før kalibrering
RevCap$lrt_DR.precal = RevCap$lrt_IR.precal - RevCap$lrt_sum.cost #Driftsresultat før kalibrering
RevCap$lrt_AVK.precal = RevCap$lrt_DR.precal / (RevCap$d_akg + RevCap$r_akg + RevCap$s_akg) #Avkastning før kalibrering

#### Så rekalibreres det slik at sum inntektsramme er lik kostnadsgrunnlaget
lrt_TOTIR.precal = sum(RevCap$lrt_IR.precal)
lrt_TOTCost.precal = sum(RevCap$lrt_cost.RC)
RevCap$lrt_RAB = RevCap$d_akg + RevCap$r_akg + RevCap$s_akg
lrt_TOTRAB = sum(RevCap$lrt_RAB)

lrt_recal.fact1 = (lrt_TOTIR.precal - lrt_TOTCost.precal) / lrt_TOTRAB
lrt_recal.norm = (lrt_TOTIR.precal - lrt_TOTCost.precal)

lrt_recal.TOT = lrt_IR_vedtak.faktisk.aar - lrt_TOT.cost.faktisk #Totalt beløp til rekalibrering eks renter
lrt_recal.fact2 = (lrt_recal.TOT*(1+nve_rente[as.character(faktisk.aar)])*(1+nve_rente[as.character(faktisk.aar+1)])) / lrt_TOTRAB

lrt_recal.TOT.int = lrt_recal.TOT*(1+nve_rente[as.character(faktisk.aar)])*(1+nve_rente[as.character(faktisk.aar+1)]) # beløp til rekal. inkl renter
#Kostnadsnorm etter kalibrering
RevCap$lrt_cost_norm.poscal = RevCap$lrt_cost_norm.precal - ((RevCap$lrt_RAB * (lrt_recal.fact1 + lrt_recal.fact2)) / rho)
#Inntektsramme etter kalibrering
RevCap$lrt_IR.poscal = (1 - rho) * RevCap$lrt_cost.RC + rho * RevCap$lrt_cost_norm.poscal
lrt_TOTIR.poscal = sum(RevCap$lrt_IR.poscal)
#Driftsresultat etter kalibrering
RevCap$lrt_DR.poscal = RevCap$lrt_IR.poscal - RevCap$lrt_sum.cost
#Avkastning etter kalibrering
RevCap$lrt_AVK.poscal = RevCap$lrt_DR.poscal / RevCap$lrt_RAB
