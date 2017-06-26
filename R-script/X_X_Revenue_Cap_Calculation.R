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

# Sum of all relevant costs for all grid levels
RevCap$lrt_sum.cost = RevCap$ld_sum.cost + RevCap$rd_sum.cost + RevCap$t_sum.cost

#Cost base used in Revenue cap calculation
RevCap$ld_cost.RC = RevCap$fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_ld_cens*y.cb.cpi.factor + RevCap$ld_dep.sf +
                        RevCap$ld_rab.sf*NVE.ir.RC + RevCap$ld_nl*pnl.rc


RevCap$rd_cost.RC = RevCap$fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cens*y.cb.cpi.factor + RevCap$rd_dep.sf +
                        RevCap$rd_rab.sf*NVE.ir.RC + RevCap$rd_nl*pnl.rc

RevCap$t_cost.RC = RevCap$fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_t_cens*y.cb.cpi.factor + RevCap$t_dep.sf +
                        RevCap$t_rab.sf*NVE.ir.RC

RevCap$lrt_cost.RC =  RevCap$ld_cost.RC + RevCap$rd_cost.RC + RevCap$t_cost.RC

# Actual inflicted costs in all grid levels, excluding network losses in regional distribution
# as well as grid assetsments costs in regional distributioin
RevCap$lrt_cost.RC.ex.nlR.og.cgaR = round(RevCap$lrt_cost.RC - RevCap$fp_rd_cga, digits = 0)

# Inflicted costs in cost base year

RevCap$lrt_cost.cby = (RevCap$ld_opex_2012 - RevCap$fp_ld_391 - RevCap$fp_ld_cga) +
                        (RevCap$rd_opex_2012 - RevCap$fp_rd_391 - RevCap$fp_rd_cga) +
                        (RevCap$t_opex_2012 - RevCap$fp_t_391) +
                        (RevCap$fp_ld_cens + RevCap$fp_rd_cens + RevCap$fp_t_cens) +
                        RevCap$fp_ld_cga + RevCap$fp_rd_cga +
                        (RevCap$ld_nl + RevCap$rd_nl)*RevCap$ap.t_2

#Regner total sum av faktiske kostnader i "faktisk.aar", avrundet for konsistens med Stata
# Sum of total cost of inflicted costs in cost base year, rounded for consistency with calculations i Stata (Improve ? - remove round?)
lrt_TOT.cost.cby = round(sum(RevCap$lrt_cost.cby), digits = 0)

## Cost norms
ld_cncR = cbind(ld_EVAL[,c("id", "ld_cn.cal.RAB")])
rd_cncR = cbind(rd_EVAL[,c("id", "rd_cn.cal.RAB")])

RevCap  = dplyr::full_join(RevCap, ld_cncR, by="id")
RevCap  = dplyr::full_join(RevCap, rd_cncR, by="id")
rm(ld_cncR, rd_cncR)

# Retrive cost norms from OOTO-model.
# .._cn.cal.RAB) indicates that these values are included in calibration.
# Contradictory to this, these companies are not included in the
# calibration and is done to "gather the pieces" in the RC calculation


ld_OOTO.cncR = cbind(ld_OOTO[,c("id", "ld_cn.cal.RAB")])



# Add cost norms for companies in OOTO to RevCap-frame based on matching id's
RevCap$ld_cn.cal.RAB[is.na(RevCap$ld_cn.cal.RAB)] = 
        ld_OOTO.cncR$ld_cn.cal.RAB[match(RevCap$id[is.na(RevCap$ld_cn.cal.RAB)],ld_OOTO.cncR$id)]

rm(ld_OOTO.cncR)

rd_OOTO.cncR = cbind(rd_OOTO[,c("id", "rd_cn.cal.RAB")])


RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
        r_OOTO.cncR$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)],r_OOTO.cncR$id)]

rm(rd_OOTO.cncR)

#Setter kostnadsnorm til 0 for alle selskaper som ikke har kostnader i gitt
# nettnivå.

RevCap = within(RevCap, d_cost_norm.calRAB[is.na(d_cost_norm.calRAB)==TRUE & fp_ld_OPEX == 0] <- 0)
RevCap = within(RevCap, rd_cn.cal.RAB[is.na(rd_cn.cal.RAB)==TRUE & fp_rd_OPEX == 0] <- 0)


#Legger til kostnadsnormer fra COREC-modellen i RevCap basert på matchende ider
RevCap$d_cost_norm.calRAB[is.na(RevCap$d_cost_norm.calRAB)] = 
        d_COREC$d_cost_norm.calRAB[match(RevCap$id[is.na(RevCap$d_cost_norm.calRAB)], d_COREC$id)]

RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
        r_COREC$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)], r_COREC$id)]


#Kostnadsnormer
RevCap$d_cost_norm.precal = RevCap$d_cost_norm.calRAB + (RevCap$d_grs.cost*y.cb.cpi.factor)
RevCap$r_cost_norm.precal = RevCap$rd_cn.cal.RAB + (RevCap$rd_nl*pnl.rc) +
                            RevCap$fp_rd_cga*y.cb.cpi.l.factor
#Kostnadsdekning for alt S-nett
RevCap$s_cost_norm.precal = RevCap$s_cost.RC

RevCap$lrt_cost_norm.precal = RevCap$d_cost_norm.precal + RevCap$r_cost_norm.precal + RevCap$s_cost_norm.precal

####INNTEKTSRAMMENE BEREGNES!

RevCap$lrt_IR.precal = (1-rho)*RevCap$lrt_cost.RC + rho*(RevCap$lrt_cost_norm.precal) #Inntektsramme før kalibrering
RevCap$lrt_DR.precal = RevCap$lrt_IR.precal - RevCap$lrt_sum.cost #Driftsresultat før kalibrering
RevCap$lrt_AVK.precal = RevCap$lrt_DR.precal / (RevCap$ld_rab.sf + RevCap$rd_rab.sf + RevCap$t_rab.sf) #Avkastning før kalibrering

#### Så rekalibreres det slik at sum inntektsramme er lik kostnadsgrunnlaget
lrt_TOTIR.precal = sum(RevCap$lrt_IR.precal)
lrt_TOTCost.precal = sum(RevCap$lrt_cost.RC)
RevCap$lrt_RAB = RevCap$ld_rab.sf + RevCap$rd_rab.sf + RevCap$t_rab.sf
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
