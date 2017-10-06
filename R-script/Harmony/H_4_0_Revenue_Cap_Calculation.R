#Revenue cap calculations

#Beregner DV på gamlemåten for konsistens i beregniger
RevCap = subset.data.frame(dat, subset = y == y.cb,
                          select=c(comp, id, y, id.y,
                                   fha_fp_ld_OPEX, fha_fp_rd_OPEX, fha_fp_t_OPEX,
                                   fha_fp_ld_cens, fha_fp_rd_cens, fha_fp_t_cens,
                                   fha_fp_ld_rab.sf, fha_fp_rd_rab.sf, fha_fp_t_rab.sf,
                                   fha_fp_ld_dep.sf, fha_fp_rd_dep.sf, fha_fp_t_dep.sf,
                                   fha_fp_ld_391,fha_fp_rd_391, fha_fp_t_391,
                                   fha_fp_ld_cga, fha_fp_rd_cga,
                                   fha_ld_nl, fha_rd_nl,
                                   fha_fp_ld_gci.cost, ap.t_2))

#Compiling data from calibration (Improve - move to 3_0 ? )
ld_EVAL$ld_cn.cal.RAB = ld_calib$cost_norm.calRAB
rd_EVAL$rd_cn.cal.RAB = rd_calib$cost_norm.calRAB

#Calculating total costs
#Local distribution
RevCap$ld_sum.cost = RevCap$fha_fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fha_fp_ld_cens*y.cb.cpi.factor + RevCap$fha_fp_ld_dep.sf +
                        RevCap$fha_ld_nl*pnl.rc
#Regional distribution
RevCap$rd_sum.cost = RevCap$fha_fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fha_fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fha_fp_rd_cens*y.cb.cpi.factor + RevCap$fha_fp_rd_dep.sf +
                        RevCap$fha_rd_nl*pnl.rc
#Transmission grid
RevCap$t_sum.cost = RevCap$fha_fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fha_fp_t_cens*y.cb.cpi.factor + RevCap$fha_fp_t_dep.sf

# Sum of all relevant costs for all grid levels
RevCap$lrt_sum.cost = RevCap$ld_sum.cost + RevCap$rd_sum.cost + RevCap$t_sum.cost

#Cost base used in Revenue cap calculation
RevCap$ld_cost.RC = RevCap$fha_fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fha_fp_ld_cens*y.cb.cpi.factor + RevCap$fha_fp_ld_dep.sf +
                        RevCap$fha_fp_ld_rab.sf*NVE.ir.RC + RevCap$fha_ld_nl*pnl.rc


RevCap$rd_cost.RC = RevCap$fha_fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fha_fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fha_fp_rd_cens*y.cb.cpi.factor + RevCap$fha_fp_rd_dep.sf +
                        RevCap$fha_fp_rd_rab.sf*NVE.ir.RC + RevCap$fha_rd_nl*pnl.rc

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

#Regner total sum av faktiske kostnader i "y.cb", avrundet for konsistens med Stata
# Sum of total cost of inflicted costs in cost base year, rounded for consistency with calculations i Stata (Improve ? - remove round?)
lrt_TOTAL.cost.y.cb = round(sum(RevCap$lrt_cost.cby), digits = 0)

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
        rd_OOTO.cncR$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)],rd_OOTO.cncR$id)]

rm(rd_OOTO.cncR)

# Cost norm equals 0 for all companies without OPEX in given grid-level

RevCap = within(RevCap, ld_cn.cal.RAB[is.na(ld_cn.cal.RAB)==TRUE & fp_ld_OPEX == 0] <- 0)
RevCap = within(RevCap, rd_cn.cal.RAB[is.na(rd_cn.cal.RAB)==TRUE & fp_rd_OPEX == 0] <- 0)



# Import cost norms for companies set to average efficiency
RevCap$ld_cn.cal.RAB[is.na(RevCap$ld_cn.cal.RAB)] = 
        ld_AV.EFF$ld_cn.cal.RAB[match(RevCap$id[is.na(RevCap$ld_cn.cal.RAB)], ld_AV.EFF$id)]

RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
        rd_AV.EFF$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)], rd_AV.EFF$id)]


# Cost norms prior to re-calibration
# See descripton of re-calibration belowe
RevCap$ld_cn.pre.recal = RevCap$ld_cn.cal.RAB + (RevCap$ld_gci.cost*y.cb.cpi.factor)
RevCap$rd_cn.pre.recal = RevCap$rd_cn.cal.RAB + (RevCap$rd_nl*pnl.rc) +
                            RevCap$fp_rd_cga*y.cb.cpi.l.factor

# Cost coverage for all transmission grid, not owned by Statnett
RevCap$t_cn.pre.recal = RevCap$t_cost.RC

RevCap$lrt_cn.pre.recal = RevCap$ld_cn.pre.recal + RevCap$rd_cn.pre.recal + RevCap$t_cn.pre.recal

# Revenue caps are calculated
# Revenue cap before re-calibration
RevCap$lrt_RC.pre.recal = (1-rho)*RevCap$lrt_cost.RC + rho*(RevCap$lrt_cn.pre.recal)
# EBIT before re-calibration
RevCap$lrt_EBIT.pre.recal = RevCap$lrt_RC.pre.recal - RevCap$lrt_sum.cost
# "Return" before re-calibration
RevCap$lrt_RET.pre.recal = RevCap$lrt_EBIT.pre.recal / (RevCap$ld_rab.sf + RevCap$rd_rab.sf + RevCap$t_rab.sf) 

#### Re-Calibration ####
# Re-calibration is described in
# NOR  http://publikasjoner.nve.no/hoeringsdokument/2015/hoeringsdokument2015_04.pdf
# Since revenue caps are calculated based on data from from year t-2, the calibration ensures that
# total revenue cap is equal to expected total cost for the industry as a whole. The re-calibration
# is done when the actual inflicted costs in year t is known, and through this step the total revenue
# cap is equal to the actual costs.

# Sum of all revenue caps before re-calibration
lrt_TOTAL.RC.pre.recal = sum(RevCap$lrt_RC.pre.recal)
# Sum cost before re-calibration
lrt_TOTAL.Cost.pre.recal = sum(RevCap$lrt_cost.RC)
# Regulatory Asset Base for all grid levels calculated for each company, self-funded assets
RevCap$lrt_RAB = RevCap$ld_rab.sf + RevCap$rd_rab.sf + RevCap$t_rab.sf
#Total Regulatory Asset Base of self-funded assets
lrt_TOTRAB = sum(RevCap$lrt_RAB)

#Recalibration factor 1 - difference between total revenue cap and total costs over total RAB
lrt_recal.fact1 = (lrt_TOTAL.RC.pre.recal - lrt_TOTAL.Cost.pre.recal) / lrt_TOTRAB
# Re-calibration Norm, difference between total revenue cap and total costs
lrt_recal.norm = (lrt_TOTAL.RC.pre.recal - lrt_TOTAL.Cost.pre.recal)

# Total sum for re-calibration excl interest
lrt_recal.TOT = lrt_RC_dec.y.cb - lrt_TOTAL.cost.y.cb

lrt_recal.fact2 = (lrt_recal.TOT*(1+NVE.ir[as.character(y.cb)])*(1+NVE.ir[as.character(y.cb+1)])) / lrt_TOTRAB
# Total sum for re-calibration incl interest
lrt_recal.TOT.int = lrt_recal.TOT*(1+NVE.ir[as.character(y.cb)])*(1+NVE.ir[as.character(y.cb+1)])

# Cost norms after re-calibration
RevCap$lrt_cn.pos.recal = RevCap$lrt_cn.pre.recal - ((RevCap$lrt_RAB * (lrt_recal.fact1 + lrt_recal.fact2)) / rho)
# Revenue caps after re-calibration
RevCap$lrt_RC.pos.recal = (1 - rho) * RevCap$lrt_cost.RC + rho * RevCap$lrt_cn.pos.recal
# Total Revenue cap for all companies after re-calibration
lrt_TOTAL.RC.pos.recal = sum(RevCap$lrt_RC.pos.recal)
#EBIT after re-calibration
RevCap$lrt_EBIT.pos.recal = RevCap$lrt_RC.pos.recal - RevCap$lrt_sum.cost
#"Return" after re-calibration
RevCap$lrt_RET.pos.recal = RevCap$lrt_EBIT.pos.recal / RevCap$lrt_RAB
