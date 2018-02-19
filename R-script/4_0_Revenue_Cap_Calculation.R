#Revenue cap calculations

#Beregner DV på gamlemåten for konsistens i beregniger
dat$ld_opex_2012 = dat$ld_OPEXxS + dat$ld_sal - dat$ld_sal.cap + dat$ld_pens + 
        dat$ld_pens.eq

dat$rd_opex_2012 = dat$rd_OPEXxS + dat$rd_sal - dat$rd_sal.cap + dat$rd_pens + 
        dat$rd_pens.eq

dat$t_opex_2012 = dat$t_OPEXxS + dat$t_sal - dat$t_sal.cap + dat$t_pens + 
        dat$t_pens.eq

RevCap = subset.data.frame(dat, subset = y == y.cb,
                          select=c(comp, id, orgn, y, id.y,
                                   fp_ld_OPEX, fp_rd_OPEX, fp_t_OPEX,
                                   ld_opex_2012, rd_opex_2012, t_opex_2012,
                                   fp_ld_cens, fp_rd_cens, fp_t_cens,
                                   ld_rab.sf, rd_rab.sf, t_rab.sf,
                                   ld_dep.sf, rd_dep.sf, t_dep.sf,
                                   fp_ld_391,fp_rd_391, fp_t_391,
                                   fp_ld_cga, fp_rd_cga,
                                   ld_nl, rd_nl,
                                   ld_gci.cost, ap.t_2, pnl.rc))

#Remove observations for companies without Reveneue Cap
RevCap = filter(RevCap, !id %in% rd_no.rc | !id %in% ld_no.rc)


#Compiling data from calibration (Improve - move to 3_0 ? )
ld_EVAL$ld_cn.cal.RAB = ld_calib$cost_norm.calRAB
rd_EVAL$rd_cn.cal.RAB = rd_calib$cost_norm.calRAB

#Calculating total costs
#Local distribution
RevCap$ld_sum.cost = RevCap$fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_ld_cens*y.cb.cpi.factor + RevCap$ld_dep.sf +
                        RevCap$ld_nl*RevCap$pnl.rc
#Regional distribution
RevCap$rd_sum.cost = RevCap$fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cens*y.cb.cpi.factor + RevCap$rd_dep.sf +
                        RevCap$rd_nl*RevCap$pnl.rc
#Transmission grid
RevCap$t_sum.cost = RevCap$fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_t_cens*y.cb.cpi.factor + RevCap$t_dep.sf

# Sum of all relevant costs for all grid levels
RevCap$lrt_sum.cost = RevCap$ld_sum.cost + RevCap$rd_sum.cost + RevCap$t_sum.cost

#Cost base used in Revenue cap calculation
RevCap$ld_cost.RC = RevCap$fp_ld_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_ld_cens*y.cb.cpi.factor + RevCap$ld_dep.sf +
                        RevCap$ld_rab.sf*NVE.ir.RC + RevCap$ld_nl*RevCap$pnl.rc


RevCap$rd_cost.RC = RevCap$fp_rd_OPEX*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cga*y.cb.cpi.l.factor +
                        RevCap$fp_rd_cens*y.cb.cpi.factor + RevCap$rd_dep.sf +
                        RevCap$rd_rab.sf*NVE.ir.RC + RevCap$rd_nl*RevCap$pnl.rc

RevCap$t_cost.RC = RevCap$fp_t_OPEX*y.cb.cpi.l.factor + 
                        RevCap$fp_t_cens*y.cb.cpi.factor + RevCap$t_dep.sf +
                        RevCap$t_rab.sf*NVE.ir.RC

RevCap$lrt_cost.RC =  RevCap$ld_cost.RC + RevCap$rd_cost.RC + RevCap$t_cost.RC

# Actual inflicted costs in all grid levels, excluding network losses in regional distribution
# as well as grid assetsments costs in regional distributioin
RevCap$lrt_cost.RC.ex.nlR.og.cgaR = round(RevCap$lrt_cost.RC - RevCap$fp_rd_cga, digits = 0)

# Inflicted costs in cost base year
#What is correct way to calculate total inflicted cost in CB.Y, new or "2012"-version

if (y.rc >= 2018) {
RevCap$lrt_cost.cby = (RevCap$fp_ld_OPEX + RevCap$fp_ld_cga + RevCap$fp_ld_cens) +
                        (RevCap$fp_rd_OPEX + RevCap$fp_rd_cga + RevCap$fp_rd_cens) +
                        (RevCap$fp_t_OPEX + RevCap$fp_t_cens) +
                        (RevCap$ld_nl + RevCap$rd_nl)*RevCap$ap.t_2
}else{
RevCap$lrt_cost.cby = (RevCap$ld_opex_2012 - RevCap$fp_ld_391 + RevCap$fp_ld_cens + RevCap$ld_nl*RevCap$ap.t_2) +
                           (RevCap$rd_opex_2012 - RevCap$fp_rd_391 + RevCap$fp_rd_cens + RevCap$rd_nl*RevCap$ap.t_2) +
                           (RevCap$t_opex_2012 - RevCap$fp_t_391 + RevCap$fp_t_cens)
}        




## Cost norms
ld_cncR = cbind(ld_EVAL[,c("id", "ld_cn.cal.RAB")])
rd_cncR = cbind(rd_EVAL[,c("id", "rd_cn.cal.RAB")])

RevCap  = dplyr::full_join(RevCap, ld_cncR, by="id")
RevCap  = dplyr::full_join(RevCap, rd_cncR, by="id")
rm(ld_cncR, rd_cncR)

#

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


# Import cost norms for companies set to average efficiency
RevCap$ld_cn.cal.RAB[is.na(RevCap$ld_cn.cal.RAB)] = 
        ld_AV.EFF$ld_cn.cal.RAB[match(RevCap$id[is.na(RevCap$ld_cn.cal.RAB)], ld_AV.EFF$id)]

RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
        rd_AV.EFF$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)], rd_AV.EFF$id)]

# Cost norm equals 0 for all companies without OPEX in given grid-level

RevCap = within(RevCap, rd_cn.cal.RAB[is.na(rd_cn.cal.RAB)==TRUE & fp_rd_OPEX == 0] <- 0)
RevCap = within(RevCap, ld_cn.cal.RAB[is.na(ld_cn.cal.RAB)==TRUE & fp_ld_OPEX == 0] <- 0)


#Drop observations if in no.rc-grous
RevCap = RevCap[!RevCap$id %in% ld_no.rc,]
#RevCap = RevCap[!is.na(RevCap$rd_cn.cal.RAB),]

# Cost norms prior to re-calibration
# See descripton of re-calibration belowe
RevCap$ld_cn.pre.recal = RevCap$ld_cn.cal.RAB + (RevCap$ld_gci.cost*y.cb.cpi.factor)
RevCap$rd_cn.pre.recal = RevCap$rd_cn.cal.RAB + (RevCap$rd_nl*RevCap$pnl.rc) +
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

# Sum of total cost of inflicted costs in cost base year, rounded for consistency with calculations i Stata (Improve ? - remove round?)
lrt_TOTAL.cost.y.cb = round(sum(RevCap$lrt_cost.cby), digits = 0)

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
