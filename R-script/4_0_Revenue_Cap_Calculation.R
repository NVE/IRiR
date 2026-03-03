
#### 4_0 Revenue cap calculation ####

# Removing companies with no rc in Rnett
dat$fp_rd_OPEX[dat$y == y.cb & dat$id %in% rd_no.rc] <- 0

# Creating new data frame for revenue cap calculation ####
RevCap = subset.data.frame(dat, subset = y == y.cb,
                           select=c(comp, id, orgn, y, id.y, fp_ld_OPEX, fp_rd_OPEX, fp_t_OPEX,
                                    fp_ld_cens, fp_rd_cens, fp_t_cens, ld_rab.sf, rd_rab.sf, t_rab.sf,
                                    ld_dep.sf, rd_dep.sf, t_dep.sf, fp_ld_391,fp_rd_391, fp_t_391,
                                    fp_rd_cga.tidl.coord, ld_nl, rd_nl, ap.t_2, pnl.rc))


# Calculating total costs, not including return on capital (sum.costs) ####
RevCap$ld_sum.cost = RevCap$fp_ld_OPEX * y.cb.cpi.l.factor + RevCap$fp_ld_cens * y.cb.cpi.factor + 
  RevCap$ld_dep.sf + RevCap$ld_nl * RevCap$pnl.rc

RevCap$rd_sum.cost = RevCap$fp_rd_OPEX * y.cb.cpi.l.factor + RevCap$fp_rd_cga.tidl.coord * y.cb.cpi.l.factor +
  RevCap$fp_rd_cens * y.cb.cpi.factor + RevCap$rd_dep.sf + RevCap$rd_nl * RevCap$pnl.rc

RevCap$t_sum.cost = RevCap$fp_t_OPEX * y.cb.cpi.l.factor + RevCap$fp_t_cens * y.cb.cpi.factor + RevCap$t_dep.sf

RevCap$lrt_sum.cost = RevCap$ld_sum.cost + RevCap$rd_sum.cost + RevCap$t_sum.cost

# Calculating cost base used in RC calculation (costs.RC) ####
RevCap$ld_cost.RC = RevCap$fp_ld_OPEX * y.cb.cpi.l.factor + RevCap$fp_ld_cens * y.cb.cpi.factor + 
  RevCap$ld_dep.sf + RevCap$ld_rab.sf * NVE.ir.t + RevCap$ld_nl * RevCap$pnl.rc

RevCap$rd_cost.RC = RevCap$fp_rd_OPEX * y.cb.cpi.l.factor + RevCap$fp_rd_cga.tidl.coord * y.cb.cpi.l.factor +
  RevCap$fp_rd_cens * y.cb.cpi.factor + RevCap$rd_dep.sf +
  RevCap$rd_rab.sf * NVE.ir.t + RevCap$rd_nl * RevCap$pnl.rc

RevCap$t_cost.RC = RevCap$fp_t_OPEX * y.cb.cpi.l.factor + RevCap$fp_t_cens * y.cb.cpi.factor + 
  RevCap$t_dep.sf + RevCap$t_rab.sf * NVE.ir.t

RevCap$lrt_cost.RC = RevCap$ld_cost.RC + RevCap$rd_cost.RC + RevCap$t_cost.RC

# Actual inflicted costs in all grid levels, excluding network losses and grid assessments costs in regional distribution ####
RevCap$lrt_cost.RC.ex.nl.cga = round(RevCap$lrt_cost.RC - RevCap$rd_nl * RevCap$pnl.rc - RevCap$fp_rd_cga.tidl.coord * y.cb.cpi.l.factor, digits = 0) 

# Inflicted costs in cost base year ####
RevCap$lrt_cost.cby = (RevCap$fp_ld_OPEX  + RevCap$fp_ld_cens) + (RevCap$fp_rd_OPEX + RevCap$fp_rd_cga.tidl.coord + RevCap$fp_rd_cens) +
  (RevCap$fp_t_OPEX + RevCap$fp_t_cens) + (RevCap$ld_nl + RevCap$rd_nl) * RevCap$ap.t_2
lrt_TOTAL.cost.y.cb = round(sum(RevCap$lrt_cost.cby), digits = 0)

# Adding cost norms to data frame ####
ld_cncR = cbind(ld_EVAL[,c("id", "ld_cn.cal.RAB")])
rd_cncR = cbind(rd_EVAL[,c("id", "rd_cn.cal.RAB")])
RevCap  = dplyr::full_join(RevCap, ld_cncR, by="id")
RevCap  = dplyr::full_join(RevCap, rd_cncR, by="id")

ld_OOTO.cncR = cbind(ld_OOTO[,c("id", "ld_cn.cal.RAB")])
RevCap$ld_cn.cal.RAB[is.na(RevCap$ld_cn.cal.RAB)] = 
  ld_OOTO.cncR$ld_cn.cal.RAB[match(RevCap$id[is.na(RevCap$ld_cn.cal.RAB)],ld_OOTO.cncR$id)]

rd_OOTO.cncR = cbind(rd_OOTO[,c("id", "rd_cn.cal.RAB")])
RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
  rd_OOTO.cncR$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)],rd_OOTO.cncR$id)]

rm(ld_cncR, rd_cncR, ld_OOTO.cncR, rd_OOTO.cncR)

RevCap$ld_cn.cal.RAB[is.na(RevCap$ld_cn.cal.RAB)] = 
  ld_AV.EFF$ld_cn.cal.RAB[match(RevCap$id[is.na(RevCap$ld_cn.cal.RAB)], ld_AV.EFF$id)]

RevCap$rd_cn.cal.RAB[is.na(RevCap$rd_cn.cal.RAB)] = 
  rd_AV.EFF$rd_cn.cal.RAB[match(RevCap$id[is.na(RevCap$rd_cn.cal.RAB)], rd_AV.EFF$id)]

# Setting cost norms equal to 0 for all companies without OPEX in given grid level ####
RevCap = within(RevCap, rd_cn.cal.RAB[is.na(rd_cn.cal.RAB)==TRUE & fp_rd_OPEX == 0] <- 0)
RevCap = within(RevCap, ld_cn.cal.RAB[is.na(ld_cn.cal.RAB)==TRUE & fp_ld_OPEX == 0] <- 0)

# Cost norms prior to re-calibration ####
RevCap$ld_cn.pre.recal  = RevCap$ld_cn.cal.RAB 
RevCap$rd_cn.pre.recal  = RevCap$rd_cn.cal.RAB + (RevCap$rd_nl * RevCap$pnl.rc) + (RevCap$fp_rd_cga.tidl.coord * y.cb.cpi.l.factor)
RevCap$t_cn.pre.recal   = RevCap$t_cost.RC # Cost coverage for transmission
RevCap$lrt_cn.pre.recal = RevCap$ld_cn.pre.recal + RevCap$rd_cn.pre.recal + RevCap$t_cn.pre.recal

# Revenue caps prior to re-calibration ####
RevCap$lrt_RC.pre.recal   = (1-rho) * RevCap$lrt_cost.RC + rho * (RevCap$lrt_cn.pre.recal)

# Re-calibrating revenue caps ####

# Re-calibration factor 1 - difference between total RC and total costs over total RAB
lrt_TOTAL.RC.pre.recal = sum(RevCap$lrt_RC.pre.recal)   # Sum of all revenue caps before re-calibration
lrt_TOTAL.Cost.pre.recal = sum(RevCap$lrt_cost.RC)      # Sum of all costs before re-calibration
RevCap$lrt_RAB = RevCap$ld_rab.sf + RevCap$rd_rab.sf + RevCap$t_rab.sf
lrt_TOTRAB = sum(RevCap$lrt_RAB)                        # Total Regulatory Asset Base
lrt_recal.fact1 = (lrt_TOTAL.RC.pre.recal - lrt_TOTAL.Cost.pre.recal) / lrt_TOTRAB

# Re-calibration factor 2 - difference between actual costs and cost base used in RC over total RAB
lrt_recal.TOT     = lrt_RC_dec.y.cb - lrt_TOTAL.cost.y.cb
lrt_recal.TOT.int = lrt_recal.TOT * (1 + NVE.ir[as.character(y.cb)]) * (1 + NVE.ir[as.character(y.cb + 1)]) - lrt_recal.TOT
lrt_recal.fact2   = lrt_recal.TOT.int / lrt_TOTRAB

# Cost norms after re-calibration
RevCap$lrt_cn.pos.recal = RevCap$lrt_cn.pre.recal - ((RevCap$lrt_RAB * (lrt_recal.fact1 + lrt_recal.fact2)) / rho)

# Revenue caps after re-calibration
RevCap$lrt_RC.pos.recal = (1 - rho) * RevCap$lrt_cost.RC + rho * RevCap$lrt_cn.pos.recal
lrt_TOTAL.RC.pos.recal = sum(RevCap$lrt_RC.pos.recal)

# Printing RevCap to Excel                
write.xlsx(RevCap, file = paste0(run_dir,"/RevCap.xlsx"), overwrite = T)

        