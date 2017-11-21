#### 3.0 Calibration stage - Stage 3 ####

# Stage 3 ensures that the industry in total recovers all cost, and that the averagely efficient firm has a return equal to
# the NVE interest. Calibration of DEA-results is perfomed such that the difference between total cost norm and total cost
# is distributed based on each companies share of the regulatory asset base of self-funded assets.
#
# For calculation specifics see nve_functions.R, NVE_cal

#Calculate cost base (cb) as input in calibration - local distribution
ld_EVAL$fha_ld_cb <- ((ld_EVAL$fha_fp_ld_OPEX*y.cb.cpi.l.factor) + ((ld_EVAL$fha_fp_ld_rab.sf*y.cb.cpi.factor)*NVE.ir.RC) + 
                                     (ld_EVAL$fha_fp_ld_dep.sf*y.cb.cpi.factor) + (ld_EVAL$fha_fp_ld_cens*y.cb.cpi.factor) + 
                                     (ld_EVAL$fha_ld_nl*pnl.rc) - (ld_EVAL$fha_fp_ld_gci.cost*y.cb.cpi.factor))

ld_calib=NVE_cal(eff=ld_EVAL$ld_eff.s2.avg, cost_base=ld_EVAL$fha_ld_cb, RAB=ld_EVAL$fha_fp_ld_rab.sf)



#Calculate cost base (cb) as input in calibration - regional distribution
rd_EVAL$fha_rd_cb <- ((rd_EVAL$fha_fp_rd_OPEX*y.cb.cpi.l.factor) + ((rd_EVAL$fha_fp_rd_rab.sf*y.cb.cpi.factor)*NVE.ir.RC) + 
                                     (rd_EVAL$rd_dep.sf*y.cb.cpi.factor) + (rd_EVAL$fha_fp_rd_cens*y.cb.cpi.factor))

rd_calib=NVE_cal(eff=rd_EVAL$rd_eff.s2.avg, cost_base = rd_EVAL$fha_rd_cb, RAB = rd_EVAL$fha_fp_rd_rab.sf)


