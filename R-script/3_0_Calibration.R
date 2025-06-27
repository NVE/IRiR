
#### 3_0 Calibrating cost norms - Stage 3 ####

# Calculating cost base (cb) as input in calibration
# Local distribution
ld_EVAL$ld_cb <- ((ld_EVAL$fp_ld_OPEX*y.cb.cpi.l.factor) + (ld_EVAL$ld_rab.sf*NVE.ir.t) +
                ld_EVAL$ld_dep.sf + (ld_EVAL$ld_cens*y.cb.cpi.factor) +
                (ld_EVAL$ld_nl*ld_EVAL$pnl.rc))

# Regional distribution
rd_EVAL$rd_cb <- ((rd_EVAL$fp_rd_OPEX*y.cb.cpi.l.factor) + (rd_EVAL$rd_rab.sf*NVE.ir.t) + 
                rd_EVAL$rd_dep.sf + (rd_EVAL$rd_cens*y.cb.cpi.factor))

# Calibrating cost norms
# Local distribution
ld_calib = NVE_cal(eff=ld_EVAL$ld_eff.s2.cb, cost_base=ld_EVAL$ld_cb, RAB=ld_EVAL$ld_RAB)
ld_EVAL$ld_cn.cal.RAB = ld_calib$cost_norm.calRAB
ld_EVAL$ld_eff.s3.cb = ld_calib$eff.cal

# Regional distribution
rd_calib = NVE_cal(eff=rd_EVAL$rd_eff.s2.cb, cost_base = rd_EVAL$rd_cb, RAB =rd_EVAL$rd_RAB)
rd_EVAL$rd_cn.cal.RAB = rd_calib$cost_norm.calRAB
rd_EVAL$rd_eff.s3.cb = rd_calib$eff.cal