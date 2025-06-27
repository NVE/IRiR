
#### 0_3 Calculating input values for DEA ####

#### Inflating costs to y.cb with CPI ####
# CPI adjustment
        v_cpi = c("ld_cens", "rd_cens", "t_cens")
        fp_v_cpi = paste("fp_", v_cpi, sep = "")
        dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_cpi), dimnames = list(v_cpi = fp_v_cpi))))
        for(c in 1:length(v_cpi))
                for(r in 1:nrow(dat))
                        dat[r, fp_v_cpi[c]] = dat[r, v_cpi[c]] * cpi[as.character(y.cb)] / cpi[as.character(dat[r, "y"])]

# CPI.L adjustment
        v_cpi.l = c("ld_391", "rd_391", "t_391", "ld_pens", "rd_pens", "t_pens", "ld_pens.eq", "rd_pens.eq", "t_pens.eq", 
                    "ld_impl", "rd_impl", "t_impl", "ld_OPEXxS", "rd_OPEXxS", "rd_cga", "t_OPEXxS", "ld_sal", 
                    "ld_sal.cap", "rd_sal", "rd_sal.cap", "t_sal", "t_sal.cap")
        fp_v_cpi.l = paste("fp_", v_cpi.l, sep = "")
        dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_cpi.l), dimnames = list(v_cpi.l = fp_v_cpi.l))))
        for(c in 1:length(v_cpi.l))
                for(r in 1:nrow(dat))
                        dat[r, fp_v_cpi.l[c]] = dat[r, v_cpi.l[c]] * cpi.l[as.character(y.cb)] / cpi.l[as.character(dat[r, "y"])]

# Pension costs, five year historical averages
        v_av.pen = c("fp_ld_pens", "fp_ld_pens.eq", "fp_ld_impl", "fp_rd_pens", "fp_rd_pens.eq", "fp_rd_impl", "fp_t_pens", 
                     "fp_t_pens.eq", "fp_t_impl")
        names_avg = c("ld_pens", "ld_pens.eq", "ld_impl", "rd_pens", "rd_pens.eq", "rd_impl", "t_pens", "t_pens.eq", "t_impl")
        avg = paste("av_", names_avg,sep = "")
        dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_av.pen), dimnames = list(v_av.pen = avg))))
        for(c in 1:length(v_av.pen))
                for(r in 1:nrow(dat))
                        if (dat[r,"y"] %in% y.avg)
                                dat[r, avg[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% y.avg,v_av.pen[c]], na.rm = T)

# Pension cost base, to be included in OPEX
        dat$fp_ld_pcb = dat$av_ld_pens + dat$av_ld_pens.eq + dat$av_ld_impl
        dat$fp_rd_pcb = dat$av_rd_pens + dat$av_rd_pens.eq + dat$av_rd_impl
        dat$fp_t_pcb  = dat$av_t_pens + dat$av_t_pens.eq + dat$av_t_impl

#### Cost base ####
# Compute TOTXDEA, local distribution
        dat$fp_ld_OPEX  = dat$fp_ld_OPEXxS + dat$fp_ld_sal - dat$fp_ld_sal.cap + dat$fp_ld_pcb - dat$fp_ld_391
        dat$ld_rab.sf   = dat$ld_bv.sf * wcp
        dat$ld_rab.gf   = dat$ld_bv.gf * wcp
        dat$ld_RAB      = dat$ld_rab.sf + dat$ld_rab.gf
        dat$ld_DEP      = dat$ld_dep.sf + dat$ld_dep.gf
        dat$ld_nl.NOK   = dat$ld_nl * sysp.t_2 
        dat$ld_TOTXDEA  = dat$fp_ld_OPEX + (dat$ld_RAB * NVE.ir.t_2) + dat$ld_DEP + dat$fp_ld_cens + dat$ld_nl.NOK 

# Compute TOTXDEA, regional distribution (network losses not included)
        dat$fp_rd_OPEX  = dat$fp_rd_OPEXxS + dat$fp_rd_sal - dat$fp_rd_sal.cap + dat$fp_rd_pcb- dat$fp_rd_391 - dat$fp_rd_cga
        dat$rd_rab.sf   = dat$rd_bv.sf * wcp
        dat$rd_rab.gf   = dat$rd_bv.gf * wcp
        dat$rd_RAB      = dat$rd_rab.sf + dat$rd_rab.gf
        dat$rd_DEP      = dat$rd_dep.sf + dat$rd_dep.gf
        dat$rd_TOTXDEA  = dat$fp_rd_OPEX + (dat$rd_RAB * NVE.ir.t_2) + dat$rd_DEP + dat$fp_rd_cens

# Compute TOTXDEA for transmission grid
        dat$fp_t_OPEX   = dat$fp_t_OPEXxS + dat$fp_t_sal - dat$fp_t_sal.cap + dat$fp_t_pcb - dat$fp_t_391
        dat$t_rab.sf    = dat$t_bv.sf * wcp
        
#### Five year historical averages of X and Y variables used in DEA ####
        v_fha = c("ld_TOTXDEA", "ld_sub", "ld_hv", "ld_ss", "rd_TOTXDEA", "rd_wv.ol", "rd_wv.uc", "rd_wv.sc", "rd_wv.ss")
        fha = paste("fha_", v_fha, sep="")
        dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_fha), dimnames = list(v_fha = fha))))
        for(c in 1:length(v_fha))
                for(r in 1:nrow(dat))
                        if (dat[r,"y"] %in% y.avg)
                                dat[r, fha[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% y.avg, v_fha[c]], na.rm = T)
        