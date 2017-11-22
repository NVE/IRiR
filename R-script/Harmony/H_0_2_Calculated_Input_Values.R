#### 0.2 Calculates Input Values for DEA ####


## Variable for costs associated with assests
## in interface between local and regional distribution

dat$ld_gci.dummy = 1
for (i in which(dat$id %in% ld_av.eff | dat$id %in% ld_ooto)){
        dat[i, "ld_gci.dummy"] = 0 
}

# Priser inn grensesnittvariabelen
dat$ld_gci.cost = 0
dat$ld_gci.cost = ifelse(dat$ld_gci.dummy == 1, dat$ld_gci * gci.p, 0)

#### CPI ####
# CPI adjustment
v_cpi = c("ld_gci.cost", "ld_cens", "rd_cens", "t_cens",
          "ld_dep.gf", "ld_bv.gf", "ld_dep.sf", "ld_bv.sf",
          "rd_dep.gf", "rd_bv.gf", "rd_dep.sf", "rd_bv.sf",
          "t_dep.sf", "t_bv.sf")
fp_v_cpi = paste("fp_", v_cpi, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_cpi), dimnames = list(v_cpi = fp_v_cpi))))
for(c in 1:length(v_cpi))
        for(r in 1:nrow(dat))
                dat[r, fp_v_cpi[c]] = dat[r, v_cpi[c]] * cpi[as.character(y.cb)] / cpi[as.character(dat[r, "y"])]
#Changed syntax for implemented pension costs from 2015.



#### CPI.L ####
# CPI.L adjustment
# Adjusting variables with CPI by delivery sector, services where labor dominates Statistics Norwat table 11118 A/O 2017
# https://www.ssb.no/en/priser-og-prisindekser/statistikker/kpi
v_cpi.l = c("ld_391", "rd_391", "t_391", "ld_pens", "rd_pens", "t_pens", "ld_pens.eq", "rd_pens.eq", "t_pens.eq", 
            "ld_impl", "rd_impl", "t_impl", "ld_OPEXxS", "ld_cga", "rd_OPEXxS", "rd_cga", "t_OPEXxS", "ld_sal", 
            "ld_sal.cap", "rd_sal", "rd_sal.cap", "t_sal", "t_sal.cap")
fp_v_cpi.l = paste("fp_", v_cpi.l, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_cpi.l), dimnames = list(v_cpi.l = fp_v_cpi.l))))
for(c in 1:length(v_cpi.l))
        for(r in 1:nrow(dat))
                dat[r, fp_v_cpi.l[c]] = dat[r, v_cpi.l[c]] * cpi.l[as.character(y.cb)] / cpi.l[as.character(dat[r, "y"])]



#### Pension costs ####
# Historical averages 2007-2013, smoother revenue caps over time
# See NVE public hearing 4/2015, p.24 NOR, https://www.nve.no/media/2567/hoeringsdokument2015_04-tariffer-ved-sammenslaaing.pdf

v_hist = c("fp_ld_pens", "fp_ld_pens.eq", "fp_ld_impl", "fp_rd_pens", "fp_rd_pens.eq", "fp_rd_impl", "fp_t_pens", "fp_t_pens.eq", "fp_t_impl")
names_hist = c("ld_pens", "ld_pens.eq", "ld_impl", "rd_pens", "rd_pens.eq", "rd_impl", "t_pens", "t_pens.eq", "t_impl")
hist = paste("hist_", names_hist, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_hist), dimnames = list(v_hist = hist))))
for(c in 1:length(v_hist))
        for(r in 1:nrow(dat)){
                if (dat[r, "y"] <= 2013)
                        dat[r, hist[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% y.hist.pen,v_hist[c]], na.rm = T)
                if (dat[r, "y"] > 2013)
                        dat[r, hist[c]] = dat[r, v_hist[c]]
        }

# Five year historical averages of pension costs
v_av.pen = c("hist_ld_pens", "hist_ld_pens.eq", "hist_ld_impl", "hist_rd_pens", "hist_rd_pens.eq", "hist_rd_impl", "hist_t_pens", "hist_t_pens.eq", "hist_t_impl", "ld_gci")
names_avg = c("ld_pens", "ld_pens.eq", "ld_impl", "rd_pens", "rd_pens.eq", "rd_impl", "t_pens", "t_pens.eq", "t_impl", "ld_gci")
avg = paste("av_", names_avg,sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_av.pen), dimnames = list(v_av.pen = avg))))
for(c in 1:length(v_av.pen))
        for(r in 1:nrow(dat))
                if (dat[r,"y"] %in% y.avg)
                        dat[r, avg[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% y.avg,v_av.pen[c]], na.rm = T)

# Pension cost base, included in OPEX
dat$fp_ld_pcb = dat$av_ld_pens + dat$av_ld_pens.eq + dat$av_ld_impl
dat$fp_rd_pcb = dat$av_rd_pens + dat$av_rd_pens.eq + dat$av_rd_impl
dat$fp_t_pcb = dat$av_t_pens + dat$av_t_pens.eq + dat$av_t_impl

#Also need mean of network losses


#### Cost base ####
# Compute TOTXDEA local distribution
dat$fp_ld_opex     = dat$fp_ld_OPEXxS + dat$fp_ld_sal - dat$fp_ld_sal.cap + dat$fp_ld_pcb
dat$fp_ld_OPEX     = dat$fp_ld_opex - dat$fp_ld_391 - dat$fp_ld_cga
dat$fp_ld_rab.sf       = dat$fp_ld_bv.sf * wcp
dat$fp_ld_rab.gf     = dat$fp_ld_bv.gf * wcp
dat$fp_ld_RAB       = dat$fp_ld_rab.sf + dat$fp_ld_rab.gf
dat$fp_ld_DEP       = dat$fp_ld_dep.sf + dat$fp_ld_dep.gf
dat$ld_nl.NOK  = dat$ld_nl * pnl.dea
dat$ld_TOTXDEA   = dat$fp_ld_OPEX + (dat$fp_ld_RAB * ir.dea) + dat$fp_ld_DEP + dat$fp_ld_cens + dat$ld_nl.NOK - dat$fp_ld_gci.cost

# Compute TOTXDEA for regional distribution
#network losses not included in benchmarked costs
dat$fp_rd_opex     = dat$fp_rd_OPEXxS + dat$fp_rd_sal - dat$fp_rd_sal.cap + dat$fp_rd_pcb
dat$fp_rd_OPEX     = dat$fp_rd_opex - dat$fp_rd_391 - dat$fp_rd_cga
dat$fp_rd_rab.sf       = dat$fp_rd_bv.sf * wcp
dat$fp_rd_rab.gf     = dat$fp_rd_bv.gf * wcp
dat$fp_rd_RAB       = dat$fp_rd_rab.sf + dat$fp_rd_rab.gf
dat$fp_rd_DEP       = dat$fp_rd_dep.sf + dat$fp_rd_dep.gf
dat$rd_TOTXDEA   = dat$fp_rd_OPEX + (dat$fp_rd_RAB * ir.dea) + dat$fp_rd_DEP + dat$fp_rd_cens

# Compute TOTXDEA for transmission grid
dat$fp_t_opex     = dat$fp_t_OPEXxS + dat$fp_t_sal - dat$fp_t_sal.cap + dat$fp_t_pcb
dat$fp_t_OPEX    = dat$fp_t_opex - dat$fp_t_391
dat$fp_t_rab.sf       = dat$fp_t_bv.sf * wcp 
dat$t_TOTXDEA   = dat$fp_t_OPEX + (dat$fp_t_rab.sf * ir.dea) + dat$fp_t_dep.sf + dat$fp_t_cens


#Peers determined by five year historical average data
# Discribed in NVE report 7/2012, p. 37 NOR http://publikasjoner.nve.no/rapport/2012/rapport2012_71.pdf
# Purpose: Stabilize front by smoothing cost variation within last five year.
# Increases comparability as all companies are compared against historical average, including peers.

v_fha = c("ld_TOTXDEA", "ld_sub", "ld_hv", "ld_ss",
          "rd_TOTXDEA", "rd_wv.ol", "rd_wv.uc", "rd_wv.sc", "rd_wv.ss",
          "fp_ld_OPEX", "fp_ld_rab.sf", "fp_ld_dep.sf", "fp_ld_cens",
          "fp_ld_391", "fp_ld_cga", "ld_nl",
          "fp_ld_gci.cost", 
          "fp_rd_OPEX", "fp_rd_rab.sf", "fp_rd_dep.sf", "fp_rd_cens",
          "fp_rd_391", "fp_rd_cga", "rd_nl",
          "fp_t_OPEX", "fp_t_rab.sf", "fp_t_dep.sf", "fp_t_cens")
fha = paste("fha_", v_fha, sep="")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_fha), dimnames = list(v_fha = fha))))
for(c in 1:length(v_fha))
        for(r in 1:nrow(dat))
                if (dat[r,"y"] %in% y.avg)
                        dat[r, fha[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% y.avg, v_fha[c]], na.rm = T)



