#### Spec - OOTO model for companies Out Of The Ordinary ####
# 
# Companies exempted from normal evaluation
# These companies are compared to their own historical performance, cost in y.cb vs five year historical average cost in y.avg
# Criterias in local distribution: Number of subscribers (ld_sub) < 500 or kilometers of high voltage grid (ld_hv) < 100
# Criterias in regional distribution/transmission: Sum of all cost weights (rd_vw.ol + rd_wv.uc + rd_wv.sc + rd_wv.ss) < 4000
# or 0 km high voltage grid in regional distribution (rd_vw.ol = 0)
# 
# Cost norm for companies in OOTO-model (.._cn.cal.RAB) indicates that these values are included
# in calibration. Contradictory to this, these companies are not included in the
# calibration and is done to "gather the pieces" in the RC calculation


## Local distribution ------

#Create dummy variable for OOTO model
dat$ld_OOTO = 0

#Asign dummy variable value for all companies in ld_ooto
for (i in which(dat$id %in% ld_ooto)){  
        dat[i,"ld_OOTO"]  = 1   
} 

#Create data frame for companies in OOTO moidel
for (i in which(dat$y == y.cb)){
        ld_OOTO <-dat[dat$ld_OOTO==1 & dat$y == y.cb,]
}

# Prices from REN catalogue. opko funnet i notat fra 2009 (Improve - ?)
ld_sub.NOK = 12
ld_hv.NOK = 419
ld_ss.NOK = 147

#Rounding values for outputs and input (costs)
ld_OOTO$fha_ld_sub  <- round(ld_OOTO$fha_ld_sub, digits = 0)  
ld_OOTO$fha_ld_ss <- round(ld_OOTO$fha_ld_ss, digits = 0)  
ld_OOTO$fha_ld_hv <- round(ld_OOTO$fha_ld_hv, digits = 0) 
ld_OOTO$fha_ld_TOTXDEA <- round(ld_OOTO$fha_ld_TOTXDEA, digits = 0) 

# Output in local distribution grid
#Cost base year
ld_OOTO$ld_output = ld_OOTO$ld_sub*ld_sub.NOK + ld_OOTO$ld_hv*ld_hv.NOK + ld_OOTO$ld_ss*ld_ss.NOK
#Average values
ld_OOTO$fha_ld_output = ld_OOTO$fha_ld_sub*ld_sub.NOK + ld_OOTO$fha_ld_hv*ld_hv.NOK + ld_OOTO$fha_ld_ss*ld_ss.NOK

#Calculate output pr NOK
ld_OOTO$ld_output.NOK = ld_OOTO$ld_output/ld_OOTO$ld_TOTXDEA
ld_OOTO$fha_ld_output.NOK = ld_OOTO$fha_ld_output/ld_OOTO$fha_ld_TOTXDEA

#Calculates efficiency score for companies in OOTO-model
ld_OOTO$ld_eff.OOTO = ld_OOTO$ld_output.NOK/ld_OOTO$fha_ld_output.NOK

#Cost base 
ld_OOTO$ld_cb <- ((ld_OOTO$fp_ld_OPEX*y.cb.cpi.l.factor) + (ld_OOTO$ld_rab.sf*NVE.ir.RC) + 
                          ld_OOTO$ld_dep.sf + (ld_OOTO$ld_cens*y.cb.cpi.factor) + 
                          (ld_OOTO$ld_nl*pnl.rc) - (ld_OOTO$ld_gci.cost*y.cb.cpi.factor))


# Cost norm
ld_OOTO$ld_cn.cal.RAB = ld_OOTO$ld_cb*ld_OOTO$ld_eff.OOTO


## Regional distribution ----
dat$rd_OOTO = 0


for (i in which(dat$id %in% rd_ooto)){  
        dat[i,"rd_OOTO"]  = 1   
} 

#Further special treatment of Drangedal Everk KF (35), Rauma Energi AS (162) & Roros Elektrisietsverk AS (173) ----
rd_OOTO.spes = (c(35, 162, 173))
# Femårige snittverdier (totalkostnad og outputs)
v_fha_rd_OOTO = c("rd_TOTXDEA", "rd_wv.ol", "rd_wv.uc", "rd_wv.sc", "rd_wv.ss")
fha = paste("fha_", v_fha, sep="")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(v_fha), dimnames = list(v_fha = fha))))
for(c in 1:length(v_fha))
        for(r in 1:nrow(dat))
                if (dat[r,"y"] %in% 2011:2014 & dat[r,"id"] %in% rd_OOTO.spes)
                        dat[r, fha[c]] = mean(dat[dat$orgn == dat$orgn[r] & dat$y %in% 2011:2014, v_fha[c]], na.rm = T)
#Normal OOTO continues RD------------------------------------------------------------------------------------------


for (i in which(dat$y %in% y.avg)){
        rd_OOTO <-dat[dat$rd_OOTO==1 & dat$y==y.cb,]
}



#Rounding values for outputs and input (costs)
rd_OOTO$fha_rd_wv.ol  <- round(rd_OOTO$fha_rd_wv.ol, digits = 0)  
rd_OOTO$fha_rd_wv.uc <- round(rd_OOTO$fha_rd_wv.uc, digits = 0)  
rd_OOTO$fha_rd_wv.sc <- round(rd_OOTO$fha_rd_wv.sc, digits = 0)
rd_OOTO$fha_rd_wv.ss <- round(rd_OOTO$fha_rd_wv.ss, digits = 0)
rd_OOTO$fha_rd_TOTXDEA <- round(rd_OOTO$fha_rd_TOTXDEA, digits = 0)

# Output in regional distribution grid
# Cost base year
rd_OOTO$rd_output = rd_OOTO$rd_wv.ol + rd_OOTO$rd_wv.uc + rd_OOTO$rd_wv.sc + rd_OOTO$rd_wv.ss 
# Average years
rd_OOTO$fha_rd_output = rd_OOTO$fha_rd_wv.ol + rd_OOTO$fha_rd_wv.uc + rd_OOTO$fha_rd_wv.sc + rd_OOTO$fha_rd_wv.ss

#Calculate output pr NOK
rd_OOTO$rd_output.NOK = rd_OOTO$rd_output/rd_OOTO$rd_TOTXDEA
rd_OOTO$fha_rd_output.NOK = rd_OOTO$fha_rd_output/rd_OOTO$fha_rd_TOTXDEA

#Calculates efficiency score for companies in OOTO-model
rd_OOTO$rd_eff.OOTO = rd_OOTO$rd_output.NOK/rd_OOTO$fha_rd_output.NOK

# Cost base
rd_OOTO$rd_cb <- ((rd_OOTO$fp_rd_OPEX*y.cb.cpi.l.factor) + (rd_OOTO$rd_rab.sf*NVE.ir.RC) + 
                          rd_OOTO$rd_dep.sf + (rd_OOTO$rd_cens*y.cb.cpi.factor))
# Cost norm
rd_OOTO$rd_cn.cal.RAB = rd_OOTO$rd_cb * rd_OOTO$rd_eff.OOTO
