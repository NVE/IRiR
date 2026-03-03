
#### Special treatment - OOTO model for companies out of the ordinary ####

# Local distribution ####
        dat$ld_OOTO = 0 # Create dummy variable for OOTO model
        
        for (i in which(dat$id %in% ld_ooto)){  
                dat[i,"ld_OOTO"]  = 1   
        } # Assign dummy variable value for all companies in OOTO
        
        for (i in which(dat$y == y.cb)){
                ld_OOTO <-dat[dat$ld_OOTO==1 & dat$y == y.cb,]
        } # Create data frame for companies in OOTO model

# Prices from REN catalogue
        ld_sub.NOK = 12
        ld_hv.NOK = 419
        ld_ss.NOK = 147

# Rounding values for outputs and input
        ld_OOTO$fha_ld_sub     <- round(ld_OOTO$fha_ld_sub, digits = 0)  
        ld_OOTO$fha_ld_ss      <- round(ld_OOTO$fha_ld_ss, digits = 0)  
        ld_OOTO$fha_ld_hv      <- round(ld_OOTO$fha_ld_hv, digits = 0) 
        ld_OOTO$fha_ld_TOTXDEA <- round(ld_OOTO$fha_ld_TOTXDEA, digits = 0) 

# Calculating total outputs
        ld_OOTO$ld_output = ld_OOTO$ld_sub*ld_sub.NOK + ld_OOTO$ld_hv*ld_hv.NOK + ld_OOTO$ld_ss*ld_ss.NOK                 # Outputs, cost base year
        ld_OOTO$fha_ld_output = ld_OOTO$fha_ld_sub*ld_sub.NOK + ld_OOTO$fha_ld_hv*ld_hv.NOK + ld_OOTO$fha_ld_ss*ld_ss.NOK # Outputs, five year historical averages

# Calculate output per TOTXDEA
        ld_OOTO$ld_output.NOK = ld_OOTO$ld_output/ld_OOTO$ld_TOTXDEA
        ld_OOTO$fha_ld_output.NOK = ld_OOTO$fha_ld_output/ld_OOTO$fha_ld_TOTXDEA
 
# Calculating efficiency score for companies in OOTO-model
        ld_OOTO$ld_eff.OOTO = ld_OOTO$ld_output.NOK/ld_OOTO$fha_ld_output.NOK

# Cost base 
        ld_OOTO$ld_cb <- ((ld_OOTO$fp_ld_OPEX*y.cb.cpi.l.factor) + (ld_OOTO$ld_rab.sf*NVE.ir.t) + 
                        ld_OOTO$ld_dep.sf + (ld_OOTO$ld_cens*y.cb.cpi.factor) + (ld_OOTO$ld_nl*ld_OOTO$pnl.rc) )

# Cost norm
        ld_OOTO$ld_cn.cal.RAB = ld_OOTO$ld_cb*ld_OOTO$ld_eff.OOTO

# Regional distribution ####
        dat$rd_OOTO = 0 # Create dummy variable for OOTO model

        for (i in which(dat$id %in% rd_ooto)){  
                dat[i,"rd_OOTO"]  = 1   
        } # Assign dummy variable value for all companies in OOTO
        
        for (i in which(dat$y %in% y.avg)){
                rd_OOTO <-dat[dat$rd_OOTO==1 & dat$y==y.cb,]
        } # Create data frame for companies in OOTO model

# Rounding values for outputs and input
        rd_OOTO$fha_rd_wv.ol   <- round(rd_OOTO$fha_rd_wv.ol, digits = 0)  
        rd_OOTO$fha_rd_wv.uc   <- round(rd_OOTO$fha_rd_wv.uc, digits = 0)  
        rd_OOTO$fha_rd_wv.sc   <- round(rd_OOTO$fha_rd_wv.sc, digits = 0)
        rd_OOTO$fha_rd_wv.ss   <- round(rd_OOTO$fha_rd_wv.ss, digits = 0)
        rd_OOTO$fha_rd_TOTXDEA <- round(rd_OOTO$fha_rd_TOTXDEA, digits = 0)

# Calculating total outputs
        rd_OOTO$rd_output = rd_OOTO$rd_wv.ol + rd_OOTO$rd_wv.uc + rd_OOTO$rd_wv.sc + rd_OOTO$rd_wv.ss                     # Outputs, cost base year
        rd_OOTO$fha_rd_output = rd_OOTO$fha_rd_wv.ol + rd_OOTO$fha_rd_wv.uc + rd_OOTO$fha_rd_wv.sc + rd_OOTO$fha_rd_wv.ss # Outputs, five year historical averages

# Calculating output pr NOK
        rd_OOTO$rd_output.NOK = rd_OOTO$rd_output/rd_OOTO$rd_TOTXDEA
        rd_OOTO$fha_rd_output.NOK = rd_OOTO$fha_rd_output/rd_OOTO$fha_rd_TOTXDEA

# Calculating efficiency score for companies in OOTO-model
        rd_OOTO$rd_eff.OOTO = rd_OOTO$rd_output.NOK/rd_OOTO$fha_rd_output.NOK

# Cost base
        rd_OOTO$rd_cb <- ((rd_OOTO$fp_rd_OPEX*y.cb.cpi.l.factor) + (rd_OOTO$rd_rab.sf*NVE.ir.t) + 
                          rd_OOTO$rd_dep.sf + (rd_OOTO$rd_cens*y.cb.cpi.factor))
# Cost norm
        rd_OOTO$rd_cn.cal.RAB = rd_OOTO$rd_cb * rd_OOTO$rd_eff.OOTO
        
        