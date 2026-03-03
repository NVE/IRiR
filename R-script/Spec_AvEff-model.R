
#### Special treatment - companies set to average efficiency ####
# Condition: incomplete data or structural brakes in data

# Local distribution
        dat$ld_AV.EFF = 0 # Create dummy variable

        for (i in which(dat$id %in% ld_av.eff & dat$y==y.cb)){  
                dat[i,"ld_AV.EFF"]  = 1   
        } # Set dummy variable value to 1 for companies in vector ld_av.eff

        for (i in which(dat$y %in% y.cb)){
                ld_AV.EFF <-dat[dat$ld_AV.EFF==1,]
        } # Create data frame of companies with ld_AV.EFF = 1

        ld_AV.EFF$ld_cb <- ((ld_AV.EFF$fp_ld_OPEX*y.cb.cpi.l.factor) + (ld_AV.EFF$ld_rab.sf*NVE.ir.t) + 
                        ld_AV.EFF$ld_dep.sf + (ld_AV.EFF$ld_cens*y.cb.cpi.factor) + (ld_AV.EFF$ld_nl*ld_AV.EFF$pnl.rc))

        ld_AV.EFF$ld_cn.cal.RAB = ld_AV.EFF$ld_cb # Cost norm set equal to cost base

# Regional distribution
        dat$rd_AV.EFF = 0 # Create dummy variable
        
        for (i in which(dat$id %in% rd_av.eff & dat$y== y.cb)){  
                dat[i,"rd_AV.EFF"]  = 1   
        } # Set dummy variable value to 1 for companies in vector rd_av.eff
        
        for (i in which(dat$y %in% y.cb)){
                rd_AV.EFF <-dat[dat$rd_AV.EFF==1,]
        } # Create data frame of companies with ld_AV.EFF = 1
        
        rd_AV.EFF$rd_cb <- ((rd_AV.EFF$fp_rd_OPEX*y.cb.cpi.l.factor) + (rd_AV.EFF$rd_rab.sf*NVE.ir.t) + 
                        rd_AV.EFF$rd_dep.sf + (rd_AV.EFF$rd_cens*y.cb.cpi.factor))
        
        rd_AV.EFF$rd_cn.cal.RAB = rd_AV.EFF$rd_cb # Cost norm set equal to cost base
        