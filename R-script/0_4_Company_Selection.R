
#### 0_4 Company selection ####

# LD - DSOs with no revenue cap - no.rc ####
        ld_no.rc <- (c())
        dat$ld_no.rc.auto <- ifelse(dat$ld_rab.sf==0 & dat$ld_dep.sf==0 & dat$ld_nl==0 & dat$ld_OPEXxS==0 & dat$y==y.cb, 1, 0)
        ld_no.rc.auto <- dat[(dat$ld_no.rc.auto>0),"id"]
        ld_no.rc.auto <- as.numeric(ld_no.rc.auto)
        ld_no.rc <- unique(append(ld_no.rc.auto, ld_no.rc))
        print_ld_no.rc = filter(dat, y == y.cb, (dat$id %in% ld_no.rc))
        print_ld_no.rc[,c("comp", "id", "ld_RAB", "ld_DEP", "ld_nl", "ld_OPEXxS")]
        
# LD - DSOs set to average efficiency - av.eff ####
        ld_av.eff <- (c(753))
        ld_av.eff_candidates = filter(dat, dat$ld_ss == 0, dat$ld_sub == 0,
                                      dat$ld_hv == 0, dat$ld_TOTXDEA == 0,
                                      !(dat$id %in% ld_no.rc))
        ld_av.eff_candidates[, c("comp", "id")]
        ld_av.eff.auto <- ld_av.eff_candidates[, c("id")]
        ld_av.eff <- unique(append(ld_av.eff.auto, ld_av.eff))
       

# LD - DSOs with less than 500 subscribers - OOTO ####
        ld_ooto <- (c()) 
        ld_ooto_candidates = filter(dat, y == y.cb, 
                                    dat$ld_sub < 500,
                                    !(dat$id %in% ld_no.rc  | 
                                              dat$id %in% ld_av.eff))
        ld_ooto_candidates[, c("comp", "id", "ld_sub")]
        ld_ooto.auto <- ld_ooto_candidates[, c("id")]
        ld_ooto <- unique(append(ld_ooto.auto, ld_ooto))

# RD - DSOs with no revenue cap - no.rc ####
        rd_no.rc  <- (c())  
        dat$rd_no.rc.auto <- ifelse(dat$rd_RAB==0 & dat$rd_DEP==0 & dat$rd_nl==0 & dat$rd_OPEXxS==0 & dat$y==y.cb, 1, 0)
        rd_no.rc.auto <- dat[(dat$rd_no.rc.auto>0),"id"]
        rd_no.rc.auto <- as.numeric(rd_no.rc.auto)
        rd_no.rc <- unique(append(rd_no.rc.auto, rd_no.rc))
        print_rd_no.rc = filter(dat, y == y.cb, (dat$id %in% rd_no.rc))
        print_rd_no.rc[,c("comp", "id", "rd_RAB", "rd_DEP", "rd_nl", "rd_OPEXxS")]
        
# RD - DSOs set to average efficiency - av.eff ####
        rd_av.eff <- (c(135))  
        dat$rd_wv.sum = dat$rd_wv.ol + dat$rd_wv.uc + dat$rd_wv.sc + dat$rd_wv.ss
        rd_av.eff_candidates = filter(dat, dat$rd_wv.sum == 0 | dat$rd_rab.sf == 0,
                                      !(dat$id  %in% rd_no.rc ))
        # rd_av.eff_candidates[, c("comp", "id", "rd_wv.sum", "rd_rab.sf", "rd_TOTXDEA")]
        rd_av.eff.auto <- rd_av.eff_candidates[, c("id")]
        rd_av.eff <- unique(append(rd_av.eff.auto, rd_av.eff))
        print_rd_av.eff = filter(dat, (dat$id %in% rd_av.eff))
        print_rd_av.eff[,c("comp", "id", "y", "rd_rab.sf", "rd_TOTXDEA", "rd_OPEXxS", "rd_wv.sum")]
        rd_av.eff = rd_av.eff[!rd_av.eff == ""] # Fyll inn ID'er for selskaper som evt. ikke skal med i av.eff

# RD - DSOs with less than 4000 weighted values or no overhead lines - OOTO ####
        rd_ooto <- (c())
        rd_ooto_candidates = filter(dat, y == y.cb, rd_wv.sum < 4000 | dat$rd_wv.ol == 0 , 
                                    !(dat$id %in% rd_av.eff |
                                              dat$id %in% rd_no.rc))
        rd_ooto_candidates[, c("comp", "id", "rd_wv.sum",  "rd_wv.ol", "rd_wv.sc", "rd_wv.ss", "rd_wv.uc")]
        rd_ooto.auto <- rd_ooto_candidates[, c("id")]
        rd_ooto <-  (unique(append(rd_ooto.auto, rd_ooto)))

# T  - DSOs with no revenue cap - no.rc ####
        dat$t_no.rc <- ifelse(dat$t_cens==0 & dat$t_OPEXxS==0 & dat$t_dep.sf==0 & dat$t_rab.sf==0 & dat$y==y.cb, 1, 0)
        t_no.rc <- dat[(dat$t_no.rc>0), "id"]
        t_no.rc <- as.numeric(t_no.rc)
        # print_t_no.rc = filter(dat, y == y.cb, (dat$id %in% t_no.rc))
        # print_t_no.rc[,c("comp", "id", "t_rab.sf", "t_dep.sf", "t_OPEXxS")]      
        dat$fp_t_OPEX   = ifelse(dat$id %in% t_no.rc & dat$y==y.cb, 0, dat$fp_t_OPEXxS + dat$fp_t_sal - dat$fp_t_sal.cap + dat$fp_t_pcb - dat$fp_t_391)
        
# RD - DSOs not allowed to become peers - sep.eval ####
        # Identifying candidates to rd_sep.eval
        rd_sep.eval = filter(dat, y == y.cb, fha_rd_TOTXDEA < 50000, 
                                        !(dat$id %in% rd_ooto | dat$id %in% rd_av.eff |
                                          dat$rd_wv.sum == 0 | dat$id %in% rd_no.rc))
        rd_sep.eval <- rd_sep.eval[,c("id", "comp", "fha_rd_TOTXDEA", "rd_wv.sum")]
        rd_sep.eval <- rd_sep.eval[, c("id")]

# Creating data frames to be used in DEA based on company selection ####
        
        # Creating data frame for local distribution
        dat$ld_EVAL = 0
        dat$ld_EVAL = ifelse(dat$ld_TOTXDEA > 0, dat$ld_EVAL <- 1, dat$ld_EVAL <- 0)
        for (i in which(dat$id %in% ld_av.eff)){
                dat[i,"ld_EVAL"]  = 0 
        }
        for (i in which(dat$id %in% ld_ooto)){  
                dat[i,"ld_EVAL"]  = 0   
        } 
        for (i in which(dat$id %in% ld_no.rc)){  
                dat[i,"ld_EVAL"]  = 0   
        } 
        
        ld_EVAL   = dat[dat$ld_EVAL==1 & dat$y==y.cb,] 
        ld_eval = ld_EVAL$id                          

        # Creating data frame for regional distribution
        dat$rd_EVAL = 0  
        dat$rd_EVAL = ifelse(dat$rd_TOTXDEA > 0, dat$rd_EVAL <- 1, dat$rd_EVAL <- 0) 
        for (i in which(dat$id %in% rd_av.eff)){  
                dat[i,"rd_EVAL"]  = 0  
        }
        for (i in which(dat$id %in% rd_ooto)){  
                dat[i,"rd_EVAL"]  = 0   
        }  
        for (i in which(dat$id %in% rd_no.rc)){  
                dat[i,"rd_EVAL"]  = 0   
        } 
        
        rd_EVAL   = dat[dat$rd_EVAL == 1 & dat$y==y.cb,] 
        rd_DEA_id = rd_EVAL$id                           
        rd_eval   = as.numeric(na.omit(unique(subset(rd_EVAL$id, !(rd_EVAL$id %in% rd_sep.eval | rd_EVAL$id %in%  rd_ooto | 
                                                                   rd_EVAL$id %in% rd_av.eff| rd_EVAL$id %in% rd_no.rc)))))
        
        rm(ld_av.eff_candidates, ld_ooto_candidates, rd_av.eff_candidates, rd_ooto_candidates,
           ld_av.eff.auto, rd_av.eff.auto, ld_ooto.auto, rd_ooto.auto)
        