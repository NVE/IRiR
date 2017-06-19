#### 0.3 Company selection -  defines data frames for different models ####

# Groups defined in 0.1 are used to create new data frames

# Local distribution
dat$ld_EVAL = 0
dat$ld_EVAL <- ifelse(dat$ld_TOTXDEA > 0, dat$ld_EVAL <- 1, dat$ld_EVAL <- 0)


for (i in which(dat$id %in% ld_av.eff)){
        dat[i,"ld_EVAL"]  = 0 
}

for (i in which(dat$id %in% ld_ooto)){  
        dat[i,"ld_EVAL"]  = 0   
} 

for (i in which(dat$id %in% ld_no.rc)){  
        dat[i,"ld_EVAL"]  = 0   
} 

for (i in which(dat$y %in% y.avg)){
        ld_EVAL <-dat[dat$ld_EVAL==1,]
}
#Creating vector for companies in normal evaluation, local distribution
ld_eval <- as.numeric(na.omit(unique(subset(ld_EVAL$id, !(ld_EVAL$id %in% ld_sep.eval | ld_EVAL$id %in%  ld_ooto | ld_EVAL$id %in% ld_av.eff)))))
#Creating dataframe containing only observations for companies, normal evaluation
ld_EVAL <- subset.data.frame(ld_EVAL, !is.na(y) & y==y.cb)
#Vector conataining only ids from ld_EVAL-frame, in same order used for DEA 
ld_DEA_id <- ld_EVAL$id



# Regional distribution
dat$rd_EVAL = 0  
dat$rd_EVAL <- ifelse(dat$rd_TOTXDEA > 0, dat$rd_EVAL <- 1, dat$rd_EVAL <- 0) 

for (i in which(dat$id %in% rd_av.eff)){  
        dat[i,"rd_EVAL"]  = 0  
}

for (i in which(dat$id %in% rd_ooto)){  
        dat[i,"rd_EVAL"]  = 0   
}  

rd_EVAL <- dat[dat$rd_EVAL == 1,]


#Vector containing only ids of companies in normal evalution
rd_eval <- as.numeric(na.omit(unique(subset(rd_EVAL$id, !(rd_EVAL$id %in% rd_sep.eval | rd_EVAL$id %in%  rd_ooto | rd_EVAL$id %in% rd_av.eff)))))
# Creating dataframe with all companies included i normal evaluation, and companies only allowed to be peers for themselves
rd_EVAL <- subset.data.frame(rd_EVAL, !is.na(y) & y==y.cb)
#Vector containing IDs of all companies in DEA, used for datawrangling
rd_DEA_id <- rd_EVAL$id


