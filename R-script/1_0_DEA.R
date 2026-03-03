
#### 1.0 Calculating DEA scores - Stage 1 ####

# Selecting inputs and outputs to be used in DEA ####
        # Local distribution
        # X - input in DEA: TOTX
        # Y - Outputs in DEA: subscribers, km high voltage lines/cables, substations
        X.avg.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("fha_ld_TOTXDEA")]  
        Y.avg.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("fha_ld_sub","fha_ld_hv","fha_ld_ss")]  
        X.cb.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,"ld_TOTXDEA"]
        Y.cb.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("ld_sub","ld_hv","ld_ss")]
        names(X.cb.ld) = ld_eval
        rownames(Y.cb.ld) = ld_eval
        names(X.avg.ld) = ld_eval
        rownames(Y.avg.ld) = ld_eval

        # Regional distribution
        # X - input in DEA: TOTX
        # Y - Outputs in DEA: weighted walues of grid components
        X.avg.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,"fha_rd_TOTXDEA"]  
        Y.avg.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,c("fha_rd_wv.ol","fha_rd_wv.uc","fha_rd_wv.sc","fha_rd_wv.ss")]  
        X.cb.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,"rd_TOTXDEA"]
        Y.cb.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,c("rd_wv.ol","rd_wv.uc","rd_wv.sc","rd_wv.ss")]
        names(X.avg.rd) = rd_DEA_id
        rownames(Y.avg.rd) = rd_DEA_id
        names(X.cb.rd) = rd_DEA_id
        rownames(Y.cb.rd) = rd_DEA_id

        # Data used in DEA are rounded to closest integer, i.e. thousands
        X.avg.ld <- round(X.avg.ld, digits = 0)
        Y.avg.ld  <- round(Y.avg.ld, digits = 0)
        X.cb.ld <- round(X.cb.ld, digits = 0)
        Y.cb.ld  <- round(Y.cb.ld, digits = 0)
        X.avg.rd <- round(X.avg.rd, digits = 0)
        Y.avg.rd  <- round(Y.avg.rd, digits = 0)
        X.cb.rd <- round(X.cb.rd, digits = 0)
        Y.cb.rd  <- round(Y.cb.rd, digits = 0)

# LD - DEA calculations ####
        write.csv(cbind(ld_EVAL$id, X.avg.ld, Y.avg.ld,X.cb.ld, Y.cb.ld), file = "./Results/ld_InputDEA.csv")

        # DEA with cost base year observations and peers determined by average data
        dea.cb.avg.ld = dea(X=X.cb.ld, Y=Y.cb.ld, XREF=X.avg.ld[as.character(ld_eval)], YREF=Y.avg.ld[as.character(ld_eval),], RTS="crs")
        ld_EVAL = data.frame(cbind(ld_EVAL, dea.cb.avg.ld$eff))
        colnames(ld_EVAL)[colnames(ld_EVAL)=="dea.cb.avg.ld.eff"] <- "ld_eff.s1.cb"
        
        # DEA with averaged observations and peers determined by average data
        dea.avg.avg.ld = dea(X=X.avg.ld, Y=Y.avg.ld, XREF=X.avg.ld[as.character(ld_eval)], YREF=Y.avg.ld[as.character(ld_eval),], RTS="crs")
        ld_EVAL = data.frame(cbind(ld_EVAL, dea.avg.avg.ld$eff))
        colnames(ld_EVAL)[colnames(ld_EVAL)=="dea.avg.avg.ld.eff"] <- "ld_eff.s1.avg"

        # Preserving lambdas / cost norm shares for peers
        ld_lambda = dea.cb.avg.ld$lambda
        colnames(ld_lambda) = c(ld_eval)
        ld_lambda = ld_lambda[,order(as.numeric(colnames(ld_lambda)))]

        ld_lambda.avg = dea.avg.avg.ld$lambda
        colnames(ld_lambda.avg) = c(ld_eval)
        ld_lambda.avg = ld_lambda.avg[,order(as.numeric(colnames(ld_lambda.avg)))]

        ld_lambda[is.na(ld_lambda)] <- 0
        ld_lambda.avg[is.na(ld_lambda.avg)] <- 0

# RD - DEA calculations ####
        write.csv(cbind(rd_EVAL$id, X.avg.rd, Y.avg.rd, X.cb.rd, Y.cb.rd), file = "./Results/rd_InputDEA.csv")

        # DEA with cost base year observations and peers determined by average data
        dea.cb.avg.rd = dea(X=X.cb.rd, Y=Y.cb.rd, XREF=X.avg.rd[as.character(rd_eval)], YREF=Y.avg.rd[as.character(rd_eval),], RTS="crs")
        rd_EVAL = data.frame(cbind(rd_EVAL, dea.cb.avg.rd$eff))
        colnames(rd_EVAL)[colnames(rd_EVAL)=="dea.cb.avg.rd.eff"] <- "rd_eff.s1.cb"

        # DEA with averaged observations and peers determined by average data
        dea.avg.avg.rd = dea(X=X.avg.rd, Y=Y.avg.rd, XREF=X.avg.rd[as.character(rd_eval)], YREF=Y.avg.rd[as.character(rd_eval),], RTS="crs")
        rd_EVAL = data.frame(cbind(rd_EVAL, dea.avg.avg.rd$eff))
        colnames(rd_EVAL)[colnames(rd_EVAL)=="dea.avg.avg.rd.eff"] <- "rd_eff.s1.avg"

        # Adapted calculation for companies in sep.eval, DEA with cost base year observations and peers determined by average data
        eff.cb.avg.rd = dea.cb.avg.rd$eff
        rd_lambda = cbind(dea.cb.avg.rd$lambda,matrix(NA,nrow=nrow(dea.cb.avg.rd$lambda),ncol=length(rd_sep.eval)))
        colnames(rd_lambda) = c(rd_eval, rd_sep.eval)
        for(i in rd_sep.eval)
        {
                dea.sep.cb.avg.rd = dea(X=X.cb.rd,Y=Y.cb.rd,RTS="crs",XREF=X.avg.rd[as.character(c(rd_eval,i))],YREF=Y.avg.rd[as.character(c(rd_eval,i)),])
                eff.cb.avg.rd[as.character(i)] = dea.sep.cb.avg.rd$eff[as.character(i)] 
                for(j in c(rd_eval,i))
                        rd_lambda[as.character(i),as.character(j)] = dea.sep.cb.avg.rd$lambda[as.character(i),paste("L_",as.character(j),sep="")]
        }
        rd_EVAL$rd_eff.s1.cb = eff.cb.avg.rd
        rd_lambda = rd_lambda[,order(as.numeric(colnames(rd_lambda)))]

        # Adapted calculation for companies in sep.eval, DEA with averaged observations and peers determined by average data
        eff.avg.avg.rd = dea.avg.avg.rd$eff
        rd_lambda.avg = cbind(dea.avg.avg.rd$lambda,matrix(NA,nrow=nrow(dea.avg.avg.rd$lambda),ncol=length(rd_sep.eval)))
        colnames(rd_lambda.avg) = c(rd_eval,rd_sep.eval)
        for(i in rd_sep.eval)
        {
                dea.sep.avg.avg.rd = dea(X=X.avg.rd,Y=Y.avg.rd,RTS="crs",XREF=X.avg.rd[as.character(c(rd_eval,i))],YREF=Y.avg.rd[as.character(c(rd_eval,i)),])
                eff.avg.avg.rd[as.character(i)] = dea.sep.avg.avg.rd$eff[as.character(i)]
                for(j in c(rd_eval,i))
                        rd_lambda.avg[as.character(i),as.character(j)] = dea.sep.avg.avg.rd$lambda[as.character(i),paste("L_",as.character(j),sep="")]
        }
        rd_EVAL$rd_eff.s1.avg = eff.avg.avg.rd
        rd_lambda.avg = rd_lambda.avg[,order(as.numeric(colnames(rd_lambda.avg)))]
        
        rd_lambda[is.na(rd_lambda)] <- 0
        rd_lambda.avg[is.na(rd_lambda.avg)] <- 0
        
        