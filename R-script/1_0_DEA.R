#### 1.0 Calculates  ####

## Five year historical averages are used for defining the efficient frontier

# Local distribution
# X - input in DEA, average TOTEX last five years
# Y - Outputs in DEA, average subscribers, km hv lines/cables, substations in local dist. grid
X.avg.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("fha_ld_TOTXDEA")]  
Y.avg.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("fha_ld_sub","fha_ld_hv","fha_ld_ss")]  
#Add rownames to DEA-data 
names(X.avg.ld) = ld_DEA_id
rownames(Y.avg.ld) = ld_DEA_id
#--------------------------------------------------------------------------

# Regional distribution
# X - input in DEA, average TOTEX last five years
# Y - Outputs in DEA, averages of weighted walues of regional grid components
# Weights calculated for overhead lines, underground cables, sea cables and substations.
# See NOR https://www.nve.no/elmarkedstilsynet-marked-og-monopol/okonomisk-regulering-av-nettselskap/aktuelle-prosjekter/mer-treffsikre-kostnadsnormer-for-nettselskaper


X.avg.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,"fha_rd_TOTXDEA"]  
Y.avg.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,c("fha_rd_wv.ol","fha_rd_wv.uc","fha_rd_wv.sc","fha_rd_wv.ss")]  
#Add rownames to DEA-data
names(X.avg.rd) = rd_DEA_id
rownames(Y.avg.rd) = rd_DEA_id


# Cost base data for companies in DEA
#Local distribution
X.cb.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,"ld_TOTXDEA"]
Y.cb.ld = dat[dat$orgn %in% ld_EVAL$orgn & dat$y == y.cb,c("ld_sub","ld_hv","ld_ss")]
#Add rownames to DEA-data
names(X.cb.ld) = ld_DEA_id
rownames(Y.cb.ld) = ld_DEA_id

# R-nett
X.cb.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,"rd_TOTXDEA"]
Y.cb.rd = dat[dat$orgn %in% rd_EVAL$orgn & dat$y == y.cb,c("rd_wv.ol","rd_wv.uc","rd_wv.sc","rd_wv.ss")]

#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(X.cb.rd) = rd_DEA_id
rownames(Y.cb.rd) = rd_DEA_id

#All data used in DEA are rounded to closest integer, i.e. thousands

X.avg.ld <- round(X.avg.ld, digits = 0)
Y.avg.ld$fha_ld_sub  <- round(Y.avg.ld$fha_ld_sub, digits = 0)
Y.avg.ld$fha_ld_ss <- round(Y.avg.ld$fha_ld_ss, digits = 0)
Y.avg.ld$fha_ld_hv <- round(Y.avg.ld$fha_ld_hv, digits = 0)


X.cb.ld <- round(X.cb.ld, digits = 0)
Y.cb.ld$ld_sub  <- round(Y.cb.ld$ld_sub, digits = 0)
Y.cb.ld$ld_ss <- round(Y.cb.ld$ld_ss, digits = 0)
Y.cb.ld$ld_hv <- round(Y.cb.ld$ld_hv, digits = 0)



X.avg.rd <- round(X.avg.rd, digits = 0)
Y.avg.rd$fha_rd_wv.ol  <- round(Y.avg.rd$fha_rd_wv.ol, digits = 0)
Y.avg.rd$fha_rd_wv.uc <- round(Y.avg.rd$fha_rd_wv.uc, digits = 0)
Y.avg.rd$fha_rd_wv.sc <- round(Y.avg.rd$fha_rd_wv.sc, digits = 0)
Y.avg.rd$fha_rd_wv.ss <- round(Y.avg.rd$fha_rd_wv.ss, digits = 0)


X.cb.rd <- round(X.cb.rd, digits = 0)
Y.cb.rd$rd_wv.ol  <- round(Y.cb.rd$rd_wv.ol, digits = 0)
Y.cb.rd$rd_wv.uc <- round(Y.cb.rd$rd_wv.uc, digits = 0)
Y.cb.rd$rd_wv.sc <- round(Y.cb.rd$rd_wv.sc, digits = 0)
Y.cb.rd$rd_wv.ss <- round(Y.cb.rd$rd_wv.ss, digits = 0)

#### DEA D-nett ####


### DEA input
write.csv(cbind(ld_EVAL$id, X.avg.ld, Y.avg.ld,X.cb.ld, Y.cb.ld), file = "./Results/ld_InputDEA.csv")
#Companies that can only be their own peers
ld_sep.eval

#### Actual calculations of efficiency scores using
# dea command from Benchmarking package by P. Bogetoft & L. Otto
# https://cran.r-project.org/web/packages/Benchmarking/Benchmarking.pdf


#Local distribution grid
# Cost base year observations, peers determined by average data
dea.cb.avg.ld = dea(X=X.cb.ld, Y=Y.cb.ld, XREF=X.avg.ld[as.character(ld_eval)], YREF=Y.avg.ld[as.character(ld_eval),], RTS="crs")
plot(sort(dea.cb.avg.ld$eff))
#View(cbind(X.cb.ld, Y.avg.ld, dea.cb.avg.ld$eff)[order(dea.cb.avg.ld$eff)])
ld_EVAL = data.frame(cbind(ld_EVAL, dea.cb.avg.ld$eff))
colnames(ld_EVAL)[colnames(ld_EVAL)=="dea.cb.avg.ld.eff"] <- "ld_eff.s1.cb"

#Calculating effeciency, only including average observations
dea.avg.avg.ld = dea(X=X.avg.ld, Y=Y.avg.ld, XREF=X.avg.ld[as.character(ld_eval)], YREF=Y.avg.ld[as.character(ld_eval),], RTS="crs")
plot(sort(dea.avg.avg.ld$eff))
ld_EVAL = data.frame(cbind(ld_EVAL, dea.avg.avg.ld$eff))
colnames(ld_EVAL)[colnames(ld_EVAL)=="dea.avg.avg.ld.eff"] <- "ld_eff.s1.avg"



#Adapted calculation for companies only allowed to be their own peers, cost base data
eff.cb.avg.ld = dea.cb.avg.ld$eff

ld_lambda = cbind(dea.cb.avg.ld$lambda,matrix(NA,nrow=nrow(dea.cb.avg.ld$lambda),ncol=length(ld_sep.eval)))
colnames(ld_lambda) = c(ld_eval,ld_sep.eval)
for(i in ld_sep.eval)
{
        dea.sep.cb.avg.ld = dea(X=X.cb.ld,Y=Y.cb.ld,RTS="crs",XREF=X.avg.ld[as.character(c(ld_eval,i))],YREF=Y.avg.ld[as.character(c(ld_eval,i)),])
        eff.cb.avg.ld[as.character(i)] = dea.sep.cb.avg.ld$eff[as.character(i)] 
        for(j in c(ld_eval,i))
                ld_lambda[as.character(i),as.character(j)] = dea.sep.cb.avg.ld$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}

ld_lambda = ld_lambda[,order(as.numeric(colnames(ld_lambda)))]


#Adapted calculation for companies only allowed to be their own peers, average data only
eff.avg.avg.ld = dea.avg.avg.ld$eff

ld_lambda.avg = cbind(dea.avg.avg.ld$lambda,matrix(NA,nrow=nrow(dea.avg.avg.ld$lambda),ncol=length(ld_sep.eval)))
colnames(ld_lambda.avg) = c(ld_eval,ld_sep.eval)
for(i in ld_sep.eval)
{
        dea.sep.avg.avg.ld = dea(X=X.avg.ld,Y=Y.avg.ld,RTS="crs",XREF=X.avg.ld[as.character(c(ld_eval,i))],YREF=Y.avg.ld[as.character(c(ld_eval,i)),])
        eff.avg.avg.ld[as.character(i)] = dea.sep.avg.avg.ld$eff[as.character(i)]
        for(j in c(ld_eval,i))
                ld_lambda.avg[as.character(i),as.character(j)] = dea.sep.avg.avg.ld$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}


ld_lambda.avg = ld_lambda.avg[,order(as.numeric(colnames(ld_lambda.avg)))]

##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
ld_lambda[is.na(ld_lambda)] <- 0
ld_lambda.avg[is.na(ld_lambda.avg)] <- 0


#### DEA R-nett ####

### DEA input
write.csv(cbind(rd_EVAL$id, X.avg.rd, Y.avg.rd, X.cb.rd, Y.cb.rd), file = "./Resultater/r_InputDEA.csv")

#selskapene som kun kan danne front for seg selv er disse
r_separat_dmuer

#id for alle selskapene som ikke er spesialmodell

# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i X.avg.ld og Y.avg.ld, deascore beregnes som 
# årets observasjoner av kostnader (X.cb.ld) og oppgaver Y.cb.ld
#D-nett
dea.faktisk.snitt.r = dea(X=X.cb.rd, Y=Y.cb.rd, XREF=X.avg.rd[as.character(r_normal)], YREF=Y.avg.rd[as.character(r_normal),], RTS="crs")
plot(sort(dea.faktisk.snitt.r$eff))
#View(cbind(X.cb.ld, Y.avg.ld, dea.cb.avg.ld$eff)[order(dea.cb.avg.ld$eff)])
rd_EVAL = data.frame(cbind(rd_EVAL, dea.faktisk.snitt.r$eff))
#Endrer navn på variabelen som merges inn
colnames(rd_EVAL)[colnames(rd_EVAL)=="dea.faktisk.snitt.r.eff"] <- "r_f_sf_eff"

#Beregner ren snitt front
dea.snitt.snitt.r = dea(X=X.avg.rd, Y=Y.avg.rd, XREF=X.avg.rd[as.character(r_normal)], YREF=Y.avg.rd[as.character(r_normal),], RTS="crs")
plot(sort(dea.snitt.snitt.r$eff))
rd_EVAL = data.frame(cbind(rd_EVAL, dea.snitt.snitt.r$eff))
#Endrer navn på variabelen som merges inn
colnames(rd_EVAL)[colnames(rd_EVAL)=="dea.snitt.snitt.r.eff"] <- "r_sf_eff"


#spesialkjøring for selskaper som bare kan være front for seg selv
eff.faktisk.snitt.r = dea.faktisk.snitt.r$eff

r_lambda = cbind(dea.faktisk.snitt.r$lambda,matrix(NA,nrow=nrow(dea.faktisk.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(r_lambda) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.faktisk.snitt.r = dea(X=X.cb.rd,Y=Y.cb.rd,RTS="crs",XREF=X.avg.rd[as.character(c(r_normal,i))],YREF=Y.avg.rd[as.character(c(r_normal,i)),])
        eff.faktisk.snitt.r[as.character(i)] = dea.sep.faktisk.snitt.r$eff[as.character(i)] 
        for(j in c(r_normal,i))
                r_lambda[as.character(i),as.character(j)] = dea.sep.faktisk.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}

r_lambda = r_lambda[,order(as.numeric(colnames(r_lambda)))]


#spesialkjøring for selskaper som bare kan være front for seg selv
eff.snitt.snitt.r = dea.snitt.snitt.r$eff

r_lambda.snitt = cbind(dea.snitt.snitt.r$lambda,matrix(NA,nrow=nrow(dea.snitt.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(r_lambda.snitt) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.snitt.snitt.r = dea(X=X.avg.rd,Y=Y.avg.rd,RTS="crs",XREF=X.avg.rd[as.character(c(r_normal,i))],YREF=Y.avg.rd[as.character(c(r_normal,i)),])
        eff.snitt.snitt.r[as.character(i)] = dea.sep.snitt.snitt.r$eff[as.character(i)]
        for(j in c(r_normal,i))
                r_lambda.snitt[as.character(i),as.character(j)] = dea.sep.snitt.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}


r_lambda.snitt = r_lambda.snitt[,order(as.numeric(colnames(r_lambda.snitt)))]


##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
r_lambda[is.na(r_lambda)] <- 0
r_lambda.snitt[is.na(r_lambda.snitt)] <- 0
