#### DEA D-nett ####

#### Trinn 1 - DEA-kjøringer ####

#Runder av data til DEA til "hele tusen"  
        #Disse får definere teknologien (fronten) i hovedkjøringen
x.snitt.d <- round(x.snitt.d, digits = 0)  
y.snitt.d$sf_d_ab  <- round(y.snitt.d$sf_d_ab, digits = 0)  
y.snitt.d$sf_d_ns <- round(y.snitt.d$sf_d_ns, digits = 0)  
y.snitt.d$sf_d_hs <- round(y.snitt.d$sf_d_hs, digits = 0)

#Disse måles mot fronten og gir gjeldende DEA-score
x.faktisk.d <- round(x.faktisk.d, digits = 0) 
y.faktisk.d$d_ab  <- round(y.faktisk.d$d_ab, digits = 0)  
y.faktisk.d$d_ns <- round(y.faktisk.d$d_ns, digits = 0)  
y.faktisk.d$d_hs <- round(y.faktisk.d$d_hs, digits = 0) 

### DEA input
write.csv(cbind(d_tilDEA$id, x.snitt.d, y.snitt.d,x.faktisk.d, y.faktisk.d), file = "./Resultater/d_InputDEA.csv")


# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.d og y.snitt.d, deascore beregnes som 
# årets observasjoner av kostnader (x.faktisk.d) og oppgaver y.faktisk.d
        #D-nett
dea.faktisk.snitt.d = dea(X=x.faktisk.d, Y=y.faktisk.d, XREF=x.snitt.d, YREF=y.snitt.d, RTS="crs")
plot(sort(dea.faktisk.snitt.d$eff))
#View(cbind(x.faktisk.d, y.snitt.d, dea.faktisk.snitt.d$eff)[order(dea.faktisk.snitt.d$eff)])
d_tilDEA = data.frame(cbind(d_tilDEA, dea.faktisk.snitt.d$eff))
#Endrer navn på variabelen som merges inn
colnames(d_tilDEA)[colnames(d_tilDEA)=="dea.faktisk.snitt.d.eff"] <- "d_f_sf_eff"

write.csv(cbind(d_tilDEA$id,dea.faktisk.snitt.d$eff), file = "./Resultater/DEAeff1.csv")
write.csv(d_tilDEA, file = "./Resultater/d_DEAResultat_Data.csv")


#Beregner kostbidrag
#1. Henter vekter fra DEA-beregning
d_kostbidrag = data.frame(dea.faktisk.snitt.d$lambda)
#2. Transponerer matrisen slik at den kan ganges direkte ut med TOTEX
d_kostbidrag = data.frame(t(d_kostbidrag))
#3. Multipliserer df med snitt-TOTEX
d_kostbidrag = data.frame(d_kostbidrag * x.snitt.d)
rownames(d_kostbidrag) = d_DEA_id
colnames(d_kostbidrag) = d_DEA_id


#Beregner normkostandel
#1. Henter kostnadsbidrag
d_normkostandel = data.frame(d_kostbidrag)
#2. Regner summen av alle kolonner og deler på sum pr kolonne 
d_normkostandel = sweep(d_normkostandel,2, colSums(d_normkostandel),'/')
rownames(d_normkostandel) = d_DEA_id
colnames(d_normkostandel) = d_DEA_id


#Henter vekter
d_lambda = data.frame(dea.faktisk.snitt.d$lambda)
rownames(d_lambda) = d_DEA_id
colnames(d_lambda) = d_DEA_id

#Transponerer tilbake for å kunne koble DFs til et sett
d_kostbidrag = data.frame(t(d_kostbidrag))
d_normkostandel = data.frame(t(d_normkostandel))
#Legger til prefix på hver kolonne
colnames(d_lambda) = paste("d_vekt_", colnames(d_lambda),sep="")
colnames(d_kostbidrag) = paste("d_kostbidrag_", colnames(d_kostbidrag), sep="")
colnames(d_normkostandel) = paste("d_normkostandel", colnames(d_normkostandel), sep="")
#Kombinerer fire df til én
d_vekter.temp = data.frame(cbind(d_DEA_id, d_lambda, d_normkostandel, d_kostbidrag))
#Fjerner alle df som ikke har sum større enn 0
d_vekter.faktisk = d_vekter.temp[, colSums(d_vekter.temp) > 0]
#Fjerner midlertidege dfs
rm(d_kostbidrag, d_lambda, d_normkostandel, d_vekter.temp)

#Lager liste av IDer for referenter i D-nett
d_ref = as.list(colnames(d_vekter.faktisk))
d_ref = unique(na.omit(as.numeric(unlist(strsplit(unlist(d_ref), "[^0-9]+")))))


write.csv(d_vekter.faktisk, file = "./Resultater/d_vektberegning.csv")

# # Spesialkjøring for selskaper som bare kan være front for seg selv
# Er pr dd ikke aktuelt for D-nett.
# eff.snitt.snitt.r = res.tmp1$eff
# lambda.snitt.snitt.r = cbind(res.tmp1$lambda,matrix(NA,nrow=nrow(res.tmp1$lambda),ncol=length(sep.eval.r)))
# colnames(lambda.snitt.snitt.r) = c(front.r,sep.eval.r)
# for(i in sep.eval.r)
# {
#         res.tmp2 = dea(X=x.snitt.r,Y=y.snitt.r,RTS="crs",XREF=x.snitt.r[as.character(c(front.r,i))],YREF=y.snitt.r[as.character(c(front.r,i)),])
#         eff.snitt.snitt.r[as.character(i)] = res.tmp2$eff[as.character(i)] 
#         for(j in c(front.r,i))
#                 lambda.snitt.snitt.r[as.character(i),as.character(j)] = res.tmp2$lambda[as.character(i),paste("L_",as.character(j),sep="")]
# }
# remove(res.tmp1)
# remove(res.tmp2)
# 
# plot(sort(eff.snitt.snitt.r))
# View(lambda.snitt.snitt.r)



