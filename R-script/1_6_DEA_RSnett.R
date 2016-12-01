#### DEA R-nett ####


#Runder av data til DEA til "hele tusen"  
#Disse får definere teknologien (fronten) i hovedkjøringen
x.snitt.r <- round(x.snitt.r, digits = 0)  
y.snitt.r$sf_r_vluft  <- round(y.snitt.r$sf_r_vluft, digits = 0)  
y.snitt.r$sf_r_vjord <- round(y.snitt.r$sf_r_vjord, digits = 0)  
y.snitt.r$sf_r_vsjo <- round(y.snitt.r$sf_r_vsjo, digits = 0)
y.snitt.r$sf_r_vgrs <- round(y.snitt.r$sf_r_vgrs, digits = 0)

#Disse måles mot fronten og gir gjeldende DEA-score
x.faktisk.r <- round(x.faktisk.r, digits = 0) 
y.faktisk.r$r_vluft  <- round(y.faktisk.r$r_vluft, digits = 0)  
y.faktisk.r$r_vjord <- round(y.faktisk.r$r_vjord, digits = 0)  
y.faktisk.r$r_vsjo <- round(y.faktisk.r$r_vsjo, digits = 0)
y.faktisk.r$r_vgrs <- round(y.faktisk.r$r_vgrs, digits = 0) 

### DEA input
write.csv(cbind(r_tilDEA$id, x.snitt.r, y.snitt.r, x.faktisk.r, y.faktisk.r), file = "./Resultater/r_InputDEA.csv")

#selskapene som kun kan danne front for seg selv er disse
r_separat_dmuer

#id for alle selskapene som ikke er spesialmodell

# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.d og y.snitt.d, deascore beregnes som 
# årets observasjoner av kostnader (x.faktisk.d) og oppgaver y.faktisk.d
#D-nett
dea.faktisk.snitt.r = dea(X=x.faktisk.r, Y=y.faktisk.r, XREF=x.snitt.r[as.character(r_normal)], YREF=y.snitt.r[as.character(r_normal),], RTS="crs")
plot(sort(dea.faktisk.snitt.r$eff))
#View(cbind(x.faktisk.d, y.snitt.d, dea.faktisk.snitt.d$eff)[order(dea.faktisk.snitt.d$eff)])
r_tilDEA = data.frame(cbind(r_tilDEA, dea.faktisk.snitt.r$eff))
#Endrer navn på variabelen som merges inn
colnames(r_tilDEA)[colnames(r_tilDEA)=="dea.faktisk.snitt.r.eff"] <- "r_f_sf_eff"

#Beregner ren snitt front
dea.snitt.snitt.r = dea(X=x.snitt.r, Y=y.snitt.r, XREF=x.snitt.r[as.character(r_normal)], YREF=y.snitt.r[as.character(r_normal),], RTS="crs")
plot(sort(dea.snitt.snitt.r$eff))
r_tilDEA = data.frame(cbind(r_tilDEA, dea.snitt.snitt.r$eff))
#Endrer navn på variabelen som merges inn
colnames(r_tilDEA)[colnames(r_tilDEA)=="dea.snitt.snitt.r.eff"] <- "r_sf_eff"


#spesialkjøring for selskaper som bare kan være front for seg selv
eff.faktisk.snitt.r = dea.faktisk.snitt.r$eff

lambda.faktisk.snitt.r = cbind(dea.faktisk.snitt.r$lambda,matrix(NA,nrow=nrow(dea.faktisk.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(lambda.faktisk.snitt.r) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.faktisk.snitt.r = dea(X=x.faktisk.r,Y=y.faktisk.r,RTS="crs",XREF=x.snitt.r[as.character(c(r_normal,i))],YREF=y.snitt.r[as.character(c(r_normal,i)),])
        eff.faktisk.snitt.r[as.character(i)] = dea.sep.faktisk.snitt.r$eff[as.character(i)] 
        for(j in c(r_normal,i))
                lambda.faktisk.snitt.r[as.character(i),as.character(j)] = dea.sep.faktisk.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}


#spesialkjøring for selskaper som bare kan være front for seg selv
eff.snitt.snitt.r = dea.snitt.snitt.r$eff

lambda.snitt.snitt.r = cbind(dea.snitt.snitt.r$lambda,matrix(NA,nrow=nrow(dea.snitt.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(lambda.snitt.snitt.r) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.snitt.snitt.r = dea(X=x.snitt.r,Y=y.snitt.r,RTS="crs",XREF=x.snitt.r[as.character(c(r_normal,i))],YREF=y.snitt.r[as.character(c(r_normal,i)),])
        eff.snitt.snitt.r[as.character(i)] = dea.sep.snitt.snitt.r$eff[as.character(i)] 
        for(j in c(r_normal,i))
                lambda.snitt.snitt.r[as.character(i),as.character(j)] = dea.sep.snitt.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}

##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
lambda.faktisk.snitt.r[is.na(lambda.faktisk.snitt.r)] <- 0
lambda.snitt.snitt.r[is.na(lambda.snitt.snitt.r)] <- 0
write.csv(cbind(d_tilDEA$id,dea.faktisk.snitt.d$eff), file = "./Resultater/DEAeff1.csv")
write.csv(d_tilDEA, file = "./Resultater/r_DEAResultat_Data.csv")


#Beregner kostbidrag
#1. Henter vekter fra DEA-beregning
r_kostbidrag = data.frame(dea.faktisk.snitt.r$lambda)
r_kostbidrag.snitt = data.frame(dea.snitt.snitt.r$lambda)
#2. Transponerer matrisen slik at den kan ganges direkte ut med TOTEX
r_kostbidrag = data.frame(t(r_kostbidrag))
r_kostbidrag.snitt = data.frame(t(r_kostbidrag.snitt))
#3. Multipliserer df med snitt-TOTEX
r_kostbidrag = data.frame(r_kostbidrag * x.snitt.r)
r_kostbidrag.snitt = data.frame(r_kostbidrag.snitt * x.snitt.r)
rownames(r_kostbidrag) = subset(r_DEA_id, r_DEA_id %in% r_normal)
colnames(r_kostbidrag) = r_DEA_id
rownames(r_kostbidrag.snitt) = subset(r_DEA_id, r_DEA_id %in% r_normal)
colnames(r_kostbidrag.snitt) = r_DEA_id

#Beregner normkostandel
#1. Henter kostnadsbidrag
r_normkostandel = data.frame(r_kostbidrag)
r_normkostandel.snitt = data.frame(r_kostbidrag.snitt)
#2. Regner summen av alle kolonner og deler på sum pr kolonne 
r_normkostandel = sweep(r_normkostandel,2, colSums(r_normkostandel),'/')
r_normkostandel.snitt = sweep(r_normkostandel.snitt,2, colSums(r_normkostandel.snitt),'/')
rownames(r_normkostandel) = subset(r_DEA_id, r_DEA_id %in% r_normal)
colnames(r_normkostandel) = r_DEA_id
rownames(r_normkostandel.snitt) = subset(r_DEA_id, r_DEA_id %in% r_normal)
colnames(r_normkostandel.snitt) = r_DEA_id

#Henter vekter
r_lambda = data.frame(dea.faktisk.snitt.r$lambda)
r_lambda.snitt = data.frame(dea.snitt.snitt.r$lambda)
rownames(r_lambda) = subset(r_DEA_id, r_DEA_id %in% r_normal | r_DEA_id %in% r_separat_dmuer)
colnames(r_lambda) = subset(r_DEA_id, r_DEA_id %in% r_normal)
rownames(r_lambda.snitt) = subset(r_DEA_id, r_DEA_id %in% r_normal | r_DEA_id %in% r_separat_dmuer)
colnames(r_lambda) = subset(r_DEA_id, r_DEA_id %in% r_normal)

#Transponerer tilbake for å kunne koble DFs til et sett
r_kostbidrag = data.frame(t(r_kostbidrag))
r_normkostandel = data.frame(t(r_normkostandel))
r_kostbidrag.snitt = data.frame(t(r_kostbidrag.snitt))
r_normkostandel.snitt = data.frame(t(r_normkostandel.snitt))
#Legger til prefix på hver kolonne
colnames(r_lambda) = paste("r_vekt_", colnames(r_lambda),sep="")
colnames(r_kostbidrag) = paste("r_kostbidrag_", colnames(r_kostbidrag), sep="")
colnames(r_normkostandel) = paste("r_normkostandel", colnames(r_normkostandel), sep="")
colnames(r_lambda.snitt) = paste("sf_r_vekt_", colnames(r_lambda.snitt),sep="")
colnames(r_kostbidrag.snitt) = paste("sf_r_kostbidrag_", colnames(r_kostbidrag.snitt), sep="")
colnames(r_normkostandel.snitt) = paste("sf_r_normkostandel", colnames(r_normkostandel.snitt), sep="")

#Kombinerer fire df til én
r_vekter.temp = data.frame(cbind(r_DEA_id, r_lambda, r_normkostandel, r_kostbidrag))
r_vekter.temp.snitt = data.frame(cbind(r_DEA_id, r_lambda.snitt, r_normkostandel.snitt, r_kostbidrag.snitt))
#Fjerner alle kolonner som ikke har sum større enn 0
r_vekter.faktisk = r_vekter.temp[, colSums(r_vekter.temp) > 0]
r_vekter.snitt = r_vekter.temp.snitt[, colSums(r_vekter.temp.snitt) > 0]
#Fjerner midlertidege dfs
rm(r_kostbidrag, r_lambda, r_normkostandel, r_vekter.temp)
rm(r_kostbidrag.snitt, r_lambda.snitt, r_normkostandel.snitt, r_vekter.temp.snitt)
#Lager liste av IDer for referenter i D-nett
r_ref = as.list(colnames(r_vekter.faktisk))
r_ref = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref), "[^0-9]+")))))
r_ref.snitt = as.list(colnames(r_vekter.snitt))
r_ref.snitt = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref.snitt), "[^0-9]+")))))

#Lage logisk sjekk for å sjekke dref=drefsnitt

write.csv(r_vekter.faktisk, file = "./Resultater/r_vektberegning.csv")


