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


#selskapene som kun kan danne front for seg selv er disse
d_separat_dmuer

#id for alle selskapene som ikke er spesialmodell

# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.d og y.snitt.d, deascore beregnes som 
# årets observasjoner av kostnader (x.faktisk.d) og oppgaver y.faktisk.d
#D-nett
dea.faktisk.snitt.d = dea(X=x.faktisk.d, Y=y.faktisk.d, XREF=x.snitt.d[as.character(d_normal)], YREF=y.snitt.d[as.character(d_normal),], RTS="crs")
plot(sort(dea.faktisk.snitt.d$eff))
#View(cbind(x.faktisk.d, y.snitt.d, dea.faktisk.snitt.d$eff)[order(dea.faktisk.snitt.d$eff)])
d_tilDEA = data.frame(cbind(d_tilDEA, dea.faktisk.snitt.d$eff))
#Endrer navn på variabelen som merges inn
colnames(d_tilDEA)[colnames(d_tilDEA)=="dea.faktisk.snitt.d.eff"] <- "d_f_sf_eff"

#Beregner ren snitt front
dea.snitt.snitt.d = dea(X=x.snitt.d, Y=y.snitt.d, XREF=x.snitt.d[as.character(d_normal)], YREF=y.snitt.d[as.character(d_normal),], RTS="crs")
plot(sort(dea.snitt.snitt.d$eff))
d_tilDEA = data.frame(cbind(d_tilDEA, dea.snitt.snitt.d$eff))
#Endrer navn på variabelen som merges inn
colnames(d_tilDEA)[colnames(d_tilDEA)=="dea.snitt.snitt.d.eff"] <- "d_sf_eff"


#spesialkjøring for selskaper som bare kan være front for seg selv
eff.faktisk.snitt.d = dea.faktisk.snitt.d$eff

d_lambda = cbind(dea.faktisk.snitt.d$lambda,matrix(NA,nrow=nrow(dea.faktisk.snitt.d$lambda),ncol=length(d_separat_dmuer)))
colnames(d_lambda) = c(d_normal,d_separat_dmuer)
for(i in d_separat_dmuer)
{
        dea.sep.faktisk.snitt.d = dea(X=x.faktisk.d,Y=y.faktisk.d,RTS="crs",XREF=x.snitt.d[as.character(c(d_normal,i))],YREF=y.snitt.d[as.character(c(d_normal,i)),])
        eff.faktisk.snitt.d[as.character(i)] = dea.sep.faktisk.snitt.d$eff[as.character(i)] 
        for(j in c(d_normal,i))
                d_lambda[as.character(i),as.character(j)] = dea.sep.faktisk.snitt.d$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}


#spesialkjøring for selskaper som bare kan være front for seg selv - blir noe feil med snitt-mot-snitt-kjøring
eff.snitt.snitt.d = dea.snitt.snitt.d$eff

d_lambda.snitt = cbind(dea.snitt.snitt.d$lambda,matrix(NA,nrow=nrow(dea.snitt.snitt.d$lambda),ncol=length(d_separat_dmuer)))
colnames(d_lambda.snitt) = c(d_normal,d_separat_dmuer)
for(i in d_separat_dmuer)
{
        dea.sep.snitt.snitt.d = dea(X=x.snitt.d,Y=y.snitt.d,RTS="crs",XREF=x.snitt.d[as.character(c(d_normal,i))],YREF=y.snitt.d[as.character(c(d_normal,i)),])
        eff.snitt.snitt.d[as.character(i)] = dea.sep.snitt.snitt.d$eff[as.character(i)]
        for(j in c(d_normal,i))
                d_lambda.snitt[as.character(i),as.character(j)] = dea.sep.snitt.snitt.d$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}

##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
d_lambda[is.na(d_lambda)] <- 0
d_lambda.snitt[is.na(d_lambda.snitt)] <- 0

#Beregner kostbidrag - dette er vekten for hver referent pr selskap ganget med referentens tilhørende snittkostnad.
d_kostbidrag = d_lambda*x.snitt.d[match(colnames(d_lambda), names(x.snitt.d))][col(d_lambda)]
d_kostbidrag.snitt = d_lambda.snitt*x.snitt.d[match(colnames(d_lambda.snitt), names(x.snitt.d))][col(d_lambda.snitt)]

# write.csv(cbind(d_tilDEA$id,dea.faktisk.snitt.d$eff), file = "./Resultater/DEAeff1.csv")
# write.csv(d_tilDEA, file = "./Resultater/d_DEAResultat_Data.csv")

#Beregner normkostandel - dette er andelen av kostnadsnormen hver referent utgjør pr selskap
d_normkostandel = d_kostbidrag/rowSums(d_kostbidrag)
d_normkostandel.snitt = d_kostbidrag.snitt/rowSums(d_kostbidrag.snitt)


#Legger til prefix på hver kolonne
colnames(d_lambda) = paste("d_vekt_", colnames(d_lambda),sep="")
colnames(d_kostbidrag) = paste("d_kostbidrag_", colnames(d_kostbidrag), sep="")
colnames(d_normkostandel) = paste("d_normkostandel", colnames(d_normkostandel), sep="")
colnames(d_lambda.snitt) = paste("sf_d_vekt_", colnames(d_lambda.snitt),sep="")
colnames(d_kostbidrag.snitt) = paste("sf_d_kostbidrag_", colnames(d_kostbidrag.snitt), sep="")
colnames(d_normkostandel.snitt) = paste("sf_d_normkostandel", colnames(d_normkostandel.snitt), sep="")

#Kombinerer fire df til én
d_vekter.temp = data.frame(cbind(d_DEA_id, d_lambda, d_normkostandel, d_kostbidrag))
d_vekter.temp.snitt = data.frame(cbind(d_DEA_id, d_lambda.snitt, d_normkostandel.snitt, d_kostbidrag.snitt))
#Fjerner alle kolonner som ikke har sum større enn 0
d_vekter.faktisk = d_vekter.temp[, colSums(d_vekter.temp) > 0]
d_vekter.snitt = d_vekter.temp.snitt[, colSums(d_vekter.temp.snitt) > 0]
#Fjerner midlertidege dfs
rm(d_kostbidrag, d_lambda, d_normkostandel, d_vekter.temp)
rm(d_kostbidrag.snitt, d_lambda.snitt, d_normkostandel.snitt, d_vekter.temp.snitt)
#Lager liste av IDer for referenter i D-nett
d_ref.alle = as.list(colnames(d_vekter.faktisk))
d_ref.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(d_ref.alle), "[^0-9]+")))))
d_ref.snitt.alle = as.list(colnames(d_vekter.snitt))
d_ref.snitt.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(d_ref.snitt.alle), "[^0-9]+")))))
#Selskaper som er referenter i R-nett
d_ref = subset(d_ref.alle, d_ref.alle %in% d_normal)
d_ref.snitt = subset(d_ref.snitt.alle, d_ref.snitt.alle %in% d_normal)
#Selskaper i R-nett som er referenter for seg selv.
d_ref.sep = subset(d_ref.alle, d_ref.alle %in% d_separat_dmuer)
d_ref.snitt.sep = subset(d_ref.snitt.alle, d_ref.snitt.alle %in% d_separat_dmuer)
# 
# #Lage logisk sjekk for å sjekke dref=drefsnitt



