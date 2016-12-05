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

r_lambda = cbind(dea.faktisk.snitt.r$lambda,matrix(NA,nrow=nrow(dea.faktisk.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(r_lambda) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.faktisk.snitt.r = dea(X=x.faktisk.r,Y=y.faktisk.r,RTS="crs",XREF=x.snitt.r[as.character(c(r_normal,i))],YREF=y.snitt.r[as.character(c(r_normal,i)),])
        eff.faktisk.snitt.r[as.character(i)] = dea.sep.faktisk.snitt.r$eff[as.character(i)] 
        for(j in c(r_normal,i))
                r_lambda[as.character(i),as.character(j)] = dea.sep.faktisk.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}


#spesialkjøring for selskaper som bare kan være front for seg selv - blir noe feil med snitt-mot-snitt-kjøring
eff.snitt.snitt.r = dea.snitt.snitt.r$eff

r_lambda.snitt = cbind(dea.snitt.snitt.r$lambda,matrix(NA,nrow=nrow(dea.snitt.snitt.r$lambda),ncol=length(r_separat_dmuer)))
colnames(r_lambda.snitt) = c(r_normal,r_separat_dmuer)
for(i in r_separat_dmuer)
{
        dea.sep.snitt.snitt.r = dea(X=x.snitt.r,Y=y.snitt.r,RTS="crs",XREF=x.snitt.r[as.character(c(r_normal,i))],YREF=y.snitt.r[as.character(c(r_normal,i)),])
        eff.snitt.snitt.r[as.character(i)] = dea.sep.snitt.snitt.r$eff[as.character(i)]
        for(j in c(r_normal,i))
                r_lambda.snitt[as.character(i),as.character(j)] = dea.sep.snitt.snitt.r$lambda[as.character(i),paste("L_",as.character(j),sep="")]
}

##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
r_lambda[is.na(r_lambda)] <- 0
r_lambda.snitt[is.na(r_lambda.snitt)] <- 0

#Beregner kostbidrag - dette er vekten for hver referent pr selskap ganget med referentens tilhørende snittkostnad.
r_kostbidrag = r_lambda*x.snitt.r[match(colnames(r_lambda), names(x.snitt.r))][col(r_lambda)]
r_kostbidrag.snitt = r_lambda.snitt*x.snitt.r[match(colnames(r_lambda.snitt), names(x.snitt.r))][col(r_lambda.snitt)]

# write.csv(cbind(d_tilDEA$id,dea.faktisk.snitt.d$eff), file = "./Resultater/DEAeff1.csv")
# write.csv(d_tilDEA, file = "./Resultater/r_DEAResultat_Data.csv")

#Beregner normkostandel - dette er andelen av kostnadsnormen hver referent utgjør pr selskap
r_normkostandel = r_kostbidrag/rowSums(r_kostbidrag)
r_normkostandel.snitt = r_kostbidrag.snitt/rowSums(r_kostbidrag.snitt)


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
r_ref.alle = as.list(colnames(r_vekter.faktisk))
r_ref.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref.alle), "[^0-9]+")))))
r_ref.snitt.alle = as.list(colnames(r_vekter.snitt))
r_ref.snitt.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref.snitt.alle), "[^0-9]+")))))
#Selskaper som er referenter i R-nett
r_ref = subset(r_ref.alle, r_ref.alle %in% r_normal)
r_ref.snitt = subset(r_ref.snitt.alle, r_ref.snitt.alle %in% r_normal)
#Selskaper i R-nett som er referenter for seg selv.
r_ref.sep = subset(r_ref.alle, r_ref.alle %in% r_separat_dmuer)
r_ref.snitt.sep = subset(r_ref.snitt.alle, r_ref.snitt.alle %in% r_separat_dmuer)
# 
# #Lage logisk sjekk for å sjekke dref=drefsnitt

rm(r.ref.alle, r_ref.snitt, r_ref.snitt.alle, r_ref.snitt.sep, r_ref.alle)

# write.csv(r_vekter.faktisk, file = "./Resultater/r_vektberegning.csv")
# 
# 
