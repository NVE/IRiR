#### Beregner og velger data for selskapene som skal evalueres ####

# Snittdata for selskaper som skal evalueres  
# D-nett  
x.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_TOTXDEA")]  
y.snitt.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_d_ab","sf_d_hs","sf_d_ns")]  
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.snitt.d) = d_DEA_id
rownames(y.snitt.d) = d_DEA_id


# R-nett  
x.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"sf_r_TOTXDEA"]  
y.snitt.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("sf_r_vluft","sf_r_vjord","sf_r_vsjo","sf_r_vgrs")]  
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.snitt.r) = r_DEA_id
rownames(y.snitt.r) = r_DEA_id


# Faktiske data for selskaper som skal evalueres
# D-nett
x.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,"d_TOTXDEA"]
y.faktisk.d = dat[dat$orgnr %in% d_tilDEA$orgnr & dat$aar == faktisk.aar,c("d_ab","d_hs","d_ns")]
#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.faktisk.d) = d_DEA_id
rownames(y.faktisk.d) = d_DEA_id

# R-nett
x.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,"r_TOTXDEA"]
rownames(x.faktisk.r)
y.faktisk.r = dat[dat$orgnr %in% r_tilDEA$orgnr & dat$aar == faktisk.aar,c("r_vluft","r_vjord","r_vsjo","r_vgrs")]

#Navngir rader for data til DEA slik at disse er gjenkjennelige i resultater
names(x.faktisk.r) = r_DEA_id
rownames(y.faktisk.r) = r_DEA_id

#Removing decimals for inputs and outputs
x.snitt.d <- round(x.snitt.d, digits = 0) 
y.snitt.d = y.snitt.d %>% mutate_each(funs(round(.,0)), sf_d_ab, sf_d_ns, sf_d_hs)
x.faktisk.d <- round(x.faktisk.d, digits = 0)
y.faktisk.d = y.faktisk.d %>% mutate_each(funs(round(.,0)), d_ab, d_ns, d_hs)
x.snitt.r <- round(x.snitt.r, digits = 0)  
y.snitt.r = y.snitt.r %>% mutate_each(funs(round(.,0)), sf_r_vluft, sf_r_vjord, sf_r_vsjo, sf_r_vgrs)
x.faktisk.r <- round(x.faktisk.r, digits = 0) 
y.faktisk.r = y.faktisk.r %>% mutate_each(funs(round(.,0)), r_vluft, r_vjord, r_vsjo, r_vgrs)



# #Disse får definere teknologien (fronten) i hovedkjøringen
# x.snitt.d <- round(x.snitt.d, digits = 0)  
# y.snitt.d$sf_d_ab  <- round(y.snitt.d$sf_d_ab, digits = 0)  
# y.snitt.d$sf_d_ns <- round(y.snitt.d$sf_d_ns, digits = 0)  
# y.snitt.d$sf_d_hs <- round(y.snitt.d$sf_d_hs, digits = 0)
# 
# #Disse måles mot fronten og gir gjeldende DEA-score
# x.faktisk.d <- round(x.faktisk.d, digits = 0) 
# y.faktisk.d$d_ab  <- round(y.faktisk.d$d_ab, digits = 0)  
# y.faktisk.d$d_ns <- round(y.faktisk.d$d_ns, digits = 0)  
# y.faktisk.d$d_hs <- round(y.faktisk.d$d_hs, digits = 0) 
# 
# #Runder av data til DEA til "hele tusen"  
# #Disse får definere teknologien (fronten) i hovedkjøringen
# x.snitt.r <- round(x.snitt.r, digits = 0)  
# y.snitt.r$sf_r_vluft  <- round(y.snitt.r$sf_r_vluft, digits = 0)  
# y.snitt.r$sf_r_vjord <- round(y.snitt.r$sf_r_vjord, digits = 0)  
# y.snitt.r$sf_r_vsjo <- round(y.snitt.r$sf_r_vsjo, digits = 0)
# y.snitt.r$sf_r_vgrs <- round(y.snitt.r$sf_r_vgrs, digits = 0)
# 
# #Disse måles mot fronten og gir gjeldende DEA-score
# x.faktisk.r <- round(x.faktisk.r, digits = 0) 
# y.faktisk.r$r_vluft  <- round(y.faktisk.r$r_vluft, digits = 0)  
# y.faktisk.r$r_vjord <- round(y.faktisk.r$r_vjord, digits = 0)  
# y.faktisk.r$r_vsjo <- round(y.faktisk.r$r_vsjo, digits = 0)
# y.faktisk.r$r_vgrs <- round(y.faktisk.r$r_vgrs, digits = 0)

#### DEA D-nett ####


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

d_lambda = d_lambda[,order(as.numeric(colnames(d_lambda)))]


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


d_lambda.snitt = d_lambda.snitt[,order(as.numeric(colnames(d_lambda.snitt)))]

##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
d_lambda[is.na(d_lambda)] <- 0
d_lambda.snitt[is.na(d_lambda.snitt)] <- 0

# #Beregner kostbidrag - dette er vekten for hver referent pr selskap ganget med referentens tilhørende snittkostnad.
# d_kostbidrag = d_lambda*x.snitt.d[match(colnames(d_lambda), names(x.snitt.d))][col(d_lambda)]
# d_kostbidrag.snitt = d_lambda.snitt*x.snitt.d[match(colnames(d_lambda.snitt), names(x.snitt.d))][col(d_lambda.snitt)]


# #Beregner normkostandel - dette er andelen av kostnadsnormen hver referent utgjør pr selskap
# d_normkostandel = d_kostbidrag/rowSums(d_kostbidrag)
# d_normkostandel.snitt = d_kostbidrag.snitt/rowSums(d_kostbidrag.snitt)

# #Lager liste av IDer for referenter i D-nett
# d_ref.alle = as.list(colnames(d_vekter.faktisk))
# d_ref.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(d_ref.alle), "[^0-9]+")))))
# d_ref.snitt.alle = as.list(colnames(d_vekter.snitt))
# d_ref.snitt.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(d_ref.snitt.alle), "[^0-9]+")))))
# #Selskaper som er referenter i R-nett
# d_ref = subset(d_ref.alle, d_ref.alle %in% d_normal)
# d_ref.snitt = subset(d_ref.snitt.alle, d_ref.snitt.alle %in% d_normal)
# #Selskaper i R-nett som er referenter for seg selv.
# d_ref.sep = subset(d_ref.alle, d_ref.alle %in% d_separat_dmuer)
# d_ref.snitt.sep = subset(d_ref.snitt.alle, d_ref.snitt.alle %in% d_separat_dmuer)

#### DEA R-nett ####

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

r_lambda = r_lambda[,order(as.numeric(colnames(r_lambda)))]


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


r_lambda.snitt = r_lambda.snitt[,order(as.numeric(colnames(r_lambda.snitt)))]


##Setter alle NA-verdier i lambda(vekt-dataframes til 0.)
r_lambda[is.na(r_lambda)] <- 0
r_lambda.snitt[is.na(r_lambda.snitt)] <- 0

# #Beregner kostbidrag - dette er vekten for hver referent pr selskap ganget med referentens tilhørende snittkostnad.
# r_kostbidrag = r_lambda*x.snitt.r[match(colnames(r_lambda), names(x.snitt.r))][col(r_lambda)]
# r_kostbidrag.snitt = r_lambda.snitt*x.snitt.r[match(colnames(r_lambda.snitt), names(x.snitt.r))][col(r_lambda.snitt)]
# 
# 
# #Beregner normkostandel - dette er andelen av kostnadsnormen hver referent utgjør pr selskap
# r_normkostandel = r_kostbidrag/rowSums(r_kostbidrag)
# r_normkostandel.snitt = r_kostbidrag.snitt/rowSums(r_kostbidrag.snitt)
# 
# 
# #Legger til prefix på hver kolonne
# colnames(r_lambda) = paste("r_vekt_", colnames(r_lambda),sep="")
# colnames(r_kostbidrag) = paste("r_kostbidrag_", colnames(r_kostbidrag), sep="")
# colnames(r_normkostandel) = paste("r_normkostandel", colnames(r_normkostandel), sep="")
# colnames(r_lambda.snitt) = paste("sf_r_vekt_", colnames(r_lambda.snitt),sep="")
# colnames(r_kostbidrag.snitt) = paste("sf_r_kostbidrag_", colnames(r_kostbidrag.snitt), sep="")
# colnames(r_normkostandel.snitt) = paste("sf_r_normkostandel", colnames(r_normkostandel.snitt), sep="")

# #Kombinerer fire df til én
# r_vekter.temp = data.frame(cbind(r_DEA_id, r_lambda, r_normkostandel, r_kostbidrag))
# r_vekter.temp.snitt = data.frame(cbind(r_DEA_id, r_lambda.snitt, r_normkostandel.snitt, r_kostbidrag.snitt))
# #Fjerner alle kolonner som ikke har sum større enn 0
# r_vekter.faktisk = r_vekter.temp[, colSums(r_vekter.temp) > 0]
# r_vekter.snitt = r_vekter.temp.snitt[, colSums(r_vekter.temp.snitt) > 0]
# #Fjerner midlertidege dfs
# rm(r_kostbidrag, r_lambda, r_normkostandel, r_vekter.temp)
# rm(r_kostbidrag.snitt, r_lambda.snitt, r_normkostandel.snitt, r_vekter.temp.snitt)
# #Lager liste av IDer for referenter i D-nett
# r_ref.alle = as.list(colnames(r_vekter.faktisk))
# r_ref.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref.alle), "[^0-9]+")))))
# r_ref.snitt.alle = as.list(colnames(r_vekter.snitt))
# r_ref.snitt.alle = unique(na.omit(as.numeric(unlist(strsplit(unlist(r_ref.snitt.alle), "[^0-9]+")))))
# #Selskaper som er referenter i R-nett
# r_ref = subset(r_ref.alle, r_ref.alle %in% r_normal)
# r_ref.snitt = subset(r_ref.snitt.alle, r_ref.snitt.alle %in% r_normal)
# #Selskaper i R-nett som er referenter for seg selv.
# r_ref.sep = subset(r_ref.alle, r_ref.alle %in% r_separat_dmuer)
# r_ref.snitt.sep = subset(r_ref.snitt.alle, r_ref.snitt.alle %in% r_separat_dmuer)
# # 
# # #Lage logisk sjekk for å sjekke dref=drefsnitt
# 
# rm(r_ref.snitt, r_ref.snitt.alle, r_ref.snitt.sep, r_ref.alle)