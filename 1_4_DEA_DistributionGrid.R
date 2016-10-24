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
write.csv(cbind(d_tilDEA$id, x.snitt.d, y.snitt.d,x.faktisk.d, y.faktisk.d), file = "d_InputDEA.csv")


# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.d og y.snitt.d, deascore beregnes som 
# årets observasjoner av kostnader (x.faktisk.d) og oppgaver y.faktisk.d
        #D-nett
dea.faktisk.snitt.d = dea(X=x.faktisk.d, Y=y.faktisk.d, XREF=x.snitt.d, YREF=y.snitt.d, RTS="crs")
plot(sort(dea.faktisk.snitt.d$eff))
#View(cbind(x.faktisk.d, y.snitt.d, dea.faktisk.snitt.d$eff)[order(dea.faktisk.snitt.d$eff)])
write.csv(cbind(d_tilDEA$id,dea.faktisk.snitt.d$eff), file = "DEAeff1.csv")
write.csv(cbind(d_tilDEA,dea.faktisk.snitt.d$eff), file = "d_DEAResultat_Data.csv")


#res.dea.d = dea(X=x.faktisk.d,Y=y.snitt.d,XREF=x.snitt.r[as.character(front.r)],YREF=y.snitt.r[as.character(front.r),],RTS="crs"))
#plot(sort(res.snitt.snitt.r$eff))