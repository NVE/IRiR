#### DEA D-nett ####

#### Trinn 1 - DEA-kjøringer ####

#Runder av data til DEA til "hele tusen"  
x.faktisk.d <- round(x.faktisk.d, digits = 0)  
y.snitt.d$sf_d_ab  <- round(y.snitt.d$sf_d_ab, digits = 0)  
y.snitt.d$sf_d_ns <- round(y.snitt.d$sf_d_ns, digits = 0)  
y.snitt.d$sf_d_hs <- round(y.snitt.d$sf_d_hs, digits = 0) 

### DEA input
write.csv(cbind(x.faktisk.d, y.snitt.d), file = "deaData.csv")


# Hovedkjøring trinn 1
# Merk at fronten defineres av de radene i x.snitt.r og y.snitt.r som tilvhører selskapene i front.r
dea.faktisk.snitt.d = sdea(X=x.faktisk.d, Y=y.snitt.d, RTS="crs")
#plot(sort(dea.faktisk.snitt.d$eff))
#View(cbind(x.faktisk.d, y.snitt.d, dea.faktisk.snitt.d$eff)[order(dea.faktisk.snitt.d$eff)])
write.csv(dea.faktisk.snitt.d$eff, file = "DEAeff1.csv")



#res.dea.d = dea(X=x.faktisk.d,Y=y.snitt.d,XREF=x.snitt.r[as.character(front.r)],YREF=y.snitt.r[as.character(front.r),],RTS="crs")
#plot(sort(res.snitt.snitt.r$eff))