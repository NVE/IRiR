#### DEA R-nett ####

#### Trinn 1 - DEA-kjøringer ####

#Runder av data til DEA til "hele tusen"  
#Disse får definere teknologien (fronten) i hovedkjøringen
x.snitt.r <- round(x.snitt.r, digits = 0)  
y.snitt.r$sf_r_ab  <- round(y.snitt.r$sf_r_ab, digits = 0)  
y.snitt.r$sf_r_ns <- round(y.snitt.r$sf_r_ns, digits = 0)  
y.snitt.r$sf_r_hs <- round(y.snitt.r$sf_r_hs, digits = 0)

#Disse måles mot fronten og gir gjeldende DEA-score
x.faktisk.r <- round(x.faktisk.r, digits = 0) 
y.faktisk.r$r_ab  <- round(y.faktisk.r$r_ab, digits = 0)  
y.faktisk.r$r_ns <- round(y.faktisk.r$r_ns, digits = 0)  
y.faktisk.r$r_hs <- round(y.faktisk.r$r_hs, digits = 0) 

### DEA input
write.csv(cbind(r_tilDEA$id, x.snitt.r, y.snitt.r, x.faktisk.r, y.faktisk.r), file = "./Resultater/r_InputDEA.csv")
