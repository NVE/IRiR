#Alternativ metode kostbidrag

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

#matrise med krysningspunkt



r_kostbidrag = lambda.faktisk.snitt.r*x.snitt.r[match(colnames(lambda.faktisk.snitt.r), names(x.snitt.r))][col(lambda.faktisk.snitt.r)]
r_kostbidrag.snitt = lambda.snitt.snitt.r*x.snitt.r[match(colnames(lambda.snitt.snitt.r), names(x.snitt.r))][col(lambda.snitt.snitt.r)]
