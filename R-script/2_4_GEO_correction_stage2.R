## Beregner koeffisienter på RVK for D-nett

#Lager dummy for selskapene som skal være med til trinn 2
d_tilDEA$d_frontlov_hoved = d_tilDEA$frontlov 

d_tilDEA$d_til2trinn = 0

#Ulik datastruktur gjør dette kanskje unødvendig i R.
#Kan se nærmere på dette i ifbm R-nett. 
#
#Roars kode:
# 
# // Bestemme hvilke selskaper som skal være med i analysen (kun frontselskaper, samt noen til, dette kan ev vurderes på nytt med bootstrap)
# gen d_til2trinn = 0
# label var d_til2trinn "Hvis 1, er med i trinn2"
# 
# replace d_til2trinn = 1 if d_frontlov_hoved==1 & aar==curr_aar
# 
# if length("$d_ikke_til_trinn2")>1 {
#         foreach num of numlist $d_ikke_til_trinn2 {    
#                 replace d_til2trinn = 0 if id == `num'
#         }
#         }
#                 
#                 
#                 
#                 replace d_til2trinn = F.d_til2trinn if aar==curr_aar-1


#Lage Fjellbekk

fjellbekk_var = data.frame(cbind(d_tilDEA$dr_he1, d_tilDEA$dr_s7, d_tilDEA$dr_skysz))
fjellbekk_var = data.frame(plyr::rename(fjellbekk_var, c("X1"="dr_he1", "X2"="dr_s7", "X3"="dr_skysz")))

# Bruker to ulike metoder for faktoranalyse
pca.fb.1=PCA(fjellbekk_var) # Beregner eigenverdier og lager grafer
pca.fb.2 = princomp(fjellbekk_var, cor=TRUE) # Beregner prinsipal-komponenter. (eigenvektorer)
#Skriver de 
pca.fb.1$eig
pca.fb.2$loadings
#Lager dataframe av de ønskede estimerte verdiene fra PCA
df.dr_Geo1 = data.frame(pca.fb.1$ind$coord[,1:1] )
colnames(df.dr_Geo1) <- "dr_Geo1" 

d_tilDEA = data.frame(cbind(d_tilDEA, df.dr_Geo1))
rm(df.dr_Geo1)

#Kjører regresjon fjellbekk
fjellbekk_reg <- lm(d_tilDEA$dr_Geo1 ~ d_tilDEA$dr_he1 + d_tilDEA$dr_s7 + d_tilDEA$dr_skysz) # Ikke helt identisk, får grave litt etter årsak.
summary(fjellbekk_reg)

## Nedenfor følger frekkevarianter for å forstå mellomregninger
# pca.fb = prcomp(fjellbekk_var, scale = TRUE)
# names(pca.fb)
# 
# #Loadings / Principal components (eigenvectors) 
# pca.fb$rotation
# # Plotter principal component scores
# biplot(pca.fb, scale = 0)
# 
# #Eigenvalue til hver component er tilsvarende dens varians
# #Beregner varians for hver faktor (principal component)
# fb.std.dev = pca.fb$sdev
# fb.var = fb.std.dev^2
# fb.var[1:3]
# #Andel av varians som forklares av hver komponent
# prop_fbvarex = fb.var/sum(fb.var)
# #Principal components/correlation
# prop_fbvarex[1:3]
# 
# #Hvor mange faktorer bør da være med til modellering?
# #Scree-plot
# plot(prop_fbvarex, xlab = "Principal component", 
#      ylab = "Proportion of Variance Explained",
#      type = "b")
# #Hvert punkt viser nå hvor stor andel av hver faktor som forklarer varians
# #Kumulativ scree-plot kan også gi et ganske klart tegn på hvor mye mer av
# #varians som forklares ved å inkludere en faktor til.
# 
# plot(cumsum(prop_fbvarex), xlab = "Principal component", 
#      ylab = "Cumulative Proportion of Variance Explained",
#      type = "b")




