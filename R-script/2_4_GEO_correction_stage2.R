## Beregner koeffisienter på RVK for D-nett

#Lager dummy for selskapene som skal være med til trinn 2
d_tilDEA$d_frontlov_hoved = d_tilDEA$frontlov 

d_tilDEA$d_til2trinn = 0

 
#Lage Fjellbekk - Geo1

fjellbekk_var = data.frame(cbind(d_tilDEA$dr_he1, d_tilDEA$dr_s7, d_tilDEA$dr_skysz))
fjellbekk_var = data.frame(plyr::rename(fjellbekk_var, c("X1"="dr_he1", "X2"="dr_s7", "X3"="dr_skysz")))

# Bruker to ulike metoder for faktoranalyse
pca.fb.1=PCA(fjellbekk_var, scale.unit = TRUE, ncp=1, graph = T) # Beregner eigenverdier og lager grafer
pca.fb.2 = princomp(fjellbekk_var) # Beregner prinsipal-komponenter. (eigenvektorer)
pca.fb.3 = prcomp(fjellbekk_var, scale. = TRUE)

#Skriver de 
pca.fb.1$eig
pca.fb.2$loadings
#Lager dataframe av de ønskede estimerte verdiene fra PCA
df.dr_Geo1=data.frame(pca.fb.3$x[,1])
df.dr_Geo1=df.dr_Geo1*-1
colnames(df.dr_Geo1) <- "dr_Geo1" 

d_tilDEA = data.frame(cbind(d_tilDEA, df.dr_Geo1))
rm(df.dr_Geo1)

#Kjører regresjon fjellbekk
fjellbekk_reg <- lm(d_tilDEA$dr_Geo1 ~ d_tilDEA$dr_he1 + d_tilDEA$dr_s7 + d_tilDEA$dr_skysz)
summary(fjellbekk_reg)
rm(fjellbekk_var)

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


# Lage Øyvind - Geo 2

oyvind_var = data.frame(cbind(d_tilDEA$dr_vr2_k2lukk, d_tilDEA$dr_aoey1sz, d_tilDEA$dr_hssjoand))
oyvind_var = data.frame(plyr::rename(oyvind_var, c("X1"="dr_vr2_k2lukk", "X2"="dr_aoey1sz", "X3"="dr_hssjoand")))

pca.oy.3 = prcomp(oyvind_var, scale. = TRUE)
df.dr_Geo2=data.frame(pca.oy.3$x[,1])
colnames(df.dr_Geo2) <- "dr_Geo2" 
d_tilDEA = data.frame(cbind(d_tilDEA, df.dr_Geo2))
rm(df.dr_Geo2)

oyvind_reg <- lm(d_tilDEA$dr_Geo2 ~ d_tilDEA$dr_vr2_k2lukk + d_tilDEA$dr_aoey1sz + d_tilDEA$dr_hssjoand)
summary(oyvind_reg)


# Lage Frost - Geo 3
d_tilDEA$dr_tempneg = d_tilDEA$dr_temp*-1 # Dette er ikke korrekt pt
if (d_tilDEA$dr_brgrad_gjsn < 65.9){
        d_tilDEA$dr_brgrad_gjsn==65.9        
} else {
        
}