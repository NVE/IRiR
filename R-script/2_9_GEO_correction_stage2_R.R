## Beregner koeffisienter på RVK for D-nett

#Lager dummy for selskapene som skal være med til trinn 2
r_tilDEA$r_frontlov_hoved = r_tilDEA$frontlov 

r_tilDEA$r_til2trinn = 0


#Lage Helskog - Geo3

helskog_var = data.frame(cbind(r_tilDEA$rr_s12, r_tilDEA$rr_he))
helskog_var = data.frame(plyr::rename(helskog_var, c("X1"="rr_s12", "X2"="rr_he")))

# Bruker to ulike metoder for faktoranalyse
pca.fb.1=PCA(helskog_var, scale.unit = TRUE, ncp=1, graph = T) # Beregner eigenverdier og lager grafer
pca.fb.2 = princomp(helskog_var) # Beregner prinsipal-komponenter. (eigenvektorer)
pca.fb.3 = prcomp(helskog_var, scale. = TRUE)

#Skriver de 
pca.fb.1$eig
pca.fb.2$loadings
#Lager dataframe av de ønskede estimerte verdiene fra PCA
df.rr_Geo3=data.frame(pca.fb.3$x[,1])
df.rr_Geo3=df.rr_Geo3*-1
colnames(df.rr_Geo3) <- "rr_Geo3" 

r_tilDEA = data.frame(cbind(r_tilDEA, df.rr_Geo3))
rm(df.rr_Geo3)

#Kjører regresjon helskog
helskog_reg <- lm(r_tilDEA$dr_Geo3 ~ r_tilDEA$rr_s12 + r_tilDEA$rr_he)
summary(helskog_reg)
rm(helskog_var)

## Nedenfor følger frekkevarianter for å forstå mellomregninger
# pca.fb = prcomp(helskog_var, scale = TRUE)
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



