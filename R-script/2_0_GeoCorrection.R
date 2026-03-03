
#### 2_0 Adjusting efficiency scores for environmental factors (z-variables) - Stage 2 ####
# Only relevant for local distribution

#### Bootstrap ####
if(BS.new == 1) {
  ld_bs = as.data.frame(dea.boot(X=as.matrix(X.avg.ld[as.character(ld_eval)]),
                                 Y=as.matrix(Y.avg.ld[as.character(ld_eval),]),
                                 NREP = BS.ite, RTS="crs", ORIENTATION = "in", alpha=0.1)$eff.bc)
} else {
  ld_bs = read.csv("./Data/Bootstrap/ld_bs.csv",sep=",")
}
if(BS.new == 1 ) {
  names(ld_bs) = "ld_bs.correst"
  ld_bs = rownames_to_column(ld_bs, "id")
  write.csv(ld_bs, "./Data/Bootstrap/ld_bs.csv", row.names = FALSE)
}
stopifnot(exists("ld_bs") == TRUE)

# Merging bootstrap corrected scores into ld_EVAL
ld_EVAL = merge.data.frame(ld_EVAL, ld_bs, by="id", all.x = T)
ld_EVAL$ld_eff.bs.is2.cZ = ld_EVAL$ld_bs.correst

#### Estimating and adjusting fpr Z-variables ####
ld_EVAL$ld_cnorm = ld_EVAL$ld_TOTXDEA * ld_EVAL$ld_eff.s1.cb  # Previously used as scaling factor

# Coast / Kyst
  Geovar.ldz = cbind(ld_EVAL[,c("ldz_salt", "ldz_coast_wind", "ldz_water")])
  ld_EVAL$pca_coast = z.est(geovar.in = Geovar.ldz, restricted.obs = Geovar.ldz)*-1 
  cor(Geovar.ldz) 
  pca_coast_reg <- lm( ld_EVAL$pca_coast ~ ld_EVAL$ldz_salt + ld_EVAL$ldz_coast_wind + ld_EVAL$ldz_water)

# Leafinc / L?vfall 
  Geovar.ldz = cbind(ld_EVAL[,c("ldz_incline", "ldz_prod", "ldz_snow_trees", "ldz_forest_broadleaf")])
  ld_EVAL$pca_leafinc = z.est(geovar.in = Geovar.ldz, restricted.obs = Geovar.ldz) 
  cor(Geovar.ldz)   
  pca_leafinc_reg <- lm( ld_EVAL$pca_leafinc ~ ld_EVAL$ldz_incline + ld_EVAL$ldz_prod + 
                           ld_EVAL$ldz_snow_trees + ld_EVAL$ldz_forest_broadleaf)

# Frost
  Geovar.ldz = cbind(ld_EVAL[,c("ldz_snowdrift", "ldz_snow_400", "ldz_wind_99", "ldz_frosthours")])
  ld_EVAL$pca_frost = z.est(geovar.in = Geovar.ldz, restricted.obs = Geovar.ldz) 
  cor(Geovar.ldz)
  pca_frost_reg <- lm( ld_EVAL$pca_frost ~ ld_EVAL$ldz_snowdrift + ld_EVAL$ldz_snow_400 + 
                           ld_EVAL$ldz_wind_99 + ld_EVAL$ldz_frosthours)

# Calculating Z-variable coefficients
  Geovar.ldz = cbind(ld_EVAL[,c("ldz_forest_mixed_conf","pca_leafinc", "pca_coast","pca_frost")]) 
  cor(Geovar.ldz)    

# Creating new vector for bootstrapped efficiency scores
  ld_eff.bs = ld_EVAL$ld_eff.bs.is2.cZ
  names(ld_eff.bs) = names(X.avg.ld)
  
  ldz.reg = Zvar1(x=X.avg.ld, z=Geovar.ldz, eff=ld_eff.bs, lambda = ld_lambda.avg,
                  id = names(X.avg.ld), id.out = NULL)
  ldz.coeff = ldz.reg$coeff 
  
  print(summary(ldz.reg$res.regr.NVE))

# Adjusting efficiency scores from stage 1, using difference in Z-value relative to target unit
  eff.cb.avg.ld = dea.cb.avg.ld$eff
  ld_s2 = Zvar2(x = X.avg.ld, eff = eff.cb.avg.ld, id = names(X.avg.ld),
                lambda = ld_lambda, coeff = ldz.coeff, z = Geovar.ldz)
  ld_EVAL$ld_eff.s2.cb = ld_s2$eff.corr

# Korreksjonen kan maks være 40 prosentpoeng begge veier (opp- og nedjustering)
ld_EVAL <- ld_EVAL %>% 
    mutate(ld_eff.s2.cb = case_when(
      ld_eff.s1.cb - ld_eff.s2.cb > 0.40 ~ ld_eff.s1.cb + 0.40,  # Hvis differansen er over +40 pp, settes korrigering til 40%
      ld_eff.s1.cb - ld_eff.s2.cb < -0.40 ~ ld_eff.s1.cb - 0.40, # Hvis differansen er over -40 pp, settes korrigering til -40%
      TRUE ~ ld_s2$eff.corr                                      # Hvis ingen av betingelsene over er oppfylt, korriger med verdien fra trinn 2
  ))
  
# No Z-variable adjustment for regional grid
  rd_EVAL$rd_eff.s2.cb = rd_EVAL$rd_eff.s1.cb
