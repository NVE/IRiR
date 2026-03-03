#### Printing results ####

# Grunnlagsdata LD
        Grunnlagsdata_ld = dat[, c("orgn", "id.y", "id", "y", "comp", "fp_ld_OPEXxS", "fp_ld_sal", "fp_ld_sal.cap", "av_ld_pens", 
                                   "av_ld_pens.eq", "av_ld_impl", "fp_ld_391", "fp_ld_OPEX", "ld_rab.gf", "ld_dep.gf", "ld_rab.sf", 
                                   "ld_dep.sf", "fp_ld_cens", "ld_nl.NOK", "ld_TOTXDEA", "ld_sub", "ld_hv", "ld_ss", "ld_EVAL",
                                   "ldz_salt", "ldz_coast_wind", "ldz_water", "ldz_incline", "ldz_prod", "ldz_snow_trees",
                                   "ldz_forest_broadleaf", "ldz_snowdrift", "ldz_snow_400","ldz_wind_99", "ldz_frosthours",
                                   "ldz_forest_mixed_conf")]
        
        Grunnlagsdata_ld = Grunnlagsdata_ld[Grunnlagsdata_ld$y %in% y.avg,]

# Resultater LD
        Resultater_ld = ld_EVAL[,c("orgn", "id.y", "id", "y", "comp", "ld_cb", "ld_eff.s1.cb", "ld_eff.s2.cb", 
                                   "ldz_forest_mixed_conf", "pca_frost", "pca_leafinc", "pca_coast", "ld_cnorm")]
        Resultater_ld = left_join(Resultater_ld, ld_z.diff, by="id")
        Resultater_ld = left_join(Resultater_ld, ld_ncs, by="id")
        OOTO_ld       = ld_OOTO[,c("orgn", "id.y", "id", "y", "comp", "ld_cb", "ld_eff.OOTO")]
        Av.eff_ld     = ld_AV.EFF[,c("orgn", "id.y", "id", "y", "comp", "ld_cb", "ld_AV.EFF")]
        
# Grunnlagsdata RD
        Grunnlagsdata_rd = dat[, c("orgn", "id.y", "id", "y", "comp", "fp_rd_OPEXxS", "fp_rd_sal", "fp_rd_sal.cap", "av_rd_pens", 
                                   "av_rd_pens.eq", "av_rd_impl", "fp_rd_391", "fp_rd_cga.tidl.coord", "fp_rd_OPEX", "rd_rab.gf", "rd_dep.gf", 
                                   "rd_rab.sf", "rd_dep.sf", "fp_rd_cens", "rd_TOTXDEA", "rd_wv.ol", "rd_wv.sc", "rd_wv.ss", "rd_wv.uc", "rd_EVAL")]
        Grunnlagsdata_rd = Grunnlagsdata_rd[Grunnlagsdata_rd$y %in% y.avg,]

# Resultater RD
        Resultater_rd = rd_EVAL[,c("orgn", "id.y", "id", "y", "comp", "rd_cb", "rd_eff.s1.cb", "rd_eff.s2.cb")]
        Resultater_rd = left_join(Resultater_rd, rd_ncs, by="id")
        OOTO_rd       = rd_OOTO[,c("orgn", "id.y", "id", "y", "comp", "rd_cb", "rd_eff.OOTO")]
        Av.eff_rd     = rd_AV.EFF[,c("orgn", "id.y", "id", "y", "comp", "rd_cb", "rd_AV.EFF")]
        
# Print results to excel
        ld_print = list("Datagrunnlag_LD" = Grunnlagsdata_ld, "Resultater_LD" = Resultater_ld, "Spesialmodell_LD" = OOTO_ld, 
                        "Til_gjennomsnitt_LD" = Av.eff_ld)
        rd_print = list("Datagrunnlag_RD" = Grunnlagsdata_rd, "Resultater_RD" = Resultater_rd, "Spesialmodell_RD" = OOTO_rd, 
                        "Til_gjennomsnitt_RD" = Av.eff_rd)
        
        writexl::write_xlsx(ld_print, paste0(run_dir,"/Data_Resultater_LD.xlsx"))
        writexl::write_xlsx(rd_print, paste0(run_dir,"/Data_Resultater_RD.xlsx"))
        
        
#### Uttrekk til inntektsrammeark ####
        Grunnlagsdata_lrt = dat[which(dat$y %in% y.cb), c("id.y", "orgn", "id", "comp", "rd_cga.tidl.coord", "fp_ld_OPEX", "ld_dep.sf", "ld_rab.sf", "ld_RAB", "ld_nl", 
                                                          "ld_cens", "fp_rd_OPEX", "rd_dep.sf", "rd_rab.sf", "rd_RAB", "rd_nl", "rd_cens",  "fp_t_OPEX", 
                                                          "t_dep.sf", "t_rab.sf", "t_cens", "pnl.rc", "ap.t_2")]                
        
Eff_ooto_rd = rd_OOTO[,c("id", "rd_eff.OOTO")]
Eff_ooto_ld = ld_OOTO[,c("id", "ld_eff.OOTO")]

Eff_ld = ld_EVAL[,c("id", "ld_eff.s2.cb")]
Eff_rd = rd_EVAL[,c("id", "rd_eff.s2.cb")]
Grunnlagsdata_lrt = left_join(Grunnlagsdata_lrt, Eff_ooto_ld, by="id")
Grunnlagsdata_lrt = left_join(Grunnlagsdata_lrt, Eff_ooto_rd, by="id")
Grunnlagsdata_lrt = left_join(Grunnlagsdata_lrt, Eff_ld, by="id")
Grunnlagsdata_lrt = left_join(Grunnlagsdata_lrt, Eff_rd, by="id")

write_xlsx(Grunnlagsdata_lrt, paste0(run_dir,"/Til inntektsrammeark.xlsx"))


#### Koeffisienter ####
differ = as.data.frame(ldz.reg$z.diff)
differ = cbind(ld_EVAL$id, differ)
koeffer = as.data.frame(ldz.reg$coeff)
koeffer <- tibble::rownames_to_column(koeffer)

pca_leafinc_reg <- as.data.frame(pca_leafinc_reg$coefficients)
pca_leafinc_reg <- tibble::rownames_to_column(pca_leafinc_reg)


pca_coast_reg <- as.data.frame(pca_coast_reg$coefficients)
pca_coast_reg <- tibble::rownames_to_column(pca_coast_reg)

pca_frost_reg <- as.data.frame(pca_frost_reg$coefficients)
pca_frost_reg <- tibble::rownames_to_column(pca_frost_reg)

list <- list(koeffer, pca_coast_reg, pca_frost_reg, pca_leafinc_reg)
writexl::write_xlsx(list, paste0(run_dir,"/koeffisienter.xlsx"))


#### Til Frisch ####
ld_frisch_fha <- unique( dat[dat$ld_EVAL == 1 , c("id", "fha_ld_TOTXDEA", "fha_ld_sub", "fha_ld_hv", "fha_ld_ss")])

for (i in which(dat$id %in% rd_sep.eval)){  
        dat[i,"rd_EVAL"]  = 0   
}  

rd_frisch_fha <- unique( dat[dat$rd_EVAL == 1, c("id", "fha_rd_TOTXDEA" , "fha_rd_wv.ol" , "fha_rd_wv.uc" ,"fha_rd_wv.sc", "fha_rd_wv.ss" )])

write.csv(ld_frisch_fha, file = paste0(run_dir,"/ld_frisch_fha.csv"))
write.csv(rd_frisch_fha, file = paste0(run_dir,"/rd_frisch_fha.csv"))
