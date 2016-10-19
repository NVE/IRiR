#### Export til Excel trinn 1 ####

# Gir manglende observasjoner en verdi lik null
dat[is.na(dat)] <- 0

# OBSOBS: sjekk at datarammene inneholder riktige variabler når alle variabler er laget

# Konstruerer datarammer til Excel-arkene for D-nett trinn 1
d_grunnlagsdata_trinn1 = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_dv, dat$d_391, 
                                    dat$d_utred, dat$d_DV, dat$d_AKG, dat$d_abakg, dat$d_akg, dat$d_avs, 
                                    dat$d_abavs, dat$d_avs, dat$d_kile, dat$d_nettap, 
                                    dat$d_nettapkr, dat$d_grs.cost, dat$d_TOTXDEA, dat$d_ab, 
                                    dat$d_hs, dat$d_ns)

d_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_DVxL, dat$d_lonn, 
                         dat$d_lonnakt, dat$d_pensj,  dat$fp_d_pensj, 
                         dat$d_pensjek, dat$d_impl, dat$fp_d_impl, dat$av_fp_d_pensj, 
                         dat$fp_d_pensjek, dat$av_fp_d_pensjek, 
                         dat$av_fp_d_impl, dat$d_pensjkostgrlag, dat$d_dv, dat$d_dv_2012)

d_gjsnittfront = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$fp_d_dv, dat$fp_d_391, 
                            dat$fp_d_DV, dat$fp_d_kile, dat$av_d_TOTXDEA, dat$av_d_ab, 
                            dat$av_d_hs, dat$av_d_ns) 
#                           dat$d_snittfront*)
# 
# d_DEAdata = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$frontlov, 
#                        dat$d_snittfront_d_TOTXDEA, dat$d_snittfront_d_ab, dat$d_snittfront_d_hs, 
#                        d_snittfront_d_ns)
# 
# d_vektberegning = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_frontlov_hoved, 
#                              dat$frontlov, dat$d_vekt*, dat$d_normkostandel*, dat$d_kostbidrag*)
# 
# d_DEAresultater = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$d_frontlov_hoved, 
#                              dat$d_score_snittfront, dat$d_score_spesial, dat$d_dea_til2trinn)

d_grunnlagsdata_trinn1 = d_grunnlagsdata_trinn1[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
d_forslagDV = d_forslagDV[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
d_gjsnittfront = d_gjsnittfront[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_DEAdata = d_DEAdata[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_vektberegning = d_vektberegning[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# d_DEAresultater = d_DEAresultater[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]

# Konstruerer datarammer til Excel-arkene for R-nett trinn 1
r_grunnlagsdata_trinn1 = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_akg, 
                                    dat$r_abakg, dat$r_AKG, dat$r_avs, dat$r_abavs, dat$r_AVS, 
                                    dat$r_kile, dat$r_TOTXDEA, dat$r_vluft, dat$r_vjord,
                                    dat$r_vsjo, dat$r_vgrs)

r_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_DVxL, dat$r_lonn, 
                         dat$r_lonnakt, dat$r_pensj, dat$fp_r_pensj, dat$av_fp_r_pensj, 
                         dat$r_pensjek, dat$fp_r_pensjek, dat$av_fp_r_pensjek, dat$r_impl, 
                         dat$fp_r_impl, dat$av_fp_r_impl, dat$r_pensjkostgrlag, dat$r_dv, 
                         dat$r_dv_2012)

r_gjsnittfront = data.frame(dat$idaar, dat$idaar, dat$selskap, dat$fp_r_dv, dat$fp_r_391, 
                            dat$fp_r_utred, dat$fp_r_DV, dat$fp_r_kile, dat$av_r_vsjo, 
                            dat$av_r_vluft, dat$av_r_vjord, dat$av_r_vgrs, dat$av_r_TOTXDEA) 
#                           dat$r_snittfront*)

# r_DEAdata = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$frontlov, dat$r_snittfront*)
# 
# r_vektberegning = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_frontlov_hoved, 
#                              dat$frontlov, dat$r_vekt*, dat$r_normkostandel*, dat$r_kostbidrag*)
# 
# r_DEAresultater = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$r_frontlov_hoved, 
#                              dat$r_score_snittfront, dat$r_score_spesial, dat$r_dea_til2trinn)
# 
# s_forslagDV = data.frame(dat$idaar, dat$id, dat$aar, dat$selskap, dat$s_DVxL, dat$s_lonn, 
#                          dat$s_lonnakt, dat$s_pensj, dat$fp_s_pensj_faktisk, dat$fp_s_pensj, 
#                          dat$av_fp_s_pensj, dat$s_pensjek, dat$fp_s_pensjek, 
#                          dat$fp_s_pensjek_faktisk, dat$av_fp_s_pensjek, dat$s_impl, 
#                          dat$fp_s_impl_faktisk, dat$fp_s_impl, dat$av_fp_s_impl, 
#                          dat$s_pensjkostgrlag, dat$s_dv, dat$s_dv_2012)

r_grunnlagsdata_trinn1 = r_grunnlagsdata_trinn1[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
r_forslagDV = r_forslagDV[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
r_gjsnittfront = r_gjsnittfront[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_DEAdata = r_DEAdata[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_vektberegning = r_vektberegning[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]
# r_DEAresultater = r_DEAresultater[dat$aar %in% snitt.aar | dat$aar == faktisk.aar,]


# Lager Excel-filer
DataDEA1D = createWorkbook(type="xlsx")
side1D = createSheet(wb = DataDEA1D, sheetName = "d_grunnlagsdata_trinn1")
side2D = createSheet(wb = DataDEA1D, sheetName = "d_forslagDV")
side3D = createSheet(wb = DataDEA1D, sheetName = "d_gjsnittfront")
# side4D = createSheet(wb = Data og DEA 1D, sheetName = "d_dataDEA")
# side5D = createSheet(wb = Data og DEA 1D, sheetName = "d_vektberegning")
# side6D = createSheet(wb = Data og DEA 1D, sheetName = "d_DEAresultater")
addDataFrame(x = d_grunnlagsdata_trinn1, sheet = side1D)
addDataFrame(x = d_forslagDV, sheet = side2D)
addDataFrame(x = d_gjsnittfront, sheet = side3D)
# addDataFrame(x = d_DEAdata, sheet = side4D)
# addDataFrame(x = d_vektberegning, sheet = side5D)
# addDataFrame(x = d_DEAresultater, sheet = side6D)
saveWorkbook(DataDEA1D, "Data og DEAresultater trinn 1 Distribusjonsnett.xlsx")
# 
DataDEA1R = createWorkbook()
side1R = createSheet(wb = DataDEA1R, sheetName = "r_grunnlagsdata_trinn1")
side2R = createSheet(wb = DataDEA1R, sheetName = "r_forslagDV")
side3R = createSheet(wb = DataDEA1R, sheetName = "r_gjsnittfront")
# side4R = createSheet(wb = DataDEA1R, sheetName = "r_dataDEA")
# side5R = createSheet(wb = DataDEA1R, sheetName = "r_vektberegning")
# side6R = createSheet(wb = DataDEA1R, sheetName = "r_DEAresultater")
addDataFrame(x = r_grunnlagsdata_trinn1, sheet = side1R)
addDataFrame(x = r_forslagDV, sheet = side2R)
addDataFrame(x = r_gjsnittfront, sheet = side3R)
# addDataFrame(x = r_DEAdata, sheet = side4R)
# addDataFrame(x = r_vektberegning, sheet = side5R)
# addDataFrame(x = r_DEAresultater, sheet = side6R)
saveWorkbook(DataDEA1R, "Data og DEAresultater trinn 1 Regionalnett.xlsx")
# 
# DataS = createWorkbook()
# side1S = createSheet(wb=Data S, sheetName = "s_forslagDV")
# addDataFrame(x = s_forslagDV, sheet = side1S)
# saveWorkbook(DataS, "Data Sentralnett")