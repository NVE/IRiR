
# Cost norm contribution and cost norm share for each DSO

ld_PI = PeerI(x = X.avg.ld, eff = eff.cb.avg.ld, id = names(X.avg.ld), lambda = ld_lambda)
rd_PI = PeerI(x = X.avg.rd, eff = eff.cb.avg.rd, id = names(X.avg.rd), lambda = rd_lambda)

ld_ncs = ld_PI$cost.norm.share
ld_ncs = as.data.frame(ld_ncs[, colSums(ld_ncs) > 0])
rd_ncs = rd_PI$cost.norm.share
rd_ncs = as.data.frame(rd_ncs[, colSums(rd_ncs) > 0])

ld_ncs = tibble::rownames_to_column(ld_ncs, "id")
rd_ncs = tibble::rownames_to_column(rd_ncs, "id")
ld_ncs$id = as.numeric(ld_ncs$id)
rd_ncs$id = as.numeric(rd_ncs$id)

# Create data frame with key figures for each DSO

KeyFig = RevCap[, c("orgn", "comp", "id", "lrt_RC.pos.recal", "lrt_cn.pre.recal")]

v = colnames(ld_ncs) %in% KeyFig$id
w = KeyFig$id %in% colnames(ld_ncs)
colnames(ld_ncs)[v] = KeyFig$comp[w]
colnames(ld_ncs)[v] = paste("ld_ncs", colnames(ld_ncs[v]), sep = "_")
v = colnames(rd_ncs) %in% KeyFig$id
w = KeyFig$id %in% colnames(rd_ncs)
colnames(rd_ncs)[v] = KeyFig$comp[w]
colnames(rd_ncs)[v] = paste("rd_ncs", colnames(rd_ncs[v]), sep = "_")

KeyFig = left_join(KeyFig, ld_ncs, by = "id")
KeyFig = left_join(KeyFig, rd_ncs, by = "id")

ld_EVAL$ld_eff.s3.cb = ld_calib$eff.cal
KF_ldE = ld_EVAL[,c("id", "ld_eff.s1.cb", "ld_eff.s2.cb", "ld_eff.s3.cb")]
rd_EVAL$rd_eff.s3.cb = rd_calib$eff.cal
KF_rdE = rd_EVAL[,c("id", "rd_eff.s1.cb", "rd_eff.s2.cb", "rd_eff.s3.cb")]
KeyFig = left_join(KeyFig, KF_ldE, by="id")
KeyFig = left_join(KeyFig, KF_rdE, by="id")

ld_env.cond = ld_EVAL[,c("id",
                         "ldz_salt",
                         "ldz_coast_wind",
                         "ldz_water",
                         "ldz_incline",
                         "ldz_prod",
                         "ldz_snow_trees",
                         "ldz_forest_broadleaf",
                         "ldz_snowdrift",
                         "ldz_snow_400",
                         "ldz_wind_99",
                         "ldz_frosthours",
                         "ldz_forest_mixed_conf",
                         "pca_frost",
                         "pca_leafinc",
                         "pca_coast")]

KeyFig = left_join(KeyFig, ld_env.cond, by = "id")


ld_z.diff = as.data.frame(ld_s2$z.diff)
colnames(ld_z.diff) = paste("diff_tu", colnames(ld_z.diff), sep = "_")
ld_z.diff = tibble::rownames_to_column(ld_z.diff, "id")
ld_z.diff$id = as.numeric(ld_z.diff$id)
KeyFig = left_join(KeyFig, ld_z.diff, by="id")

rm(KF_ldE, KF_rdE, ld_env.cond)

write_xlsx(KeyFig, paste0(run_dir,"/KeyFig.xlsx"))

#### Information om peers ####
df_rd_lambda = as.data.frame(rd_lambda)
ref_rd = (df_rd_lambda[, colSums(df_rd_lambda) > 0])
id_ref_rd = as.numeric(colnames(ref_rd))
referenter_rd = dat[dat$id %in% id_ref_rd[!id_ref_rd %in% rd_sep.eval] & dat$y == y.cb, 
                            c("id", "comp", "fha_rd_TOTXDEA", "fha_rd_wv.ol", "fha_rd_wv.sc", "fha_rd_wv.uc", "fha_rd_wv.ss")]
referenter_rd

df_ld_lambda =  as.data.frame(ld_lambda)
ref_ld = (df_ld_lambda[, colSums(df_ld_lambda) > 0])
id_ref_ld = as.numeric(colnames(ref_ld))
referenter_ld = dat[dat$id %in% id_ref_ld & dat$y == y.cb, c("id", "comp", "fha_ld_TOTXDEA", "fha_ld_sub", "fha_ld_hv", "fha_ld_ss")]
referenter_ld