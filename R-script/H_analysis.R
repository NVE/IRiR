# Create table for Torgeresens Rho # Incorrect

ld_Torg.Rho.avg = ToRho(x = X.avg.ld, lambda = ld_lambda.avg)$Torg.Rho

ld_Torg.Rho.avg = as.data.frame(ld_Torg.Rho.avg)
ld_Torg.Rho.avg = tibble::rownames_to_column(ld_Torg.Rho.avg, "id")
ld_Torg.Rho.avg$id = as.numeric(ld_Torg.Rho.avg$id)

id_comp = ld_EVAL[,c("id", "comp")]

ld_Torg.Rho.avg = inner_join(id_comp,ld_Torg.Rho.avg, by =  "id")

rd_Torg.Rho.avg = ToRho(x = X.avg.rd, lambda = rd_lambda.avg)$Torg.Rho

rd_Torg.Rho.avg = as.data.frame(rd_Torg.Rho.avg)
rd_Torg.Rho.avg = tibble::rownames_to_column(rd_Torg.Rho.avg, "id")
rd_Torg.Rho.avg$id = as.numeric(rd_Torg.Rho.avg$id)

id_comp = rd_EVAL[,c("id", "comp")]

rd_Torg.Rho.avg = inner_join(id_comp,rd_Torg.Rho.avg, by =  "id")

# Cost norm contribution and cost norm share for each DMU.

ld_PI = PeerI(x = X.avg.ld, eff = eff.cb.avg.ld, id = names(X.avg.ld), lambda = ld_lambda.avg)
rd_PI = PeerI(x = X.avg.rd, eff = eff.cb.avg.rd, id = names(X.avg.rd), lambda = rd_lambda.avg)

ld_ncs = ld_PI$cost.norm.share
ld_ncs = as.data.frame(ld_ncs[, colSums(ld_ncs) > 0])
rd_ncs = rd_PI$cost.norm.share
rd_ncs = as.data.frame(rd_ncs[, colSums(rd_ncs) > 0])

ld_ncs = tibble::rownames_to_column(ld_ncs, "id")
rd_ncs = tibble::rownames_to_column(rd_ncs, "id")
ld_ncs$id = as.numeric(ld_ncs$id)
rd_ncs$id = as.numeric(rd_ncs$id)

#---------------------------------------------------
#Create Dataframe with Key figures for each DSO.

KeyFig = RevCap[, c("orgn", "comp", "id", "lrt_RC.pre.recal", "lrt_cn.pre.recal")]

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
rm(ld_ncs, rd_ncs)

ld_EVAL$ld_eff.s3.avg = ld_calib$eff.cal
KF_ldE = ld_EVAL[,c("id", "ld_eff.s1.avg", "ld_eff.s2.avg", "ld_eff.s3.avg")]
rd_EVAL$rd_eff.s3.avg = rd_calib$eff.cal
KF_rdE = rd_EVAL[,c("id", "rd_eff.s1.avg", "rd_eff.s2.avg", "rd_eff.s3.avg")]
KeyFig = left_join(KeyFig, KF_ldE, by="id")
KeyFig = left_join(KeyFig, KF_rdE, by="id")
rm(KF_ldE, KF_rdE)


ld_env.cond = ld_EVAL[,c("id", "ldz_hvug.s", "ldz_f4", "ldz_Geo1", "ldz_Geo2", "ldz_Geo3")]
rd_env.cond = rd_EVAL[,c("id", "rdz_Geo1")]
KeyFig = left_join(KeyFig, ld_env.cond, by = "id")
KeyFig = left_join(KeyFig, rd_env.cond, by = "id")
rm(ld_env.cond, rd_env.cond)

ld_z.diff = as.data.frame(ld_s2$z.diff)
colnames(ld_z.diff) = paste("diff_tu", colnames(ld_z.diff), sep = "_")
ld_z.diff = tibble::rownames_to_column(ld_z.diff, "id")
ld_z.diff$id = as.numeric(ld_z.diff$id)
rd_z.diff = as.data.frame(rd_s2$z.diff)
colnames(rd_z.diff) = ("diff_tu_rdz_Geo1")
rd_z.diff = tibble::rownames_to_column(rd_z.diff, "id")
rd_z.diff$id = as.numeric(rd_z.diff$id)

KeyFig = left_join(KeyFig, ld_z.diff, by="id")
KeyFig = left_join(KeyFig, rd_z.diff, by="id")
rm(ld_z.diff, rd_z.diff)

KeyFigorgn = KeyFig[KeyFig$orgn == comp.org, ]