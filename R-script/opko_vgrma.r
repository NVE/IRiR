new.rd_wv.ss = read.csv(file="new_rd_wv.ss.csv", sep=",")
# IDs to simplify scripts and aid analysts
id = read.csv("./Data/BaseData/id.csv", sep = ",")
# Dedicate IDs to base data using merge
new.rd_wv.ss = merge.data.frame(new.rd_wv.ss, id, by = "orgn", all.x = TRUE)
new.rd_wv.ss$id.y <- paste(new.rd_wv.ss$id, new.rd_wv.ss$y, sep="")
new.rd_wv.ss$id.y <- as.numeric(new.rd_wv.ss$id.y)
new.rd_wv.ss = dplyr::select(new.rd_wv.ss, -id_erapp2, -name)

old.rd_wv.ss = dat[dat$y %in% y.avg, c("id", "id.y", "rd_wv.ss")]


new.rd_wv.ss = left_join(new.rd_wv.ss, old.rd_wv.ss, by = "id.y")
new.rd_wv.ss = dplyr::select(new.rd_wv.ss, -id.y.y, -id.x)
new.rd_wv.ss$diff.2016 = new.rd_wv.ss$new_rd_vw.ss - new.rd_wv.ss$rd_wv.ss

rd_wv.ss.c = read.csv2(file="rd_SSwComp.csv", sep=",", dec = ".")

rd_wv.ss.c = rd_wv.ss.c[, c("id.y", "rd_wv.ss.c")]

dat = left_join(dat, rd_wv.ss.c, by="id.y")



dat$rd_wv.ss = ifelse(is.na(dat$rd_wv.ss.c), dat$rd_wv.ss, dat$rd_wv.ss.c)

dat = dplyr::select(dat, -rd_wv.ss.c)
