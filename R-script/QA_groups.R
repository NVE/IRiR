## Groups

# OOTO - Compared to historical own historical average
# Number of subsrcibers less than 500 (ld_sub < 500) or less than 100 km hv lines (ld_hv < 100)

ld_OOTO_candidates = filter(dat, y == y.cb & fha_ld_TOTXDEA > 0 & ld_sub<500 & ld_hv<100)

ld_OOTO_candidates[,c("id", "comp", "fha_ld_TOTXDEA", "ld_sub", "ld_hv")]

dat$rd_wv.sum = dat$rd_wv.ol + dat$rd_wv.uc + dat$rd_wv.sc + dat$rd_wv.ss
rd_OOTO_candidates = filter(dat, y == y.cb, fha_rd_TOTXDEA >0 & rd_wv.sum<4000 |
                                    y == y.cb, fha_rd_TOTXDEA >0 & rd_wv.ol==0)

rd_OOTO_candidates[,c("id", "comp", "fha_rd_TOTXDEA", "rd_wv.sum", "rd_wv.ol")]

tidl.i.rd_ooto = c(10, 18, 147, 161, 222, 274, 287, 307, 743)

dat[dat$id %in% tidl.i.rd_ooto & dat$y == 2016,c("id", "comp", "fha_rd_TOTXDEA", "rd_wv.sum", "rd_wv.ol")]
rd_sep.eval_candidates = filter(dat, y == y.cb, fha_rd_TOTXDEA > 7000 & fha_rd_TOTXDEA < 15000)
rd_sep.eval_candidates[,c("id", "comp", "fha_rd_TOTXDEA")]

tidl.i.rd_sep.eval = c(7, 103, 106, 138, 173, 184, 206, 238, 271, 343, 591, 625)

dat[dat$id %in% tidl.i.rd_sep.eval & dat$y == 2016,c("id", "comp", "fha_rd_TOTXDEA")]
