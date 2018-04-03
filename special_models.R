dat$fha_rd_wv.sum = dat$fha_rd_wv.ol + dat$fha_rd_wv.ss + dat$fha_rd_wv.uc + dat$fha_rd_wv.sc

rd_rd.sep.banckers = as.data.frame((dat[dat$y == 2015 & dat$fha_rd_TOTXDEA <= 7000 &  dat$fha_rd_TOTXDEA > 0 | 
                                          dat$y == 2015 & dat$fha_rd_wv.sum >= 4000 & dat$fha_rd_TOTXDEA > 0 , 
                                        c("id", "orgn", "comp", "fha_rd_TOTXDEA", "fha_rd_wv.sum", "fha_rd_wv.ol")]))


rd_rd.sep.candi = as.data.frame((dat[dat$y == 2015 & dat$fha_rd_TOTXDEA <= 15000 & dat$fha_rd_TOTXDEA > 0 | 
                                     dat$y == 2015 & dat$fha_rd_wv.sum >= 4000 & dat$fha_rd_TOTXDEA > 0 , 
                                     c("id", "orgn", "comp", "fha_rd_TOTXDEA", "fha_rd_wv.sum", "fha_rd_wv.ol")]))

rd_OOTO.candi = as.data.frame((dat[dat$fha_rd_wv.ol == 0 & dat$fha_rd_TOTXDEA > 0 & dat$y == 2015, 
                                   c("id", "orgn", "comp", "fha_rd_TOTXDEA", "fha_rd_wv.sum" , "fha_rd_wv.ol")]))