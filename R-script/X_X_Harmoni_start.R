#Harmoni alpha.

#som skal fusjoneres.
#Selskap1 Alta
fun.comp1 = c(971029390)
#Selskap2 Andøy
fun.comp2 = c(971048611)


dat$dr_fusj.ruter = dat$dr_antall.ruter
dat$rr_fusj.ruter = dat$rr_antall.ruter

harm.var_sum = c("d_391", "d_DVxL", "d_ab", "d_abavs", "d_abbfv", "d_aoey1", "d_avs",
                 "d_bfv", "d_grs", "d_hs", "d_hsjord", "d_hsll", "d_hssjo", "d_impl",
                 "d_kile", "d_lonn", "d_lonnakt", "d_nettap", "d_ns", "d_pensj", 
                 "d_pensjek", "d_skytelse", "d_utred", "r_391", "r_DVxL", "r_abavs", 
                 "r_abavs_melk", "r_abbfv", "r_abbfv_melk", "r_avs", "r_bfv", "r_impl",
                 "r_kile", "r_lonn", "r_lonnakt", "r_nettap", "r_pensj", "r_pensjek",
                 "r_utred", "r_vgrs", "r_vjord", "r_vluft", "r_vsjo", "s_391", "s_DVxL",
                 "s_avs", "s_bfv", "s_impl", "s_kile", "s_lonn", "s_lonnakt", "s_pensj",
                 "s_pensjek", "s_vgrs", "s_vjord", "s_vluft", "s_vsjo", "dr_fusj.ruter",
                 "rr_fusj.ruter")


# KPIA-justering
var_kpia = c("d_391", "r_391", "s_391", "d_pensj", "r_pensj", "s_pensj", "d_pensjek", "r_pensjek", "s_pensjek", 
             "d_impl", "r_impl", "s_impl", "d_DVxL", "d_utred", "r_DVxL", "r_utred", "s_DVxL", "d_lonn", 
             "d_lonnakt", "r_lonn", "r_lonnakt", "s_lonn", "s_lonnakt")
fp_var_kpia = paste("fp_", var_kpia, sep = "")
dat = cbind(dat, t(matrix(NA, ncol = nrow(dat), nrow = length(var_kpia), dimnames = list(var_kpia = fp_var_kpia))))
for(c in 1:length(var_kpia))
  for(r in 1:nrow(dat))
    dat[r, fp_var_kpia[c]] = dat[r, var_kpia[c]] * kpia[as.character(faktisk.aar)] / kpia[as.character(dat[r, "aar"])]