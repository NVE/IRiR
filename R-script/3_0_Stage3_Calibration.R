# Stage 3 - calibration

#Oppretter kostnadsgrunnlag til kalibrering
d_tilDEA$d_kostnadsgrlag <- ((d_tilDEA$fp_d_DV*faktisk.aar.kpiafaktor) + (d_tilDEA$d_akg*nve.rente.t) + 
                                     d_tilDEA$d_avs + (d_tilDEA$d_kile*faktisk.aar.kpifaktor) + 
                                     (d_tilDEA$d_nettap*nettapspris.ir) - (d_tilDEA$d_grs.cost*faktisk.aar.kpifaktor))



d_calib=NVE_cal(eff=d_tilDEA$d_deares_til_kal, cost_base=d_tilDEA$d_kostnadsgrlag, RAB=d_tilDEA$d_akg)

r_tilDEA$r_kostnadsgrlag <- ((r_tilDEA$fp_r_DV*faktisk.aar.kpiafaktor) + (r_tilDEA$r_akg*nve.rente.t) + 
                                     r_tilDEA$r_avs + (r_tilDEA$r_kile*faktisk.aar.kpifaktor))

r_calib=NVE_cal(eff=r_tilDEA$r_deares_tilkal, cost_base = r_tilDEA$r_kostnadsgrlag, RAB = r_tilDEA$r_akg)