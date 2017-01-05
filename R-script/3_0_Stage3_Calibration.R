# Stage 3 - calibration

#Oppretter kostnadsgrunnlag til kalibrering
d_tilDEA$d_kostnadsgrlag <- ((d_tilDEA$fp_d_DV*faktisk.aar.kpiafaktor) + (d_tilDEA$d_akg*nve.rente.t) + 
                                     d_tilDEA$d_avs + (d_tilDEA$d_kile*faktisk.aar.kpifaktor) + 
                                     (d_tilDEA$d_nettap*nettapspris.ir) - (d_tilDEA$d_grs.cost*faktisk.aar.kpifaktor))




