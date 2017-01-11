#Revenue Cap - Calculation

d_tilDEA$d_sumcost = d_tilDEA$d_DV*faktisk.aar.kpiafaktor +
                        d_tilDEA$d_kile*faktisk.aar.kpifaktor +
                        d_tilDEA$d_avs + d_tilDEA$d_nettap*nettapspris.ir

r_tilDEA$r_sumcost = (r_tilDEA$r_DV*faktisk.aar.kpiafaktor +
                        r_tilDEA$r_kile*faktisk.aar.kpifaktor + 
                        r_tilDEA$r_utred*faktisk.aar.kpiafaktor +
                        r_tilDEA$r_avs)


d_tilDEA$d_costRC = d_tilDEA$d_DV*faktisk.aar.kpiafaktor +
                        d_tilDEA$d_kile*faktisk.aar.kpifaktor +
                        d_tilDEA$d_avs + d_tilDEA$d_akg*rente.ir +
                        d_tilDEA$d_nettap*nettapspris.ir

r_tilDEA$r_costRC = r_tilDEA$r_DV*faktisk.aar.kpiafaktor +
                        r_tilDEA$r_kile*faktisk.aar.kpifaktor +
                        r_tilDEA$r_avs + r_tilDEA$r_akg*rente.ir



                        