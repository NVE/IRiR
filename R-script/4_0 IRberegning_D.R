# Her lages kostnadsgrunnlagene som ligger til grunn for inntektsrammeberegninen. 
# Her er det 4 typer;
# a. Sum kostnader (drs_sumkostnader) 
# b. Kostnadsgrunnlag drs_kostIR
# c. Kostnadsgrunnlag uten Rnettap og utred (drs_kostIR_ex_rnettap_og_rsutred)
# d. Så beregnes faktiske kostnader i t-2 (drs_KOSTGRL_faktisk) 

# a. Sum kostnader
d_tilDEA$d_sumkostnader <- ((d_tilDEA$d_DV*curr_aar_kpiafaktor) + (d_tilDEA$d_kile*curr_aar_kpifaktor) + 
                             d_tilDEA$d_avs + (d_tilDEA$d_nettap*nettapspris.ir))

# b. Kostnadsgrunnlag
d_tilDEA$d_kostIR <- ((d_tilDEA$d_DV*curr_aar_kpiafaktor) + (d_tilDEA$d_kile*curr_aar_kpifaktor) + 
                       d_tilDEA$d_avs + (d_tilDEA$d_akg*rente.ir) + (d_tilDEA$d_nettap*nettapspris.ir))
