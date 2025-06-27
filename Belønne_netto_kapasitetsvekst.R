
source("./R-script/yearly_increase_function.R")

Fritidsbolig <- 7.2
Husholdning <- 15.2
naering_lav <- 72.8
naering_hoy <- 123.7
Bergverk <- 413.3
Industri <- 901.9

Effektavfartstillegg <- 100000
TotaltFartstilleggIR <- Effektavfartstillegg/rho

kundevekst_data <- "Datagrunnlag kundeboost_varsel 25_19112024.xlsx"

abo <- read.xlsx(paste0("./Data/BaseData/",kundevekst_data))
abo <- replace(abo, is.na(abo), 0)

abo <- abo %>% 
  dplyr::rename(orgn = id,
                y = aar,
                comp = selskap)   

id <- openxlsx::read.xlsx("./Data/BaseData/id_ir_25.xlsx") %>% 
  mutate(orgn = as.integer(orgn)) %>% 
  select(-name)

abo <- abo %>% 
  mutate(orgn = as.integer(orgn)) %>% 
  left_join(id, by = 'orgn') %>% 
  mutate(comp = as.character(comp),
         id.y = paste(id, y, sep=""),
         id.y = as.numeric(id.y),
         orgn.y = paste(y, orgn, sep=""),
         orgn.y = as.numeric(orgn.y),
         y = as.integer(y)) %>% 
  select(-idaar) %>%
  arrange(id, y)

rm(id)

abo <- abo %>% 
  mutate(naering_lav_abo = Jordbruk_abo + Bygg_abo + DivTjeneste_abo,
         naering_hoy_abo = Drivhus_abo + Forsyning_abo + Transport_abo + Varehandel_abo)


kundegrupper <- setdiff(names(abo), c("id", "y", "orgn", "id.y", "comp", "orgn.y"))
naeringsgrupper <- c("naering_lav_abo", "naering_hoy_abo")
erapp_kundegrupper <- setdiff(kundegrupper, naeringsgrupper)


vekt_grupper <- c("Fritidsboliger_abo", "Husholdninger_abo" , "naering_lav_abo", "naering_hoy_abo", "Bergverksdrift_abo", "Industri_abo")
vekt_grupper_prc_ch = paste(vekt_grupper, "prc_ch", sep = "_")
vekt_grupper_inc = paste(vekt_grupper, "inc", sep = "_")

abo$abo_total <- rowSums(abo[, erapp_kundegrupper])
inc_var <- c("abo_total", kundegrupper)

abo <- replace(abo, is.na(abo), 0)

abo <- calculate_yearly_increase_w_prc2(abo, "id", "y", inc_var)


# Define the threshold
threshold_prc_ch <- 100
threshold_inc <- 1

# Filter rows based on conditions and create a new dataframe data_control
data_control <- abo %>%
        filter(y == y.cb) %>%
        filter(
                if_any(all_of(vekt_grupper), function(x) abs(x) > 1) &
                if_any(all_of(vekt_grupper_prc_ch), function(x) abs(x) > threshold_prc_ch) &
                if_any(all_of(vekt_grupper_inc), function(x) abs(x) > threshold_inc)
)

# View the filtered dataframe
print(data_control)
fdt <- format(Sys.time(), "%Y%m%d_%H%M%S")

KISSfilename <- paste0(fdt,"_Boost_data_control_",y.cb,".xlsx")
write.xlsx(data_control,KISSfilename)

#Setter alle NA-verdier til 0
abo <- replace(abo, is.na(abo), 0)

abo <- abo %>% 
  mutate(vektet_Fritidsbolig = Fritidsboliger_abo_inc*Fritidsbolig,
         vektet_Husholdning = Husholdninger_abo_inc*Husholdning,
         vektet_naering_lav = naering_lav_abo_inc*naering_lav,
         vektet_naering_hoy = naering_hoy_abo_inc*naering_hoy,
         vektet_Bergverk = Bergverksdrift_abo_inc*Bergverk,
         vektet_Industri = Industri_abo_inc*Industri) %>% 
  mutate(vektetKundevekst = rowSums(select(., starts_with("vektet_")), na.rm = TRUE)) %>% 
  mutate(vektetKundevekst = if_else(vektetKundevekst<=0, 0, vektetKundevekst)) %>% 
  mutate(vektetKundevekst_til_kontroll = vektetKundevekst)


Total_vektetKundevekst_y.cb <- abo %>% filter(y==y.cb) %>% select(vektetKundevekst) %>% sum()
Kundevekst_pris = TotaltFartstilleggIR/Total_vektetKundevekst_y.cb

abo <- abo %>% 
  mutate(FartstilleggIR = vektetKundevekst*Kundevekst_pris)

abo$KostprAbo = (abo$FartstilleggIR / abo$abo_total)*1000

tot_mill = TotaltFartstilleggIR/1000
print(tot_mill)

# Skriver ut resultatet til Excel
abo %>% 
  filter(y == y.cb) %>% 
  select(c(comp, id, FartstilleggIR)) %>% 
  write_xlsx("./Results/Kundeboost_resultat.xlsx")

# Skriver ut rådata til Excel
abo %>% 
  select(1:14) %>% 
  write_xlsx("./Data/Datagrunnlag for kundevekst.xlsx")
