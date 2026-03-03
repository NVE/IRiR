remove(list=ls()) # Remove all objects from memory

# Install/load packages
if (!"Benchmarking" %in% installed.packages()) install.packages("Benchmarking")
if (!"plyr" %in% installed.packages()) install.packages("plyr")
if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
if (!"openxlsx" %in% installed.packages()) install.packages("openxlsx")
if (!"rstudioapi" %in% installed.packages()) install.packages("rstudioapi")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"writexl" %in% installed.packages()) install.packages("writexl")
if (!"readxl" %in% installed.packages()) install.packages("readxl")
library(tidyverse)
library(Benchmarking)
library(dplyr)
library(openxlsx)
library(rstudioapi)
library(writexl)
library(readxl)
source("./R-script/yearly_increase_function.R")

Fritidsbolig <- 7.2
Husholdning <- 15.2
naering_lav <- 72.8
naering_hoy <- 123.7
Bergverk <- 413.3
Industri <- 901.9

#Definerer variabler
rho <- 0.7
y.cb <- 2024

Effektavfartstillegg <- 100000
TotaltFartstilleggIR <- Effektavfartstillegg/rho

kundevekst_data <- "IR2026 - Datagrunnlag for kundevekst ??? Oppdatert.xlsx"

abo <- read.xlsx(paste0("./Data/BaseData/",kundevekst_data))
abo <- replace(abo, is.na(abo), 0)

abo <- abo %>% 
  dplyr::rename(orgn = id,
                y = aar,
                comp = selskap)   

id <- openxlsx::read.xlsx("./Data/BaseData/id_ir_26.xlsx") %>% 
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

###Original### 
#abo <- abo %>% 
  #mutate(naering_lav_abo = Jordbruk_abo + Bygg_abo + DivTjeneste_abo,
         #naering_hoy_abo = Drivhus_abo + Forsyning_abo + Transport_abo + Varehandel_abo)

#Endret til "d_xxx_abo" for ?? f?? det til ?? kj??re 10.11.25
abo <- abo %>% 
  mutate(naering_lav_abo = d_Jordbruk_abo + d_Bygg_abo + d_DivTjeneste_abo,
         naering_hoy_abo = d_Drivhus_abo + d_Forsyning_abo + d_Transport_abo + d_Varehandel_abo)


kundegrupper <- setdiff(names(abo), c("id", "y", "orgn", "id.y", "comp", "orgn.y"))
naeringsgrupper <- c("naering_lav_abo", "naering_hoy_abo")
erapp_kundegrupper <- setdiff(kundegrupper, naeringsgrupper)

#Original
#vekt_grupper <- c("Fritidsboliger_abo", "Husholdninger_abo" , "naering_lav_abo", "naering_hoy_abo", "Bergverksdrift_abo", "Industri_abo")
#Legger til d_xxx_abo 10.11.25
vekt_grupper <- c("d_Fritidsboliger_abo", "d_Husholdninger_abo" , "naering_lav_abo", "naering_hoy_abo", "d_Bergverksdrift_abo", "d_Industri_abo")
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
  mutate(vektet_Fritidsbolig = d_Fritidsboliger_abo_inc*Fritidsbolig,
         vektet_Husholdning = d_Husholdninger_abo_inc*Husholdning,
         vektet_naering_lav = naering_lav_abo_inc*naering_lav,
         vektet_naering_hoy = naering_hoy_abo_inc*naering_hoy,
         vektet_Bergverk = d_Bergverksdrift_abo_inc*Bergverk,
         vektet_Industri = d_Industri_abo_inc*Industri) %>% 
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
  write_xlsx("./Results/Kundeboost_resultat_oppdatert.xlsx")

# Skriver ut rC%data til Excel
abo %>% 
  select(1:14) %>% 
  write_xlsx("./Data/Datagrunnlag for kundevekst_oppdatert.xlsx")
