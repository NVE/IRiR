remove(list=ls())

#### Aktivering av pakker ####
if (!"pxweb" %in% installed.packages()) install.packages("pxweb") # SSB 
library(pxweb)
if (!"XML" %in% installed.packages()) install.packages("XML") # SSB 
library(XML)
if (!"RCurl" %in% installed.packages()) install.packages("RCurl") # SSB 
library(RCurl)
if (!"openxlsx" %in% installed.packages()) install.packages("openxlsx") # SSB 
library(openxlsx)
if (!"zoo" %in% installed.packages()) install.packages("zoo") # SSB 
library(zoo)
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
library(lubridate)
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
library(tidyverse)
if (!"readxl" %in% installed.packages()) install.packages("readxl") 
library(readxl)
if (!"rstudioapi" %in% installed.packages()) install.packages("rstudioapi")
library(rstudioapi)
if (!"DBI" %in% installed.packages()) install.packages("DBI")
library(DBI)
if (!"odbc" %in% installed.packages()) install.packages("odbc")
library(odbc)
if (!"RODBC" %in% installed.packages()) install.packages("RODBC")
library(RODBC)
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")
library(reshape2)
if (!"tidyr" %in% installed.packages()) install.packages("tidyr")
library(tidyr)
if (!"rsdmx" %in% installed.packages()) install.packages("rsdmx")
library(rsdmx)
if (!"janitor" %in% installed.packages()) install.packages("janitor")
library(janitor)


# Setter sti til mappen hvor dette scriptet er lagret
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd() # Get current working directory

#### Manuelle justeringer #####

decision <-  0  # Select mode, 1 for decision or 0 for notice
y.cb <-  as.yearqtr("2024-01-01") # Settes til 1. januar for ?ret for rapportering
y.rc <- year(y.cb+2) # InntektsrammeC%r = cost base year + 2 C%r
wcp <-  1.01  # Working capital premium, used to calculate RAB from book values
rho <-  0.7   # Norm cost share in revenue cap 
y.avg <-  (y.cb-4):y.cb # Relevant years for calculation of average values

# Estimated cost for cost base year: total costs excluding depreciations from revenue decision y.cb
lrt_RC_dec.y.cb <- 0 #12201620 # Oppdatert 01.10.25 HBHO
# Koden vil uansett overskrive denne manuelle beregningen med den automatiske. 
ex.comp = c(900) # Selskaper som ekskluderes fra rekalibreringen til varsel 2026

# Velg dato for valutakurs, format "yyyy-mm-dd". Hvis denne er kommentert ut, velges siste tilgjengelige observasjon.
time_period_currency <- "2025-10-23" 

# Last ned data fra Nasdaq: http://www.nasdaqomx.com/transactions/markets/commodities/market-prices
# Filen maa lastes ned manuelt og legges i working directory som xlsx-fil. Scriptet vil fjerne unodvendig info fra excel-fila.
# Futures brukes kun i varsel - trenger ikke lastes inn til vedtak.
if (decision == 0){
  future <- read.xlsx("./market_prices_2025-10-23.xlsx") #Oppdatert 23.10.25 MOHH
}
future_updated <- "2025-10-23"

# Rente for forrige inntektsrammeaar (t-1)
swap_y.rc_1 <- 3.97          # Oppdatert 27.11.25 HBHO 
kreditpremie_y.rc_1 <- 0.87  # Oppdatert 25.11.25 HBHO
 
# Rente for inntektsrammeaaret (t)
swap_y.rc <- 3.94            # Oppdatert 27.11.25 HBHO
kreditpremie_y.rc <- 0.77   # Oppdatert 25.11.25 HBHO

#### 14091: Elektrisitetsbalanse (MWh) #### 
# pxweb_interactive("ttps://data.ssb.no/api/v0/no/table/14091")

# Hent data fra ny tabell (14091)
px_data <- pxweb_get(
  url = "https://data.ssb.no/api/v0/no/table/14091",
  query = list(
    "Produk2" = c("7", "7.21", "7.23"),
    "ContentsCode" = c("Kraft"),
    "Tid" = c("*")
  )
)

# Konverter til data.frame
elbalanse <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
names(elbalanse)[2] <- "maned"

# Lag `date` i yearmon-format (samme som i gammel kode)
elbalanse <- elbalanse %>%
  mutate(
    date = as.yearmon(maned, format = "%YM%m")
  )

# Filtrer p?? ??r (samme logikk som f??r)
forbruk_per_mnd <- elbalanse %>%
  filter(year(date) %in% c(year(y.cb), y.rc)) %>%
  pivot_wider(
    names_from = `produksjon og forbruk`,
    values_from = `Elektrisk kraft`
  )

# Rydd opp kolonnenavn hvis n??dvendig
colnames(forbruk_per_mnd)[4] <- "Forbruk i utvinning av raaolje og naturgass"
#colnames(forbruk_per_mnd)[which(names(forbruk_per_mnd) == "Forbruk i utvinning av raaolje og naturgass")] <- "Forbruk i utvinning av raaolje og naturgass"

# Beregn alminnelig forbruk
forbruk_per_mnd <- forbruk_per_mnd %>%
  mutate(
    Alminnelig_forbruk = `Nettoforbruk av elektrisk kraft` -
      `Forbruk i utvinning av raaolje og naturgass` -
      `Forbruk i kraftintensiv industri`,
    vekt = Alminnelig_forbruk / sum(Alminnelig_forbruk, na.rm = TRUE)
  ) %>%
  arrange(date)

#### Systempris fra NordPool ####
# Henter systempris per mnd fra NordPool (gjennom NVE_DWH)
server <- "SQL-BI03"
database = "NVE_DWH"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="SQL Server",
                      Server = server , Database = database)

kraftpriser <- dbGetQuery(con, 
  paste0("SELECT SpotPris, Dato_Id, LokalDatoTid, TidPaaDagen_Id, PrisOmraade_Id
  FROM [NVE_DWH].[Marked].[faktaKraftPris]
  WHERE Dato_Id >= '",year(y.cb),"-01-01'")) 

prisomr <- dbGetQuery(con, 
  "SELECT  PrisOmraade_Id, PrisOmraade_BK, Kommentar
  FROM [NVE_DWH].[Marked].[dimPrisOmraade]
  WHERE (lower(Kommentar) LIKE 'norwegian Area Elspot Area%') 
  OR (lower(Kommentar) LIKE 'nordic system price%')
  ")  

valutakurs <- dbGetQuery(con, 
  paste0("SELECT Dato_Id, FraValuta_Id, TilValuta_Id, Valutakurs_SysPower_NP
  FROM [NVE_DWH].[Marked].[faktaValutakurs]
  WHERE FraValuta_Id = 12 AND TilValuta_Id = 27
  AND Dato_Id >= '",year(y.cb),"-01-01'")) 

# Legger til info om prisomr og filtrerer data
kraftpriser <- kraftpriser %>% 
  right_join(prisomr, by = "PrisOmraade_Id") %>%
  mutate(Dato_Id = as_date(Dato_Id),
         date2 = as.yearmon(Dato_Id)) %>% 
  filter(year(Dato_Id) %in% c(year(y.cb), y.rc)) %>% 
  mutate(LokalDatoTid = as_datetime(LokalDatoTid, tz = "Europe/Berlin")) %>% 
  mutate(LokalDatoTid = strftime(LokalDatoTid, format="%Y-%m-%d %H:%M:%S"))
  
# Info om overgang til vintertid    
get_dst <- function(y, tz="Europe/Berlin", vinter=TRUE){
  start <- paste0(y, '-01-01')
  end <- paste0(y, '-12-31')
  d1 <- seq(as.POSIXct(start, tz = tz),
            as.POSIXct(end, tz =tz), by = 'hour')
  
  if (vinter==TRUE)
    {return(range(d1[lubridate::dst(d1)])[2])}
  else
    {return(range(d1[lubridate::dst(d1)])[1])} #hvis vinter = FALSE, returneres sommertid-tidspunktet
}
  
vintertid <- map(.x = c(year(y.cb), y.rc),
                 .f = get_dst) %>% 
  reduce(c) %>% 
  gsub(" CEST", "", .)

rep_vinter <- kraftpriser %>% filter(LokalDatoTid %in% vintertid)
  
kraftpriser <- bind_rows(kraftpriser, rep_vinter)

# Kobler valutakurs paa timer
valutakurs <- valutakurs %>% 
  mutate(Dato_Id = as_date(Dato_Id)) %>% 
  select(c(Dato_Id, Valutakurs_SysPower_NP))

kraftpriser <- kraftpriser %>% 
  left_join(valutakurs, by="Dato_Id") %>% 
  mutate(SpotPrisNOK = SpotPris * Valutakurs_SysPower_NP) # Beregner kraftpriser i NOK  
  
# Summerer kraftpriser per mnd
kraftpriser_mnd <- kraftpriser %>%
  group_by(date2, PrisOmraade_BK) %>%
  dplyr::summarise(SpotPrisNOK = mean(SpotPrisNOK)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = PrisOmraade_BK, values_from = SpotPrisNOK) %>% 
  dplyr::arrange(date2)

#### Beregner systempris ####
systempris_y.cb <- forbruk_per_mnd %>% 
  filter(year(date) == year(y.cb)) %>% 
  left_join(kraftpriser_mnd %>% select(date2, NP_SYS), by = c("date" = "date2")) %>% 
  dplyr::rename(systempris_uvektet = NP_SYS) %>% 
  mutate(vektet_pris = systempris_uvektet * vekt) %>% 
  mutate(systempris = sum(vektet_pris) + 11) 

systempris <- unique(systempris_y.cb$systempris)
sysp.t_2 = round(systempris/1000, digits = 5) # Price of network loss used in DEA   
sysp.t_2

rm(prisomr, rep_vinter)

#### Valutakurs fra Norges Bank ####
# Henter daglig eurokurs mot NOK 
# Kan endre antall observasjoner ved aa endre "lastNObservations=1" i koden nedenfor
if (decision == 0){
  url <- "https://data.norges-bank.no/api/data/EXR/B.EUR.NOK.SP?lastNObservations=1&format=sdmx-compact-2.1"
  valutakurs <- readSDMX(url) %>% 
    as.data.frame() %>% 
    mutate(OBS_VALUE = as.numeric(OBS_VALUE))
  rm(url)
} 
if (exists("time_period_currency") & decision == 0){
  url <- paste0("https://data.norges-bank.no/api/data/EXR/B.EUR.NOK.SP?format=sdmx-generic-2.1&startPeriod=",time_period_currency,"&endPeriod=",time_period_currency,"&locale=no")
  valutakurs <- readSDMX(url) %>% 
    as.data.frame(api) %>% 
    dplyr::rename(OBS_VALUE = obsValue,
                  TIME_PERIOD = obsTime) %>% 
    mutate(OBS_VALUE = as.numeric(OBS_VALUE))
  
  rm(url) }

#### EPAD og Future-kontrakter (til bruk i varsel) ####
if (decision == 0){
future <- as.data.frame(future)
future <- future[!is.na(future$Daily.Fix), ]
future$`Daily Fix` <- as.numeric(future$Daily.Fix)
future$updated <- future_updated
future$`Product series` <- future$Product.Series
future <- future[,c( "Product series", "Daily Fix", "updated")]

# Definerer ?rtall
future <- future %>%
  mutate(product = str_sub(`Product series`, start=1, end=4),
         kvartal = str_extract(`Product series`, "(?<=Q)[^-]+"),
         y = paste0("20", str_sub(`Product series`, start = -2), sep="")) %>% # Definerer aar
  filter(as.numeric(y) == as.numeric(y.cb)+2) #filterer vekk irrelevante aar 

#Filtrerer ut relevante norske prisomr?der
future$prisomr <- future$`Product series`
future$prisomr <- ifelse( grepl("OSL", (future$`Product series`)), "NO1", future$prisomr) 
future$prisomr <- ifelse( grepl("BER", (future$`Product series`)), "NO5", future$prisomr) 
future$prisomr <- ifelse( grepl("KRI", (future$`Product series`)), "NO2", future$prisomr)
future$prisomr <- ifelse( grepl("TRH", (future$`Product series`)), "NO3", future$prisomr)
future$prisomr <- ifelse( grepl("TRO", (future$`Product series`)), "NO4", future$prisomr)
future$prisomr <- ifelse( grepl("ENO", (future$`Product series`)), "SYS", future$prisomr)

#future <- future[ grepl( paste( "FUTBLYR-",substring(y.rc, 3,4), sep = "") , (future$`Product series`)) ,]

omr <- c("NO1", "NO2", "NO3", "NO4", "NO5", "SYS")
future <- future[ future$prisomr %in% omr, ]

future <- future %>% 
  filter(#y == year(y.rc), # Filtrerer pC% relevant C%r
         is.na(kvartal),  # Filtrere pC% kun C%rskontrakter (disse er NA)
         !product == "ENOA",
         !product == "ENOM",
         !product == "ENOY") %>%  # Disse kontraktene skal ikke brukes
  select(-c(product, kvartal))

colnames(future) <- c("serie", "daily_fix", "updated", "dato", "prisomr")

# Konverterer fra EUR til NOK
future$daily_fix_nok <- future$daily_fix * valutakurs$OBS_VALUE

future <- dcast(future,  updated + dato ~  prisomr, value.var = "daily_fix_nok")

epad <- future
epad[, c("NO1", "NO2", "NO3", "NO4", "NO5")] <- future[, c("NO1", "NO2", "NO3", "NO4", "NO5")] + future$SYS + 11

paste("Futurekontraktene er", future_updated)
timestamp_future <- paste("Futurekontraktene er", future_updated)
rm(future_updated)
paste("Valutakursen gjelder for", time_period_currency)
timestamp_currency <-   paste("Valutakursen gjelder for", time_period_currency)

writexl::write_xlsx(epad, "./epad_inkl_11_kr.xlsx")
}

#### Future-kontrakter fra Nasdaq (ikke i bruk, erstattes av EPAD) #####
#Ny versjon n??r man henter inn future
# if (decision == 0){
#   future_updated <- colnames(future)[1]  # Henter f??rste kolonnenavn som "oppdatert"-verdi
#   
#   future <- future %>%
#     mutate(`Daily.Fix` = as.numeric(`Daily.Fix`),
#            updated = future_updated) %>%
#     select(`Product.Series`, `Daily.Fix`, updated) %>%
#     mutate(product = str_sub(`Product.Series`, start = 1, end = 9),
#            kvartal = str_extract(`Product.Series`, "(?<=Q)[^-]+"),
#            y = paste0("20", str_sub(`Product.Series`, start = -2))) %>%
#     filter(product == "ENOFUTBLQ") %>%
#     filter(as.numeric(y) == as.numeric(y.cb) + 2)
# }


#### Beregne forbruksvekter per kvartal (ikke i bruk, un??dvendig ved bruk av EPAD) ####
# # Elbalanse frem til og med 2010 (06901: Elektrisitetsbalanse)
# pxweb_query_list <-
#   list("Produk2"=c("16"),
#        "ContentsCode"=c("Kraft"),
#        "Tid"=c("*"))
# 
# px_data <-
#   pxweb_get(url = "https://data.ssb.no/api/v0/no/table/06901",
#             query = pxweb_query_list)
# 
# elbalanse_til_2010 <- as.data.frame(px_data,
#                                     column.name.type = "text",
#                                     variable.value.type = "text") %>% 
#   dplyr::rename(maned = 2) %>% 
#   mutate(date = as_date(maned, format = "%YM%m")) %>% 
#   mutate(kvartal = floor_date(date, "quarter")) %>% 
#   mutate(kvartal2 = as.yearqtr(kvartal, format = "%Y-%m-%d"))
# 
# forbruk_per_kvartal_til_2010 <- elbalanse_til_2010 %>% 
#   group_by(kvartal2) %>% 
#   dplyr::summarise(forbruk = mean(`Elektrisk kraft`))
# 
# # Elbalanse fra og med 2010
# # Download data 
# px_data <- pxweb_get(url = "https://data.ssb.no/api/v0/no/table/12824",
#                      query = list("Produk2"=c("10"),
#                                   "ContentsCode"=c("Kraft"),
#                                   "Tid"=c("*")))
# 
# # Convert to data.frame 
# elbalanse_fra_2010 <- as.data.frame(px_data, 
#                                  column.name.type = "text",
#                                  variable.value.type = "text") %>% 
#   dplyr::rename(maned = 2) %>% 
#   mutate(date = as_date(maned, format = "%YM%m")) %>% 
#   mutate(kvartal = floor_date(date, "quarter")) %>% 
#   mutate(kvartal2 = as.yearqtr(kvartal, format = "%Y-%m-%d"))
# 
# forbruk_per_kvartal_fra_2010 <- elbalanse_fra_2010 %>% 
#   group_by(kvartal2) %>% 
#   dplyr::summarise(forbruk = mean(`Elektrisk kraft`))
# 
# # Samle elbalanse for hele perioden
# elbalanse1 <- forbruk_per_kvartal_til_2010 %>% 
#   filter(year(kvartal2) >= 2002 & year(kvartal2) <= 2009) #Velger tall fra 2002 til 2009
# 
# elbalanse2 <- forbruk_per_kvartal_fra_2010 %>% 
#   filter(
#     if (decision == 0){ # Hvis varsel, observasjoner til og med y.cb
#       year(kvartal2) <= year(y.cb)
#     } else {            # Hvis vedtak, observasjoner til og med y.rc
#       year(kvartal2) <= y.rc
#     }
#   )
# 
# elbalanse_samlet <- bind_rows(elbalanse1, elbalanse2)
# rm(elbalanse1, elbalanse2, forbruk_per_kvartal_fra_2010, forbruk_per_kvartal_til_2010, elbalanse_fra_2010, elbalanse_til_2010)

#### Referansepris kraft (ikke i bruk i varsel, erstattes av EPAD) ####
if (decision == 1){ # Beregne referansepris for varsel
  # # Beregne forbruksvekter per kvartal for perioden 2002 til y.cb (til bruk i varsel)
  # elbalanse_samlet <- elbalanse_samlet %>% 
  #   mutate(kvartal = quarter(kvartal2))
  # 
  # vektet_forbruk_per_kvartal <- elbalanse_samlet %>% 
  #   group_by(kvartal) %>% 
  #   dplyr::summarise(verdi = mean(forbruk)) %>% 
  #   ungroup() %>% 
  #   mutate(vekt = verdi/sum(verdi))
  # 
  # referansepris_kraft <- future %>% 
  #   select(c(`Daily.Fix`, kvartal)) %>% 
  #   mutate(valutakurs = valutakurs$OBS_VALUE) %>%  # Henter inn dagens valutakurs fra Norges Bank
  #   mutate(daily_fix_nok = `Daily.Fix`*valutakurs) %>% # Pris i NOK
  #   mutate(pris_med_paaslag = daily_fix_nok + 11) %>% # Pris i NOK med 11 i prispaaslag
  #   mutate(vekt = vektet_forbruk_per_kvartal$vekt) %>% 
  #   mutate(vektet_pris = pris_med_paaslag * vekt)
  # 
  # pnl.rc <- round(sum(referansepris_kraft$vektet_pris)/1000, digits = 5)
  # 
  # timestamp_future <- paste("Futurekontraktene er", future_updated)
  # timestamp_currency <- paste("Valutakursen gjelder for", valutakurs$TIME_PERIOD)
  # print(timestamp_future)
  # print(timestamp_currency)
  # rm(px_data, pxweb_query_list, future_updated, kraftpriser, elbalanse_samlet)
  # 
  # } else { # Beregne referansepris pC% kraft brukt i vedtak
  referansepris_kraft <- kraftpriser_mnd %>% 
    filter(year(date2)==y.rc) %>% 
    left_join(forbruk_per_mnd %>% select(c(date, vekt)), by = c('date2'='date')) %>%
    select(-NP_SYS) %>% 
    pivot_longer(cols = starts_with('NO'),
                 names_to = 'MBA',
                 values_to = 'pris') %>% 
    mutate(vektet_pris = (pris*vekt)) %>% 
    group_by(MBA) %>% 
    dplyr::summarise(vektet_omraadepris = sum(vektet_pris, na.rm = T)+11)
  
    nettap <- read_xlsx("Data/BaseData/nettap_prisomraader.xlsx", # denne bC8r byttes med en spC8rring mot DWH
                      col_names = c("Selskap", "id", "Aar", "MBA", "Type", "Nettnivaa", "Verdi"),
                      skip = 1)
  
    pnl.rc <- nettap %>% 
      filter(Aar == y.cb,
             Nettnivaa == "Alle") %>%
      group_by(id) %>% 
      mutate(vekt = Verdi/sum(Verdi)) %>% 
      ungroup() %>% 
      left_join(referansepris_kraft) %>% 
      mutate(pnl.rc = vektet_omraadepris * vekt)
  }

#### 12880: Makrookonomiske hovedstorrelser, prognoser KPI ####
#pxweb_interactive("https://data.ssb.no/api/v0/no/table/12880")
px_data <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/no/table/12880",
            query = list("ContentsCode"=c("Aarslonn", "KPI"),
                         "Tid"=c("*")))

ssb_12880_api <- as.data.frame(px_data, 
                               column.name.type = "text",
                               variable.value.type = "text") %>% 
  dplyr::rename(aar = 1,
         lonn = 2) %>% 
  mutate(aar = as.numeric(aar))

ssb_12880 <- ssb_12880_api

# ssb_12880_api <- ssb_12880_api %>% 
#   filter(
#     if (decision == 0){ 
#       aar > year(y.cb)          # Hvis varsel, observasjoner etter y.cb
#     } else {            
#       aar >= 2004 & aar <= y.rc # Hvis vedtak, sletter observasjoner fC8r 2004 og etter y.rc
#     }
#   ) 

kpi_arslonn_prognose <- ssb_12880_api %>% select(c(aar, lonn))
kpi_prognose <- ssb_12880_api %>%  select(c(aar, `Konsumprisindeksen (KPI)`))
rm(px_data, ssb_12880_api)

#### 11118: Konsumprisindeks for varer og tjenester, CPI-L ####
#pxweb_interactive("https://data.ssb.no/api/v0/no/table/11118")
px_data <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/no/table/11118",
            query = list("Leveringssektor"=c("B221"),
                         "ContentsCode"=c("LevIndAar","Aarsendring"),
                         "Tid"=c("*")))

ssb_11118 <- as.data.frame(px_data, 
                           column.name.type = "text", 
                           variable.value.type = "text") %>% 
  dplyr::rename(aar = 2) %>% 
  mutate(aar = as.numeric(aar))
print(" This warning message is OK:       Warning message: In pxweb_as_data_frame.pxweb_data(x, row.names = row.names, optional = optional,  : NAs introduced by coercion")

#### 03363: Konsumprisindeks for varer og tjenester, avsluttet tabell ####
#pxweb_interactive("https://data.ssb.no/api/v0/no/table/03363")
px_data <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/no/table/03363",
            query = list("Sektor"=c("L61"),
                         "ContentsCode"=c("LevIndAar","Aarsendring"),
                         "Tid"=c("*")))

ssb_03363 <- as.data.frame(px_data, 
                           column.name.type = "text",
                           variable.value.type = "text") %>% 
  dplyr::rename(aar = 2) %>% 
  mutate(aar = as.numeric(aar)) %>% 
  filter(aar > 2003)
print(" This warning message is OK:       Warning message: In pxweb_as_data_frame.pxweb_data(x, row.names = row.names, optional = optional,  : NAs introduced by coercion")

#### 03014: Konsumprisindeks, CPI ####
# Konsumprisindeks, etter konsumgruppe (2015=100) 1979 - 2020
# pxweb_interactive("https://data.ssb.no/api/v0/no/table/03014")
ssb_03014 <- 
  pxweb_get(url = "https://data.ssb.no/api/v0/no/table/03014",
            query = list("Konsumgrp"=c("TOTAL"),
                         "ContentsCode"=c("KpiAar","Aarsendring"),
                         "Tid"=c("*")))

ssb_03014 <- as.data.frame(ssb_03014, 
                           column.name.type = "text",
                           variable.value.type = "text") %>% 
  dplyr::rename(aar = 2,
                Aarsendring_prosent = 4) %>% 
  mutate(aar = as.numeric(aar))

#### CPI-L ####
kpi <- ssb_12880 # Henter prognoser for inflasjon
kpi <- kpi %>% 
  arrange(aar) %>%  # Sorting data
  filter(aar > 2014 & aar <= y.rc) %>% # Deleting years before 2004 and estimates for year beyond y.rc
  dplyr::rename(Aarslonn_indeks = lonn)

# Legger til faktisk inflasjon til y.cb ved varsel og til y.rc ved vedtak
if (decision==1){
  kpi[kpi$aar > 2014 & kpi$aar <= y.rc,]$Aarslonn_indeks <-
    ssb_11118[ssb_11118$aar <= y.rc,]$`Konsumprisindeks (2015=100)`
} else {
  kpi[kpi$aar > 2014 & kpi$aar <= year(y.cb),]$Aarslonn_indeks <-
    ssb_11118[ssb_11118$aar <= year(y.cb),]$`Konsumprisindeks (2015=100)`
}

# Regner om prognoser til indeks for varsel-modus
if (decision==0){
  for (i in (1:2)){
    kpi[kpi$aar >= y.cb,]$Aarslonn_indeks[i+1] <-
      kpi[kpi$aar >= y.cb,]$Aarslonn_indeks[i] * 
      (1 + (kpi[kpi$aar >= year(y.cb),]$Aarslonn[i+1]/100))
  }
}

cpi.l <- kpi %>% select(Aarslonn_indeks) %>% round(digits = 1) %>% as.vector() %>% unlist() %>% `names<-`(2015:(y.cb+2))

# CPI-L factor used in calibration and RC calculation
y.cb.cpi.l.factor = cpi.l[as.character(y.rc)]/cpi.l[as.character(year(y.cb))] 
y.cb.cpi.l.factor

#### CPI #### 
# Legger til faktisk inflasjon til y.cb ved varsel og til y.rc ved vedtak
kpi$konsumprisindeksen_indeks <- 100
if (decision==1){
  kpi[kpi$aar <= y.rc,]$konsumprisindeksen_indeks <- 
    ssb_03014[ssb_03014$aar > 2014 & ssb_03014$aar <= y.rc,]$`Konsumprisindeks (2015=100)`
} else {
  kpi[kpi$aar <= y.cb,]$konsumprisindeksen_indeks <- 
    ssb_03014[ssb_03014$aar > 2014 & ssb_03014$aar <= year(y.cb),]$`Konsumprisindeks (2015=100)`
}

# Regner om prognoser til indeks for varsel-modus
if (decision==0){
  for (i in (1:2)){
    kpi[kpi$aar >= year(y.cb),]$konsumprisindeksen_indeks[i+1] <-
      kpi[kpi$aar >= year(y.cb),]$konsumprisindeksen_indeks[i] * 
      (1 + (kpi[kpi$aar >= year(y.cb),]$`Konsumprisindeksen (KPI)`[i+1]/100))
  }
}

# Legger til kpi for inntektsrammeC%ret fra SSB tabell 03014 for vedtak-modus (overskriver verdi fra tabell 12880)
if(decision==1){
  ssb_12880[ssb_12880$aar==y.rc,]$`Konsumprisindeksen (KPI)` <- ssb_03014[ssb_03014$aar==y.rc,]$Aarsendring_prosent
  }

cpi <- kpi %>% select(konsumprisindeksen_indeks) %>% round(digits = 1) %>% as.vector() %>% unlist() %>% `names<-`(2015:(y.cb+2))

# CPI factor used in calibration and RC calculation
y.cb.cpi.factor = cpi[as.character(y.rc)]/cpi[as.character(year(y.cb))] 
y.cb.cpi.factor

rm(ssb_03014, ssb_03363, ssb_11118, kpi_arslonn_prognose, kpi_prognose)

#### Historiske referanserenter ####
server <- "SQL-BI03" 
database = "NVE_DWH" 
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver="SQL Server", 
                      Server = server , Database = database) 
dat <- dbGetQuery(con, "
  SELECT dimType.[Type_Id], 
  dimType.[type], 
  YEAR(CAST(faktaforutsetning.Dato_Id AS date)) AS 'y.cb', 
  CAST(dimKjoering.KjoereTidspunkt AS date) AS [time], 
  dimType.Beskrivelse, 
  dimKjoereIndeks.ErNyesteKjoering Rad,
  faktaforutsetning.verdi AS value, 
  dimVariabel.Variabelnavn AS Variable,
  dimVariabel.ForklaringNorsk AS Forklaring_norsk
  FROM Inntektsrammer.v_faktaForutsetninger AS faktaforutsetning
  LEFT OUTER JOIN Inntektsrammer.v_dimType AS dimType
  ON (faktaforutsetning.[Type_Id] = dimType.[Type_Id])
  LEFT OUTER JOIN Inntektsrammer.v_dimVariabel AS dimVariabel 
  ON (dimVariabel.[Variabel_Id] = faktaforutsetning.[Variabel_Id])
  LEFT OUTER JOIN Inntektsrammer.v_dimKjoering AS dimKjoering
  ON (dimKjoering.Kjoering_Id = faktaforutsetning.Kjoering_Id)
  LEFT OUTER JOIN Inntektsrammer.v_dimKjoereIndeks AS dimKjoereIndeks
  ON (dimKjoereIndeks.KjoereIndeks_Id = faktaforutsetning.KjoereIndeks_Id)
  WHERE dimType.ErNyesteType = 1
    ")

dat <- dat %>% 
  filter(Variable == "NVE.ir" | Variable == "NVE.ir.t") %>%       # Henter kun verdier for referanserente
  filter(type >= 3) %>%                                           # Henter kun verdier brukt i vedtak (eller etter klagebehandling)
  mutate(name_ir = y.cb+2) %>% 
  filter(name_ir >= 2009) %>%
  filter(Rad == "TRUE") %>%
  filter(Variable == "NVE.ir.t") %>%
  arrange(name_ir)

NVE.ir <- dat$value
names(NVE.ir) <- dat$name_ir

#### Referanserente ####
gjeldsandel	      <- 0.60
noytral_realrente <- 1.50
ek_beta           <- 0.875
markedspremie     <- 5.00
skatt             <- 0.22

#### Referanserente y.rc-1 ####
# Koden under kjores ikke dersom det allerede finnes en rente i DWH for y.rc-1
if (is.na(NVE.ir[NVE.ir = as.character(y.rc-1)])){
  # Snitt av aarene y.cb til y.cb + 3
  inflasjon_y.rc_1 <-	round(mean(ssb_12880[ssb_12880$aar >= year(y.cb) & ssb_12880$aar <= year(y.cb)+3,]$`Konsumprisindeksen (KPI)`),2)
  rente_y.rc_1 <- ((1-gjeldsandel)*(noytral_realrente+inflasjon_y.rc_1+(ek_beta*markedspremie))/
                     (1-skatt)+gjeldsandel*(swap_y.rc_1+kreditpremie_y.rc_1))/100
  rente_y.rc_1 <- round(rente_y.rc_1, digits = 4)
  names(rente_y.rc_1) <- y.rc-1
  
  # Legger til ny estimert rente for y.rc-1
  NVE.ir[NVE.ir = as.character(y.rc-1)] <- rente_y.rc_1
}

# Snitt av aarene y.cb+1 til y.cb + 4
inflasjon_y.rc <-	ssb_12880 %>% filter(aar >= year(y.cb)+1 & aar <= year(y.cb)+4) %>% dplyr::summarise(mean(`Konsumprisindeksen (KPI)`)) %>% round(digits = 2) %>% as.numeric()

rente_y.rc <- ((1-gjeldsandel)*(noytral_realrente+inflasjon_y.rc+(ek_beta*markedspremie)) / (1-skatt)+gjeldsandel*(swap_y.rc+kreditpremie_y.rc)) / 100
rente_y.rc <- round(rente_y.rc, digits = 4) %>% `names<-`(y.rc)

if (is.na(NVE.ir[NVE.ir = as.character(y.rc)])){
  # Legger til rente for y.cb hvis den ikke finnes i NVE.ir
  NVE.ir <- append(NVE.ir, rente_y.rc)
} else {
  # Ellers oppdateres renta
  NVE.ir[NVE.ir = as.character(y.rc)] <- rente_y.rc
}

NVE.ir.t_2 <-  NVE.ir[as.character(year(y.cb))] # Interest rate used in DEA (1_0)
NVE.ir.t <-  NVE.ir[as.character(y.rc)]   # Interest rate used in RC calculation (4_0)

#### Henter rammevilkaar fra tidligere varsel ####
#	Sporring mot grunnlagsdata i DWH hvor kun nyeste kjoring per aar blir inkludert, 
# og hvor kun historisk aar = kostnadsgrunnlag aar blir inkludert

dat <- dbGetQuery(con, "
  SELECT dimType.[Type_Id],  
  dimType.[type],  
  YEAR(CAST(faktaGrunnlagsdata.Dato_Id AS date)) AS 'y.cb',  
  faktaGrunnlagsdata. HistorikkAar AS y,
  CAST(dimKjoering.KjoereTidspunkt AS date) AS [time],  
  dimType.Beskrivelse,  
  dimKjoereIndeks.ErNyesteKjoering Rad, 
  faktaGrunnlagsdata.verdi AS value,  
  dimVariabel.Variabelnavn AS Variable, 
  dimVariabel.ForklaringNorsk AS Forklaring_norsk, 
  dimSelskap.Organisasjonsnummer AS 'orgn', 
  dimSelskap.Selskapsnavn, 
  dimSelskap.InternSelskapsId AS id 
  FROM Inntektsrammer.v_faktaGrunnlagsdata AS faktaGrunnlagsdata 
  LEFT OUTER JOIN Inntektsrammer.v_dimType AS dimType 
  ON (faktaGrunnlagsdata.[Type_Id] = dimType.[Type_Id]) 
  LEFT OUTER JOIN Inntektsrammer.v_dimVariabel AS dimVariabel  
  ON (dimVariabel.[Variabel_Id] = faktaGrunnlagsdata.[Variabel_Id]) 
  LEFT OUTER JOIN Inntektsrammer.v_dimKjoering AS dimKjoering 
  ON (dimKjoering.Kjoering_Id = faktaGrunnlagsdata.Kjoering_Id) 
  LEFT OUTER JOIN Inntektsrammer.v_dimKjoereIndeks AS dimKjoereIndeks 
  ON (dimKjoereIndeks.KjoereIndeks_Id = faktaGrunnlagsdata.KjoereIndeks_Id) 
  LEFT OUTER JOIN Inntektsrammer.v_dimSelskap AS dimSelskap 
  ON (dimSelskap.Selskap_Id = faktaGrunnlagsdata.Selskap_Id) 
  WHERE dimKjoereIndeks.ErNyesteKjoering = 1 
  AND dimType.ErNyesteType = 1 
  AND CAST(faktaGrunnlagsdata.Dato_Id AS date) = CAST(faktaGrunnlagsdata. HistorikkAar AS date)
")  
# fjern siste linje hvis "Kostnadsgrunnlag aar = aar" ikke skal gjelde

dat <-  dcast(dat, orgn + Selskapsnavn + id +
                   type + time + y.cb + y ~ Variable) %>% 
  mutate(comp = as.character(Selskapsnavn),
         id.y = paste(id, y.cb, sep = ""), # Create id.y variable  
         id.y = as.numeric(id.y),
         time = as_date(time),
         type = as.numeric(type))

# Sorterer ut data som ikke er fra tidligere varsel
if (decision==1){
  dat_geo <- dat[dat$y.cb == year(y.cb), ]
} else {
  dat_geo <- dat[dat$y.cb == year(y.cb)-1, ]
}
dat_geo <- dat_geo[dat_geo$type == max(dat_geo$type), ]

# Velger relevante variabler (rammevilkaar), pluss ap.t_2 ved vedtak 
if (decision==1){
  temp <- c('orgn', 'id', 'y.cb', 'ldz_mgc', 'ldz_salt', 'ldz_coast_wind', 'ldz_water',	'ldz_incline', 
            'ldz_snow_trees', 'ldz_forest_broadleaf', 'ldz_snowdrift',	'ldz_snow_400',	
            'ldz_wind_99',	'ldz_frosthours',	'ldz_forest_mixed_conf', 'ap.t_2')
} else {
  temp <- c('orgn', 'id', 'y.cb', 'ldz_mgc', 'ldz_salt', 'ldz_coast_wind', 'ldz_water',	'ldz_incline', 
            'ldz_snow_trees', 'ldz_forest_broadleaf', 'ldz_snowdrift',	'ldz_snow_400',	
            'ldz_wind_99',	'ldz_frosthours',	'ldz_forest_mixed_conf')
}

dat_geo <- dat_geo %>% select(all_of(temp))
rm(temp)

# Pga ulike opplastingstidspunkt til DWH blir det to rader per konsesjonC&r. 
# Dette pga trC8bbel med opplasting av 'ldz_mgc', fyller NA-radene med riktig verdi for konsesjonC&ren,
# og kaster ut radene med NA.
dat_geo <- dat_geo %>% 
  group_by(id) %>% 
  tidyr::fill(ldz_mgc, .direction = 'downup') %>% 
  ungroup() %>% 
  na.omit()

#### Kostader y.rc t-2 ####
dat_t_2 <- dat[dat$y.cb == year(y.cb)-2, ]
dat_t_2 <- dat_t_2[dat_t_2$type == max(dat_t_2$type), ]

cb.y_ex_cap <- dat_t_2 %>% 
  select(c("comp", "orgn", "y", "id.y", "id", "fp_ld_OPEX", "fp_ld_cens", "ld_nl", "fp_rd_OPEX", 
                          "fp_rd_cga", "fp_rd_cens", "rd_nl", "fp_t_OPEX", "fp_t_cens", "pnl.rc"))

if (exists('ex.comp')){
  cb.y_ex_cap <-  cb.y_ex_cap %>% filter(!id %in% ex.comp)
}

# CPI.L-justerte D&V-kostnader inkl. utredningskostnader 
cb.y_ex_cap$fp_OPEX = (cb.y_ex_cap$fp_ld_OPEX + cb.y_ex_cap$fp_rd_OPEX + cb.y_ex_cap$fp_rd_cga + 
                       cb.y_ex_cap$fp_t_OPEX)*(cpi.l[as.character(year(y.cb))]/cpi.l[as.character(year(y.cb)-2)])
sum(cb.y_ex_cap$fp_OPEX)

# Nettapskostnad (obs: ikke sikkert pnl.rc alltid skal deles paa 1000, det kommer an paa formatet)
cb.y_ex_cap$nl.cost = (cb.y_ex_cap$ld_nl + cb.y_ex_cap$rd_nl)*(cb.y_ex_cap$pnl.rc / 1000)
sum(cb.y_ex_cap$nl.cost)

# CPI-justert KILE
cb.y_ex_cap$fp_cens = (cb.y_ex_cap$fp_ld_cens + cb.y_ex_cap$fp_rd_cens +
                       cb.y_ex_cap$fp_t_cens)*cpi[as.character(year(y.cb))]/cpi[as.character(year(y.cb)-2)]
sum(cb.y_ex_cap$fp_cens)

# Sum av alt
Tot_cb.y_ex_cap = sum(cb.y_ex_cap$fp_OPEX + cb.y_ex_cap$nl.cost + cb.y_ex_cap$fp_cens)
Tot_cb.y_ex_cap
lrt_RC_dec.y.cb = Tot_cb.y_ex_cap

### Lagrer data ####
if (decision == 0){
  save(list = c("cpi", "cpi.l", "sysp.t_2", "NVE.ir", "NVE.ir.t",
                "NVE.ir.t_2", "y.cb.cpi.factor", "y.cb.cpi.l.factor",
                "y.cb", "y.rc", "decision", "y.avg", "wcp", "rho",
                "lrt_RC_dec.y.cb", "timestamp_currency",
                "timestamp_future"), # "pnl.rc" er tatt ut av lista for varsel, mohh 24.10.25
       file = "./Data/forutsetninger.Rdata")
  } else {
    save(list = c("cpi", "cpi.l", "pnl.rc", "sysp.t_2", "NVE.ir", "NVE.ir.t",
                  "NVE.ir.t_2", "y.cb.cpi.factor", "y.cb.cpi.l.factor",
                  "y.cb", "y.rc", "decision", "y.avg", "wcp", "rho",
                  "lrt_RC_dec.y.cb"),
         file = "./Data/forutsetninger.Rdata")
  }

save(list = c("dat_geo"), file = "./Data/geo_variables.Rdata") # Lagrer rammevilkor fra tidligere varsel/vedtak
save(list = c("cb.y_ex_cap"), file = "./Data/costs_t_2.Rdata") # Lagrer kostnader og ap.t_2 fra y.rc t-2

