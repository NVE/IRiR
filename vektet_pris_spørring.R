# Formålet med dette scriptet er å beregne nettapspriser tilbake i tid


#remove(list=ls())
Sys.setlocale("LC_ALL", "nb_NO.UTF-8")
options(scipen = 999)

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

#
years <- c(2018:2026)



# Henter vektet vektet nettapspris og nettap brukt i tidligere vedtak (pnl.rc)

server <- "SQL-BI03"
database = "NVE_DWH"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="SQL Server",
                      Server =  server , Database = database)

nettap <- dbGetQuery(con, 
                     paste0("SELECT *
                                	FROM [NVE_DWH].[Inntektsrammer].[faktaResultater]
                                	WHERE ([Variabel_Id] = 192 OR [Variabel_Id] = 135)"))

variabelnavn <- dbGetQuery(con, 
                           paste0("SELECT *
                                	FROM [NVE_DWH].[Inntektsrammer].[dimVariabel]"))

selskap <- dbGetQuery(con, 
                      paste0("SELECT *
                                  FROM [NVE_DWH].[Inntektsrammer].[dimSelskap]"))

kjøring <- dbGetQuery(con, 
                      paste0("SELECT *
                                  FROM [NVE_DWH].[Inntektsrammer].[dimKjoereIndeks]"))

type <- dbGetQuery(con, 
                   paste0("SELECT *
                                  FROM [NVE_DWH].[Inntektsrammer].[dimType]"))

selskap <- selskap %>%
  select(Selskap_Id, InternSelskapsId, Selskapsnavn, KostnadsgrunnlagTilAar, KostnadsgrunnlagFraAar)


nettap <- nettap |>
  left_join(variabelnavn, by="Variabel_Id") |>
  left_join(kjøring, by="KjoereIndeks_Id") |>
  left_join(type, by="Type_Id") |>
  filter(ErNyesteKjoering==1 & ErNyesteType==1) |>
  select(Dato_Id, Type_Id, Variabel_Id, Selskap_Id, Verdi, Variabelnavn, ForklaringNorsk) |>
  mutate(y.cb = year(Dato_Id),
         y.rc = y.cb+2) |> 
  filter(Type_Id >= 3) 


nettap_wide <- nettap %>%
  pivot_wider(
    id_cols = c(Selskap_Id, y.cb, y.rc, Type_Id),
    names_from = Variabelnavn,
    values_from = Verdi
  ) %>%
  left_join(selskap, by="Selskap_Id") %>% 
  drop_na(ld_nl)

# Selskaper skal over på dagens selskapsstruktur 
fusjoner <- read_xlsx('./Data/fusjon_id_paaluttrekk.xlsx') %>%
  bind_rows(tibble(id = 505,
                   id_2023_struktur = 675)) %>% # id 505 (Lillehammer og Gausdal Energinett) til Elvia (675) 
  bind_rows(tibble(id = 353,
                   id_2023_struktur = 103)) %>%  # id 353 (Finnmark Energiverk AS) til LuostejokKraftlag SA (103)
  bind_rows(tibble(id = 332,
                   id_2023_struktur = 685)) # id 332 (Statkraft SF) til Statkraft Energi AS (685)

fusjoner_manuel <- read_xlsx('./Data/fusjoner_manuel_håndtering.xlsx', range="C1:D98")

nettap_wide <- nettap_wide %>%
  left_join(fusjoner, by=c("InternSelskapsId"="id")) %>%
  left_join(fusjoner_manuel, by="InternSelskapsId") %>%
  mutate(id_2023_struktur = coalesce(id_2023_struktur.x,id_2023_struktur.y))%>%
  select(-id_2023_struktur.x,-id_2023_struktur.y)# Ser ut til at alle som ikke matcher allerede har 2023 selskap id

#missing_ids <- nettap_wide %>%
#  anti_join(fusjoner, by=c("InternSelskapsId"="id"))|>
#  distinct(InternSelskapsId, .keep_all = TRUE) |>
#  select(Selskap_Id, Selskapsnavn, InternSelskapsId)

#write.xlsx(missing_ids, file = "./Data/fusjoner_manuel_håndtering.xlsx", overwrite = FALSE)

# Beregner vekte
df_nettap_2014 <- nettap_wide %>%
  group_by(id_2023_struktur, y.cb) %>%
  mutate(
    ld_nl_tot = sum(ld_nl, na.rm = TRUE),
    vekt = ld_nl/ld_nl_tot,0,
    pnl.rc_vektet = sum(vekt*pnl.rc*1000, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  group_by(id_2023_struktur, y.cb, y.rc) %>%
  summarise(ld_nl = sum(ld_nl, na.rm = TRUE),
            pnl.rc = sum(pnl.rc, na.rm=TRUE))%>%
  ungroup() %>% 
  filter(y.cb<2015)

# Gjør det samme for senere år 

server <- "SQL-BI03"
database = "NVE_DWH"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="SQL Server",
                      Server = server , Database = database)

grunnlagsdata <- dbGetQuery(con, 
                            paste0("SELECT *
                                	FROM [NVE_DWH].[Inntektsrammer].[faktaGrunnlagsdata]"))

grunnlagsdata <- grunnlagsdata |>
  left_join(variabelnavn, by="Variabel_Id") |> 
  left_join(kjøring, by="KjoereIndeks_Id") |>
  left_join(type, by="Type_Id") |>
  filter(ErNyesteKjoering==1 & ErNyesteType==1) |>
  select(Dato_Id, Type_Id, Variabel_Id, Selskap_Id, Verdi, Variabelnavn, ForklaringNorsk, HistorikkAar) |>
  mutate(y.cb = year(Dato_Id),
         y.rc = y.cb+2) 


nettap <- grunnlagsdata |> 
  filter(Variabel_Id %in% c(135,192)) |>
  filter(Dato_Id == HistorikkAar) |>
  distinct()


nettap_wide <- nettap %>%
  pivot_wider(
    id_cols = c(Selskap_Id, y.cb, y.rc, Type_Id),
    names_from = Variabelnavn,
    values_from = Verdi
  ) %>%
  left_join(selskap, by="Selskap_Id") %>% 
  drop_na(ld_nl)

fusjoner_manuel <- read_xlsx('./Data/fusjoner_manuel_håndtering_post_2014.xlsx', range="C1:D98")

nettap_wide <- nettap_wide %>%
  left_join(fusjoner, by=c("InternSelskapsId"="id")) %>%
  left_join(fusjoner_manuel, by="InternSelskapsId") %>%
  mutate(id_2023_struktur = coalesce(id_2023_struktur.x,id_2023_struktur.y))%>%
  select(-id_2023_struktur.x,-id_2023_struktur.y)%>%
  mutate(id_2023_struktur = if_else(is.na(id_2023_struktur), InternSelskapsId,id_2023_struktur)) # Ser ut til at alle som ikke matcher allerede har 2023 selskap id

#missing_ids <- nettap_wide %>%
#  anti_join(fusjoner, by=c("InternSelskapsId"="id"))|>
#  distinct(InternSelskapsId, .keep_all = TRUE) |>
#  select(Selskap_Id, Selskapsnavn, InternSelskapsId)
#write.xlsx(missing_ids, file = "./Data/fusjoner_manuel_håndtering_post_2014.xlsx", overwrite = FALSE)

# Beregner vekte
df_nettap_2024 <- nettap_wide %>%
  group_by(id_2023_struktur, y.cb) %>%
  mutate(
    ld_nl_tot = sum(ld_nl, na.rm = TRUE),
    vekt = ld_nl/ld_nl_tot,0,
    pnl.rc_vektet = sum(vekt*pnl.rc, na.rm=TRUE)
  ) %>%
  ungroup() |>
  group_by(id_2023_struktur, y.cb, y.rc) %>%
  summarise(ld_nl = sum(ld_nl, na.rm = TRUE),
            pnl.rc = sum(pnl.rc, na.rm=TRUE))%>%
  ungroup() %>% 
  filter(y.cb>=2015)



# Binder sammen data fra 2005-2014 og 2015-2024 og gemmer fil
df_nettap <- rbind(df_nettap_2014,df_nettap_2024) |>
  left_join(read_xlsx("./Data/BaseData/id_ir_26.xlsx"), by=c("id_2023_struktur"="id")) |>
  select(-ld_nl) |>
  rename(id=id_2023_struktur)



write.xlsx(df_nettap, file = "./Data/df_nettap.xlsx", overwrite = TRUE)

# Henter systempris for år t-2
server <- "SQL-BI03"
database = "NVE_DWH"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="SQL Server",
                      Server = server , Database = database)

systempris <- dbGetQuery(con, 
                            paste0("SELECT *
                                	FROM [NVE_DWH].[Inntektsrammer].[faktaForutsetninger]
                                  WHERE ([Variabel_Id] = 317)"))


systempris <- systempris |> # OBS systempris finnes kun til 2011
  left_join(kjøring, by="KjoereIndeks_Id") |>
  left_join(type, by="Type_Id") |>
  filter(ErNyesteKjoering==1 & ErNyesteType==1) |>
  mutate(y.cb = year(Dato_Id),
         y.rc = y.cb+2,
         sysp.t_2 = ifelse(y.cb>2016, Verdi, Verdi/1000)) |>
  select(y.cb,y.rc,sysp.t_2) |>
  arrange(y.cb)


write.xlsx(systempris, file = "./Data/systempris_sysp.t_2.xlsx", overwrite = TRUE)



