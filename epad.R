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
if (!"readxl" %in% installed.packages()) install.packages("readxl") #Til ? lese xls
library(readxl)
if (!"rstudioapi" %in% installed.packages()) install.packages("rstudioapi")
library(dplyr)
if (!"DBI" %in% installed.packages()) install.packages("DBI")
library(DBI)
if (!"odbc" %in% installed.packages()) install.packages("odbc")
library(odbc)
if (!"RODBC" %in% installed.packages()) install.packages("RODBC")
library(RODBC)
if (!"reshape2" %in% installed.packages()) install.packages("reshape2")

library(reshape2)
library(rstudioapi)
library(Benchmarking)
library(plyr) 
library(dplyr)
library(openxlsx)

# Denne koden setter stien til til mappen hvor dette scriptet er lagret
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd() # Get current working directory

###############################.
#### Manuelle justeringer #####
###############################.

# Denne m? endres til 1. januar for ?ret for rapportering
y.cb = as.yearqtr("2024-01-01")
y.rc <- y.cb + 2

# Velg dato for valutakurs p? formatet "yyyy-mm-dd" og s?rg for at linjen med 
# kode ikke er kommentert ut. Hvis ikke velges siste tilgjengelig observasjon
# automatisk. 
time_period_currency <- "2025-10-23"
valutakurs <- 11.8529

#.....Last ned data fra Nasdaq
# http://www.nasdaqomx.com/commodities/market-prices
# Last ned type: Month  product: futures. Husk: inkluder de som ikke er "traded".
# Filen m? lastes ned manuelt og legges i working directory som xlsx-fil
# Deretter m? filnavnet som leses inn, oppdateres. 
# Scriptet fjerner all un?dvendig infor fra excel-fila

future <- read.xlsx("./market_prices_2025-10-23.xlsx")
future_updated <- "2025-10-23"

#######################################.
##### Future-kontrakter fra Nasdaq #####
#######################################.

# Filen m? lastes ned manuelt og legges i working directory.
# Scriptet fjerner all un?dvendig infor fra excel-fila
# Deretter m? filnavnet som leses inn, oppdateres. 
# http://www.nasdaqomx.com/transactions/markets/commodities/market-prices
# 

# future_updated <- c((colnames(future[1])))
# colnames(future) <- future[1,] # Gir riktige navn til kolonner
# future <- future[-1,] # Sletter un?dvendig rad
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
  filter(y == year(y.rc), # Filtrerer pC% relevant C%r
         is.na(kvartal),  # Filtrere pC% kun C%rskontrakter (disse er NA)
         !product == "ENOA",
         !product == "ENOM",
         !product == "ENOY") %>%  # Disse kontraktene skal ikke brukes
  select(-c(product, kvartal))

colnames(future) <- c("serie", "daily_fix", "updated", "dato", "prisomr")

# Konverterer fra EUR til NOK
future$daily_fix_nok <- future$daily_fix * valutakurs

future <- dcast(future,  updated + dato ~  prisomr, value.var = "daily_fix_nok")

epad <- future
epad[, c("NO1", "NO2", "NO3", "NO4", "NO5")] <- future[, c("NO1", "NO2", "NO3", "NO4", "NO5")] + future$SYS + 11

paste("Futurekontraktene er", future_updated)
timestamp_future <- paste("Futurekontraktene er", future_updated)
rm(future_updated)
paste("Valutakursen gjelder for", time_period_currency)
timestamp_currency <-   paste("Valutakursen gjelder for", time_period_currency)

writexl::write_xlsx(epad, "./epad_inkl_11_kr.xlsx")