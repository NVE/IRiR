# ---- 1. Setup ------------------------------------------------
library(jsonlite)


# ---- 2. Hent KPI (historisk) ---------------------------------

url_hist <- "https://data.ssb.no/api/pxwebapi/v2/tables/14701/data?lang=no&outputFormat=json-stat2&valuecodes[Tid]=*&valuecodes[VareTjenesteGrp]=00&codelist[VareTjenesteGrp]=vs_CoiCop2018Kpi01&valuecodes[ContentsCode]=KpiAar&heading=Tid,ContentsCode&stub=VareTjenesteGrp"


json_hist <- fromJSON(url_hist)

years  <- names(json_hist$dimension$Tid$category$label)
values <- json_hist$value

kpi_hist <- data.frame(
  year = as.numeric(years),
  kpi  = values
)

kpi_hist <- kpi_hist[order(kpi_hist$year), ]
last_kpi <- tail(kpi_hist$kpi, 1)

# ---- 3. Hent KPI (prognose) ----------------------------------

url_fore <- "https://data.ssb.no/api/pxwebapi/v2/tables/12880/data?lang=no&outputFormat=json-stat2&valuecodes[ContentsCode]=KPI&valuecodes[Tid]=*&heading=Tid&stub=ContentsCode"

json_fore <- fromJSON(url_fore)

years  <- names(json_fore$dimension$Tid$category$label)
values <- json_fore$value

kpi_fore <- data.frame(
  year = as.numeric(years),
  kpi  = values
)

kpi_fore <- kpi_fore[order(kpi_fore$year), ]
kpi_fore$growth <- kpi_fore$kpi
kpi_fore <- kpi_fore |> dplyr::filter(year>max(kpi_hist$year))

for (i in 1:nrow(kpi_fore)) {
  
  new_kpi <- last_kpi * (1 + kpi_fore$growth[i] / 100)
  
  kpi_fore$kpi[i] <- new_kpi
  
  last_kpi <- new_kpi
  
}


# ---- 4. Kombiner KPI ------------------------------------------

kpi_data <- rbind(kpi_hist, kpi_fore['year','kpi'])

kpi_data <- kpi_data[order(kpi_data$year), ]
kpi_data <- kpi_data[!duplicated(kpi_data$year), ]