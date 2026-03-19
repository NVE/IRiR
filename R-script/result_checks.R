server <- "SQL-BI03"
database = "NVE_DWH"
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="SQL Server",
                      Server = server , Database = database)

if (!"DBI" %in% installed.packages()) install.packages("DBI")
library(DBI)
if (!"odbc" %in% installed.packages()) install.packages("odbc")
library(odbc)
if (!"RODBC" %in% installed.packages()) install.packages("RODBC")
library(RODBC)


library(dplyr)
library(tidyr)
library(stringr)
library(readr)

#### BaseData checks ####

qry_basedata_lastnotice = "SELECT
	   s.Organisasjonsnummer as orgn
	  ,CAST(CONCAT(s.InternSelskapsId, YEAR(CAST(gd.HistorikkAar as date))) AS bigint) AS [id.y]
	  ,s.InternSelskapsId as id
	  ,YEAR(CAST([HistorikkAar] as date)) as 'y'
	  ,s.Selskapsnavn as comp
	  ,v.Variabelnavn as var
      ,TRY_CONVERT(decimal(38, 12), REPLACE([Verdi], ',', '.')) AS value -- for korrekt håndtering av komma
  FROM [NVE_DWH].[Inntektsrammer].[faktaGrunnlagsdata] gd

	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimType t on gd.Type_Id = t.Type_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimKjoereIndeks ki on gd.KjoereIndeks_Id = ki.KjoereIndeks_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimSelskap s on gd.Selskap_Id = s.Selskap_Id
	LEFT JOIN
		NVE_DWH.Inntektsrammer.dimVariabel v on gd.Variabel_Id = v.Variabel_Id

	WHERE
			t.Beskrivelse = 'Varsel' --- Må ha egen logikk på om denne skal hente varsel eller vedtak
		AND
			YEAR(CAST([Dato_Id] as date)) = 2023 -- må ersattes med y.cb-1
	ORDER BY InternSelskapsId, y"
        
        

df = dbGetQuery(con, 
  qry_basedata_lastnotice)





basedata_last_notice <- df %>%
        mutate(
                # Gjør value om til numerisk (håndterer desimalkomma og E-notasjon)
                value_num = readr::parse_number(
                        as.character(value),
                        locale = locale(decimal_mark = ",", grouping_mark = ".")
                )
        ) %>%
        select(-value) %>%
        rename(value = value_num) %>%
        pivot_wider(
                # ID-kolonner: behold disse som "nøkler" pr rad
                id_cols = c(orgn, id.y, id, y, comp),
                
                # Ny kolonne pr unik var-verdi
                names_from = var,
                
                # Verdiene som fylles inn
                values_from = value,
                
                # Hvis du har duplikater (samme id + var): velg en regel
                # Her: tar første ikke-NA. Alternativt mean/sum etc.
                values_fn = list(value = ~ dplyr::first(na.omit(.x))),
                
                # Hva skal stå hvis kombinasjonen mangler?
                values_fill = NA_real_
        )


