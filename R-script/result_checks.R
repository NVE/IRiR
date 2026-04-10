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
library(rlang)

last_cb_yr = as.integer(y.cb) - 1L

#### BaseData checks ####

qry_basedata_comparison = "SELECT
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
			t.Beskrivelse = 'Varsel' -- Henter alltid varsel. Nytt varsel sammenlignes med nyeste varsel. Nyttvedtak sammenlignes med relevant vedtak.
		AND
			YEAR(CAST([Dato_Id] as date)) = ? -- Dato_Id er samme som costbase_year i R-kode
	ORDER BY InternSelskapsId, y"
# Henter data fra DWH####
if(decision == 0){

        df = dbGetQuery(con, 
                        qry_basedata_comparison, params=list(last_cb_yr))
}else{
        df = dbGetQuery(con, 
                        qry_basedata_comparison, params=list(y.cb))
}
# Omstrukturere format til samme form som eksisterende datasett####

basedata_reference <- df %>%
        # Ikke rør 'value' - den er allerede numerisk med . som desimal
        pivot_wider(
                id_cols    = c(orgn, id.y, id, y, comp),
                names_from = var,
                values_from = value,
                # Ved duplikater: ta første ikke-NA (robust mot flere rader pr nøkkel+var)
                values_fn  = ~ dplyr::first(na.omit(.x)),
                values_fill = NA_real_
        )

rm(df) # Sletter på DWH-format

ld_bd_order = names(Grunnlagsdata_ld)
Grunnlagsdata_ld_reference = basedata_reference %>%
        select(any_of(ld_bd_order))
rm(ld_bd_order)
rd_bd_order = names(Grunnlagsdata_rd)
Grunnlagsdata_rd_reference = basedata_reference %>%
        select(any_of(rd_bd_order))
rm(rd_bd_order)

library(rlang)

# Compilation basedata checks local distribution ####

keys <- c("orgn", "id.y", "id", "y", "comp")

gld <- Grunnlagsdata_ld %>%
        mutate(across(all_of(keys), as.character))

gld_ref <- Grunnlagsdata_ld_reference %>%
        mutate(across(all_of(keys), as.character)) %>%
        rename_with(~ paste0(.x, "_ref"), setdiff(names(.), keys))

# Dropp nøkler fra høyre før join, så slipper vi coalesce
gld_nok_only <- gld %>% select(id.y, dplyr::any_of(setdiff(names(gld), keys)))

joined_simple <- gld_ref %>%
        full_join(gld_nok_only, by = "id.y")

# Videre logikk identisk: finn common_measures, løkke for prc_ch_*, og rekkefølge
measures <- setdiff(names(gld), keys)
common_measures <- measures[
        paste0(measures, "_ref") %in% names(joined_simple) &
                measures %in% names(joined_simple)
]

result <- joined_simple
for (nm in common_measures) {
        ref_col <- paste0(nm, "_ref")
        prc_col <- paste0("prc_ch_", nm)
        
        ref_vals <- result[[ref_col]]
        cur_vals <- result[[nm]]
        
        if (is.character(ref_vals)) ref_vals <- suppressWarnings(as.numeric(ref_vals))
        if (is.character(cur_vals)) cur_vals <- suppressWarnings(as.numeric(cur_vals))
        
        result[[prc_col]] <- ifelse(!is.na(cur_vals) & !is.na(ref_vals) & ref_vals != 0,
                                    (cur_vals - ref_vals) / ref_vals * 100,
                                    NA_real_)
}

ordered_cols <- c(
        keys,
        as.vector(unlist(lapply(measures, function(col) {
                c(paste0(col, "_ref"), col, paste0("prc_ch_", col))
        })))
)

Grunnlagsdata_ld_pairwise <- result %>%
        select(any_of(ordered_cols))


Grunnlagsdata_ld_pairwise <- Grunnlagsdata_ld_pairwise %>%
        arrange(as.numeric(id.y))


Grunnlagsdata_ld_pairwise$id.y = as.numeric(Grunnlagsdata_ld_pairwise$id.y)

# Hjelpefunksjon: fyller sekvens i y innenfor en gruppe,
# slik at NA blir (forrige verdi + 1), også for flere påfølgende NA-er.
fill_y_seq_forward <- function(y_char) {
        y_num <- suppressWarnings(as.integer(y_char))
        n <- length(y_num)
        if (n == 0L) return(y_char)
        
        # Finn indekser med kjente (ikke-NA) verdier
        known_idx <- which(!is.na(y_num))
        if (length(known_idx) == 0L) {
                # Ingen kjente verdier i gruppa ??? returner som er
                return(y_char)
        }
        
        # Gå gjennom hvert kjent punkt og fyll frem til neste kjente (eller slutten)
        y_fill <- y_num
        for (k in seq_along(known_idx)) {
                s <- known_idx[k]
                e <- if (k < length(known_idx)) known_idx[k + 1] - 1L else n
                if (e > s) {
                        # Fyll sekvens: (forrige + 1, +2, ...)
                        y_fill[(s + 1L):e] <- y_fill[s] + seq_len(e - s)
                }
        }
        # Ledende NA før første kjente lar vi stå som NA (ingen "forrige" å bygge på)
        as.character(y_fill)
}

# KJØRING:
# Antar at Grunnlagsdata_ld_pairwise allerede eksisterer fra forrige steg.
Grunnlagsdata_ld_pairwise <- Grunnlagsdata_ld_pairwise %>%
        # Sortér KUN på id.y for å bevare original rekkefølge innen id.y
        arrange(id.y) %>%
        group_by(id.y) %>%
        # 1) Fyll nedover for orgn, id, comp innen hver id.y
        fill(orgn, id, comp, .direction = "down") %>%
        # 2) Fyll y som sekvens (forrige + 1) for NA
        group_modify(~ {
                .x$y <- fill_y_seq_forward(.x$y)
                .x
        }) %>%
        ungroup()



Grunnlagsdata_ld_pairwise <- Grunnlagsdata_ld_pairwise %>%
        fill(orgn, id, comp, .direction = "down") %>%
        arrange(as.numeric(id.y))

Grunnlagsdata_ld_pairwise$y = as.numeric(Grunnlagsdata_ld_pairwise$y)

Grunnlagsdata_ld_pairwise <- Grunnlagsdata_ld_pairwise %>%
        mutate(
                y = if_else(is.na(y), as.numeric(y.cb), y)
        )

# HVilke kriterier ønsker vi å kontrollere?
# Steg 1: Få inn denne delen i logikken
# Endring større enn snitt som er minst > 2% (Negativ og positiv endringer)
# Liste ut endringer i GEO - er også en sjekk på at vi har fått med alle fusjonene.
# Alle endring fra ett år til ett annet. Fange opp endring over en viss terskel
# Råresultater på alt som har endret mellom de to beregninen OG en for endringer mot siste år i nyeste datasett
# Endringer til sjekk hvor begge disse to samles
# Endring i sum kostnadsgrunnlag/Kapitalgrunnlag/DV/KILE/NETTAP for å sikre at noe ikke er helt spinnvilt
# Skal kanskje ikke kodes, men i en liste over hva som sjekkes må ap.t_2 inn i en sjekkliste.

# Steg 2: Utvide logikk med sjekk av resultater fra IR-beregninger
# Endring fra år y-1 til y er viktigst her


