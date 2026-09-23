# Pakker ------------------------------------------------------------------

required_packages <- c(
        "DBI",
        "odbc",
        "dplyr",
        "tidyr",
        "stringr",
        "purrr"
)

packages_to_install <- required_packages[
        !vapply(
                required_packages,
                requireNamespace,
                logical(1),
                quietly = TRUE
        )
]

if (length(packages_to_install) > 0) {
        install.packages(packages_to_install)
}

library(DBI)
library(odbc)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)


# Tilkobling til DWH -------------------------------------------------------

server <- "SQL-BI03"
database <- "NVE_DWH"

con <- DBI::dbConnect(
        odbc::odbc(),
        Driver = "SQL Server",
        Server = server,
        Database = database
)


# Året som skal brukes ved henting av referansedata -----------------------

last_cb_yr <- as.integer(y.cb) - 1L


# SQL-spørring -------------------------------------------------------------

qry_basedata_comparison <- "
SELECT
       s.Organisasjonsnummer AS orgn,
       CAST(
           CONCAT(
               s.InternSelskapsId,
               YEAR(CAST(gd.HistorikkAar AS date))
           ) AS bigint
       ) AS [id.y],
       s.InternSelskapsId AS id,
       YEAR(CAST(gd.HistorikkAar AS date)) AS y,
       s.Selskapsnavn AS comp,
       v.Variabelnavn AS var,
       TRY_CONVERT(
           decimal(38, 12),
           REPLACE(gd.Verdi, ',', '.')
       ) AS value
FROM NVE_DWH.Inntektsrammer.faktaGrunnlagsdata gd

LEFT JOIN NVE_DWH.Inntektsrammer.dimType t
       ON gd.Type_Id = t.Type_Id

LEFT JOIN NVE_DWH.Inntektsrammer.dimKjoereIndeks ki
       ON gd.KjoereIndeks_Id = ki.KjoereIndeks_Id

LEFT JOIN NVE_DWH.Inntektsrammer.dimSelskap s
       ON gd.Selskap_Id = s.Selskap_Id

LEFT JOIN NVE_DWH.Inntektsrammer.dimVariabel v
       ON gd.Variabel_Id = v.Variabel_Id

WHERE t.Beskrivelse = 'Varsel'
  AND YEAR(CAST(gd.Dato_Id AS date)) = ?

ORDER BY
       s.InternSelskapsId,
       y
"


# Hent referansedata fra DWH ----------------------------------------------

if (decision == 0) {
        
        df <- DBI::dbGetQuery(
                con,
                qry_basedata_comparison,
                params = list(last_cb_yr)
        )
        
} else {
        
        df <- DBI::dbGetQuery(
                con,
                qry_basedata_comparison,
                params = list(as.integer(y.cb))
        )
}


# Omstrukturer referansedata til bredt format -----------------------------

basedata_reference <- df %>%
        pivot_wider(
                id_cols = c(
                        orgn,
                        id.y,
                        id,
                        y,
                        comp
                ),
                names_from = var,
                values_from = value,
                values_fn = function(x) {
                        
                        x <- x[!is.na(x)]
                        
                        if (length(x) == 0L) {
                                NA_real_
                        } else {
                                dplyr::first(x)
                        }
                },
                values_fill = NA_real_
        )

rm(df)


# Opprett referansedatasett for lokal distribusjon ------------------------

ld_bd_order <- names(Grunnlagsdata_ld)

Grunnlagsdata_ld_reference <- basedata_reference %>%
        select(any_of(ld_bd_order))

rm(ld_bd_order)


# Opprett referansedatasett for regional distribusjon ---------------------

rd_bd_order <- names(Grunnlagsdata_rd)

Grunnlagsdata_rd_reference <- basedata_reference %>%
        select(any_of(rd_bd_order))

rm(rd_bd_order)


# Sammenligning av grunnlagsdata for lokal distribusjon -------------------

keys <- c(
        "orgn",
        "id.y",
        "id",
        "y",
        "comp"
)

join_keys <- c(
        "id",
        "y"
)


# Kontroller at nødvendige nøkkelkolonner finnes --------------------------

missing_keys_gld <- setdiff(
        keys,
        names(Grunnlagsdata_ld)
)

missing_keys_ref <- setdiff(
        keys,
        names(Grunnlagsdata_ld_reference)
)

if (length(missing_keys_gld) > 0) {
        
        stop(
                paste0(
                        "Følgende nøkkelkolonner mangler i ",
                        "Grunnlagsdata_ld: ",
                        paste(missing_keys_gld, collapse = ", ")
                )
        )
}

if (length(missing_keys_ref) > 0) {
        
        stop(
                paste0(
                        "Følgende nøkkelkolonner mangler i ",
                        "Grunnlagsdata_ld_reference: ",
                        paste(missing_keys_ref, collapse = ", ")
                )
        )
}


# Klargjør det nye datasettet ---------------------------------------------

gld <- Grunnlagsdata_ld %>%
        mutate(
                orgn = as.character(orgn),
                id.y = as.character(id.y),
                id = as.character(id),
                y = suppressWarnings(as.integer(y)),
                comp = as.character(comp)
        )


# Klargjør referansedatasettet --------------------------------------------

gld_ref <- Grunnlagsdata_ld_reference %>%
        mutate(
                orgn = as.character(orgn),
                id.y = as.character(id.y),
                id = as.character(id),
                y = suppressWarnings(as.integer(y)),
                comp = as.character(comp)
        )


# Kontroller at id og år ikke mangler -------------------------------------

missing_id_year_gld <- gld %>%
        filter(
                is.na(id) |
                        id == "" |
                        is.na(y)
        )

missing_id_year_ref <- gld_ref %>%
        filter(
                is.na(id) |
                        id == "" |
                        is.na(y)
        )

if (nrow(missing_id_year_gld) > 0) {
        
        stop(
                paste0(
                        "Grunnlagsdata_ld inneholder ",
                        nrow(missing_id_year_gld),
                        " rader der id eller år mangler."
                )
        )
}

if (nrow(missing_id_year_ref) > 0) {
        
        stop(
                paste0(
                        "Grunnlagsdata_ld_reference inneholder ",
                        nrow(missing_id_year_ref),
                        " rader der id eller år mangler."
                )
        )
}


# Kontroller at det bare finnes én rad per id og år ------------------------

duplicate_gld <- gld %>%
        count(
                id,
                y,
                name = "antall"
        ) %>%
        filter(antall > 1)

duplicate_gld_ref <- gld_ref %>%
        count(
                id,
                y,
                name = "antall"
        ) %>%
        filter(antall > 1)

if (nrow(duplicate_gld) > 0) {
        
        duplicate_examples <- duplicate_gld %>%
                slice_head(n = 10) %>%
                mutate(
                        eksempel = paste0(
                                "id = ",
                                id,
                                ", år = ",
                                y,
                                ", antall = ",
                                antall
                        )
                ) %>%
                pull(eksempel)
        
        stop(
                paste0(
                        "Grunnlagsdata_ld inneholder flere rader ",
                        "for samme id og år. Eksempler: ",
                        paste(duplicate_examples, collapse = "; ")
                )
        )
}

if (nrow(duplicate_gld_ref) > 0) {
        
        duplicate_examples_ref <- duplicate_gld_ref %>%
                slice_head(n = 10) %>%
                mutate(
                        eksempel = paste0(
                                "id = ",
                                id,
                                ", år = ",
                                y,
                                ", antall = ",
                                antall
                        )
                ) %>%
                pull(eksempel)
        
        stop(
                paste0(
                        "Grunnlagsdata_ld_reference inneholder flere rader ",
                        "for samme id og år. Eksempler: ",
                        paste(duplicate_examples_ref, collapse = "; ")
                )
        )
}


# Finn variabler som finnes i begge datasettene ---------------------------

measures_gld <- setdiff(
        names(gld),
        keys
)

measures_ref <- setdiff(
        names(gld_ref),
        keys
)

common_measures <- intersect(
        measures_gld,
        measures_ref
)


# Definer variabler som ikke skal inngå i kontrollen ----------------------

excluded_named_measures <- c(
        "ld_TOTXDEA",
        "av_ld_pens",
        "av_ld_impl",
        "av_ld_pens.eq",
        "ld_nl.NOK"
)


# Finn fp_-variabler som utelates -----------------------------------------

excluded_fp_measures <- union(
        measures_gld[
                stringr::str_detect(
                        measures_gld,
                        stringr::regex(
                                "^fp_",
                                ignore_case = TRUE
                        )
                )
        ],
        measures_ref[
                stringr::str_detect(
                        measures_ref,
                        stringr::regex(
                                "^fp_",
                                ignore_case = TRUE
                        )
                )
        ]
)

excluded_fp_measures <- sort(
        unique(excluded_fp_measures)
)


# Finn særskilt angitte variabler som utelates ----------------------------

excluded_specific_measures <- union(
        measures_gld[
                stringr::str_to_lower(measures_gld) %in%
                        stringr::str_to_lower(excluded_named_measures)
        ],
        measures_ref[
                stringr::str_to_lower(measures_ref) %in%
                        stringr::str_to_lower(excluded_named_measures)
        ]
)

excluded_specific_measures <- sort(
        unique(excluded_specific_measures)
)


# Fjern variabler som ikke skal inngå i kontrollen ------------------------

common_measures <- common_measures[
        !stringr::str_detect(
                common_measures,
                stringr::regex(
                        "^fp_",
                        ignore_case = TRUE
                )
        ) &
                !stringr::str_to_lower(common_measures) %in%
                stringr::str_to_lower(excluded_named_measures)
]

if (length(common_measures) == 0) {
        
        stop(
                paste0(
                        "Fant ingen variabler som finnes i begge datasettene ",
                        "etter at variablene som ikke skal kontrolleres, ",
                        "ble utelatt."
                )
        )
}


# Kontrollregler for særskilte variabler ----------------------------------


# Toleranse for ldz-variabler ---------------------------------------------
#
# For variabler som begynner med ldz, registreres en endring bare dersom
# den absolutte differansen er minst 0,01.

ldz_tolerance <- 1e-2


# Funksjon som identifiserer ldz-variabler --------------------------------

is_ldz_measure <- function(x) {
        
        stringr::str_detect(
                x,
                stringr::regex(
                        "^ldz",
                        ignore_case = TRUE
                )
        )
}


# Oversikt over ldz-variabler med egen kontrollregel ----------------------

ldz_measures <- common_measures[
        vapply(
                common_measures,
                is_ldz_measure,
                logical(1)
        )
]

ldz_measures <- sort(
        unique(ldz_measures)
)


# Relativ endringsgrense for ld_rab.gf og ld_rab.sf -----------------------
#
# En endring registreres bare dersom den relative endringen er større
# enn 0,1 prosent.
#
# 0,1 prosent uttrykt som desimaltall er 0,001.

rab_relative_limit <- 0.001

rab_relative_measure_names <- c(
        "ld_rab.gf",
        "ld_rab.sf"
)


# Funksjon som identifiserer rab-variablene -------------------------------

is_rab_relative_measure <- function(x) {
        
        stringr::str_to_lower(x) %in%
                stringr::str_to_lower(rab_relative_measure_names)
}


# Oversikt over rab-variabler med relativ kontrollregel -------------------

rab_relative_measures <- common_measures[
        vapply(
                common_measures,
                is_rab_relative_measure,
                logical(1)
        )
]

rab_relative_measures <- sort(
        unique(rab_relative_measures)
)


# Kontroller at begge rab-variablene er funnet ----------------------------

missing_rab_relative_measures <- setdiff(
        stringr::str_to_lower(rab_relative_measure_names),
        stringr::str_to_lower(rab_relative_measures)
)

if (length(missing_rab_relative_measures) > 0) {
        
        warning(
                paste0(
                        "Følgende rab-variabler ble ikke funnet blant ",
                        "variablene som inngår i kontrollen: ",
                        paste(
                                missing_rab_relative_measures,
                                collapse = ", "
                        )
                )
        )
}


# Funksjon for sikker konvertering til numeriske verdier ------------------

convert_to_numeric <- function(x) {
        
        if (is.numeric(x)) {
                return(as.numeric(x))
        }
        
        x <- as.character(x)
        
        # Fjern alle mellomrom
        x <- stringr::str_replace_all(
                x,
                "\\s",
                ""
        )
        
        # Erstatt desimalkomma med punktum
        x <- stringr::str_replace_all(
                x,
                ",",
                "."
        )
        
        suppressWarnings(
                as.numeric(x)
        )
}


# Konverter sammenligningsvariablene til numeriske verdier ----------------

gld <- gld %>%
        mutate(
                across(
                        all_of(common_measures),
                        convert_to_numeric
                )
        )

gld_ref <- gld_ref %>%
        mutate(
                across(
                        all_of(common_measures),
                        convert_to_numeric
                )
        )


# Klargjør nytt datasett for kobling --------------------------------------

gld_compare <- gld %>%
        select(
                all_of(keys),
                all_of(common_measures)
        )


# Klargjør referansedatasettet for kobling --------------------------------

gld_ref_compare <- gld_ref %>%
        select(
                all_of(keys),
                all_of(common_measures)
        ) %>%
        rename(
                orgn_ref_metadata = orgn,
                id.y_ref_metadata = id.y,
                comp_ref_metadata = comp
        ) %>%
        rename_with(
                ~ paste0(.x, "_ref"),
                all_of(common_measures)
        )


# Finn år som finnes i begge datasettene ----------------------------------

years_gld <- sort(
        unique(
                gld_compare$y
        )
)

years_ref <- sort(
        unique(
                gld_ref_compare$y
        )
)

shared_years <- sort(
        intersect(
                years_gld,
                years_ref
        )
)

excluded_years <- sort(
        setdiff(
                union(
                        years_gld,
                        years_ref
                ),
                shared_years
        )
)

if (length(shared_years) == 0) {
        
        stop(
                paste0(
                        "Datasettene har ingen felles år. ",
                        "Det er derfor ikke mulig å sammenligne dem."
                )
        )
}


# Finn kombinasjoner av id og år som finnes i begge datasettene -----------
#
# Samme id og år må finnes i begge datasettene for at observasjonen skal
# inngå i kontrollen.

shared_id_years <- inner_join(
        gld_compare %>%
                distinct(
                        id,
                        y
                ),
        gld_ref_compare %>%
                distinct(
                        id,
                        y
                ),
        by = join_keys
)

if (nrow(shared_id_years) == 0) {
        
        stop(
                paste0(
                        "Fant ingen kombinasjoner av id og år ",
                        "som finnes i begge datasettene."
                )
        )
}


# Finn id-år-kombinasjoner som utelates -----------------------------------

id_years_gld <- gld_compare %>%
        distinct(
                id,
                y
        )

id_years_ref <- gld_ref_compare %>%
        distinct(
                id,
                y
        )

excluded_id_years <- full_join(
        id_years_gld %>%
                mutate(
                        finnes_i_nytt_datasett = TRUE
                ),
        id_years_ref %>%
                mutate(
                        finnes_i_referansedatasett = TRUE
                ),
        by = join_keys
) %>%
        mutate(
                finnes_i_nytt_datasett = coalesce(
                        finnes_i_nytt_datasett,
                        FALSE
                ),
                finnes_i_referansedatasett = coalesce(
                        finnes_i_referansedatasett,
                        FALSE
                )
        ) %>%
        filter(
                !finnes_i_nytt_datasett |
                        !finnes_i_referansedatasett
        ) %>%
        mutate(
                årsak = case_when(
                        finnes_i_nytt_datasett &
                                !finnes_i_referansedatasett ~
                                "Finnes bare i nytt datasett",
                        
                        !finnes_i_nytt_datasett &
                                finnes_i_referansedatasett ~
                                "Finnes bare i referansedatasettet",
                        
                        TRUE ~
                                "Ukjent"
                )
        ) %>%
        arrange(
                id,
                y
        )


# Begrens begge datasettene til felles id-år-kombinasjoner ----------------

gld_compare_shared <- gld_compare %>%
        semi_join(
                shared_id_years,
                by = join_keys
        )

gld_ref_compare_shared <- gld_ref_compare %>%
        semi_join(
                shared_id_years,
                by = join_keys
        )


# Koble datasettene på samme id og samme år -------------------------------

joined_simple <- inner_join(
        gld_compare_shared,
        gld_ref_compare_shared,
        by = join_keys
) %>%
        mutate(
                orgn = coalesce(
                        orgn,
                        orgn_ref_metadata
                ),
                id.y = coalesce(
                        id.y,
                        id.y_ref_metadata
                ),
                comp = coalesce(
                        comp,
                        comp_ref_metadata
                )
        ) %>%
        select(
                -orgn_ref_metadata,
                -id.y_ref_metadata,
                -comp_ref_metadata
        )


# Beregn faktisk differanse for hver variabel ------------------------------

result <- joined_simple

for (nm in common_measures) {
        
        ref_col <- paste0(
                nm,
                "_ref"
        )
        
        diff_col <- paste0(
                "diff_",
                nm
        )
        
        result[[diff_col]] <-
                result[[nm]] - result[[ref_col]]
}


# Opprett bredt parvis datasett -------------------------------------------
#
# Dette datasettet inneholder alle felles id-år-kombinasjoner.
# Kontrollgrensene brukes først når kontroll_diff opprettes.

ordered_cols <- c(
        keys,
        as.vector(
                unlist(
                        lapply(
                                common_measures,
                                function(nm) {
                                        
                                        c(
                                                paste0(nm, "_ref"),
                                                nm,
                                                paste0("diff_", nm)
                                        )
                                }
                        )
                )
        )
)

Grunnlagsdata_ld_pairwise <- result %>%
        select(
                any_of(ordered_cols)
        ) %>%
        arrange(
                id,
                y
        )


# Hent ut forskjeller som oppfyller kontrollreglene -----------------------
#
# Regler:
#
# 1. ld_rab.gf og ld_rab.sf:
#    Relativ endring må være større enn 0,1 prosent.
#
# 2. Variabler som begynner med ldz:
#    Absolutt differanse må være minst 0,0001.
#
# 3. Øvrige variabler:
#    Enhver faktisk forskjell registreres.
#
# 4. Nye og fjernede verdier:
#    Registreres uavhengig av kontrollgrensene.
#
# Relativ endring beregnes slik:
#
# abs(ny verdi - referanseverdi) / abs(referanseverdi)
#
# Dersom referanseverdien for en rab-variabel er null, kan relativ endring
# ikke beregnes. Enhver faktisk endring fra null registreres derfor.

kontroll_diff <- purrr::map_dfr(
        common_measures,
        function(nm) {
                
                ref_col <- paste0(
                        nm,
                        "_ref"
                )
                
                # Identifiser hvilken kontrollregel som gjelder
                is_ldz <- is_ldz_measure(nm)
                is_rab_relative <- is_rab_relative_measure(nm)
                
                result %>%
                        mutate(
                                # Faktisk differanse
                                differanse_kontroll =
                                        .data[[nm]] -
                                        .data[[ref_col]],
                                
                                # Absolutt differanse
                                absolutt_differanse_kontroll = abs(
                                        differanse_kontroll
                                ),
                                
                                # Relativ endring for ld_rab.gf og ld_rab.sf
                                relativ_endring = case_when(
                                        is_rab_relative &
                                                !is.na(.data[[nm]]) &
                                                !is.na(.data[[ref_col]]) &
                                                .data[[ref_col]] != 0 ~
                                                absolutt_differanse_kontroll /
                                                abs(.data[[ref_col]]),
                                        
                                        TRUE ~
                                                NA_real_
                                ),
                                
                                # Kontroller om endringen oppfyller regelen
                                verdi_endret = case_when(
                                        # ld_rab.gf og ld_rab.sf:
                                        # Relativ endring må være større
                                        # enn 0,1 prosent
                                        is_rab_relative &
                                                !is.na(.data[[ref_col]]) &
                                                .data[[ref_col]] != 0 ~
                                                relativ_endring >
                                                rab_relative_limit,
                                        
                                        # Rab-variabel med referanseverdi 0:
                                        # Enhver faktisk endring registreres
                                        is_rab_relative &
                                                !is.na(.data[[ref_col]]) &
                                                .data[[ref_col]] == 0 ~
                                                absolutt_differanse_kontroll >
                                                0,
                                        
                                        # Ldz-variabler:
                                        # Absolutt differanse må være minst
                                        # 0,0001
                                        is_ldz ~
                                                absolutt_differanse_kontroll >=
                                                ldz_tolerance,
                                        
                                        # Øvrige variabler:
                                        # Enhver faktisk forskjell
                                        TRUE ~
                                                .data[[nm]] !=
                                                .data[[ref_col]]
                                ),
                                
                                # Absolutt grense gjelder bare for
                                # ldz-variabler
                                absolutt_endringsgrense = case_when(
                                        is_ldz ~
                                                ldz_tolerance,
                                        
                                        TRUE ~
                                                NA_real_
                                ),
                                
                                # Relativ grense gjelder bare for
                                # ld_rab.gf og ld_rab.sf
                                relativ_endringsgrense = case_when(
                                        is_rab_relative ~
                                                rab_relative_limit,
                                        
                                        TRUE ~
                                                NA_real_
                                )
                        ) %>%
                        filter(
                                # Begge datasettene har verdi, og endringen
                                # oppfyller kontrollregelen
                                (
                                        !is.na(.data[[nm]]) &
                                                !is.na(.data[[ref_col]]) &
                                                coalesce(
                                                        verdi_endret,
                                                        FALSE
                                                )
                                ) |
                                        
                                        # Ny verdi
                                        (
                                                !is.na(.data[[nm]]) &
                                                        is.na(
                                                                .data[[ref_col]]
                                                        )
                                        ) |
                                        
                                        # Verdi fjernet
                                        (
                                                is.na(.data[[nm]]) &
                                                        !is.na(
                                                                .data[[ref_col]]
                                                        )
                                        )
                        ) %>%
                        transmute(
                                id,
                                orgn,
                                id.y,
                                y,
                                comp,
                                variabel = nm,
                                verdi_reference = .data[[ref_col]],
                                verdi_ny = .data[[nm]],
                                differanse = differanse_kontroll,
                                absolutt_differanse =
                                        absolutt_differanse_kontroll,
                                relativ_endring,
                                prosentvis_endring =
                                        relativ_endring * 100,
                                absolutt_endringsgrense,
                                relativ_endringsgrense,
                                prosentvis_endringsgrense =
                                        relativ_endringsgrense * 100,
                                kontrollregel = case_when(
                                        is_rab_relative ~
                                                paste0(
                                                        "Relativ endring ",
                                                        "større enn ",
                                                        rab_relative_limit *
                                                                100,
                                                        " prosent"
                                                ),
                                        
                                        is_ldz ~
                                                paste0(
                                                        "Absolutt endring ",
                                                        "minst ",
                                                        format(
                                                                ldz_tolerance,
                                                                scientific =
                                                                        FALSE
                                                        )
                                                ),
                                        
                                        TRUE ~
                                                "Enhver faktisk forskjell"
                                ),
                                type_endring = case_when(
                                        is.na(.data[[ref_col]]) &
                                                !is.na(.data[[nm]]) ~
                                                "Ny verdi",
                                        
                                        !is.na(.data[[ref_col]]) &
                                                is.na(.data[[nm]]) ~
                                                "Verdi fjernet",
                                        
                                        TRUE ~
                                                "Verdi endret"
                                )
                        )
        }
) %>%
        arrange(
                id,
                y,
                variabel
        )


# Oppsummering av endringene ----------------------------------------------

oppsummering_diff <- kontroll_diff %>%
        count(
                type_endring,
                variabel,
                name = "antall"
        ) %>%
        arrange(
                type_endring,
                desc(antall),
                variabel
        )


# Oppsummering per selskap og år ------------------------------------------

oppsummering_diff_id_year <- kontroll_diff %>%
        count(
                id,
                orgn,
                y,
                comp,
                name = "antall_endringer"
        ) %>%
        arrange(
                id,
                y
        )


# Kontroll av at rab-regelen faktisk brukes -------------------------------

rab_rule_check <- tibble(
        variabel = rab_relative_measure_names,
        identifisert_som_rab = vapply(
                rab_relative_measure_names,
                is_rab_relative_measure,
                logical(1)
        ),
        finnes_i_common_measures =
                stringr::str_to_lower(rab_relative_measure_names) %in%
                stringr::str_to_lower(common_measures)
)


# Kontrollmeldinger -------------------------------------------------------

message(
        "Antall variabler som inngår i sammenligningen: ",
        length(common_measures)
)

message(
        "Antall fp_-variabler som er utelatt: ",
        length(excluded_fp_measures)
)

if (length(excluded_fp_measures) > 0) {
        
        message(
                "fp_-variabler som er utelatt: ",
                paste(
                        excluded_fp_measures,
                        collapse = ", "
                )
        )
}

message(
        "Antall særskilt angitte variabler som er utelatt: ",
        length(excluded_specific_measures)
)

if (length(excluded_specific_measures) > 0) {
        
        message(
                "Særskilt angitte variabler som er utelatt: ",
                paste(
                        excluded_specific_measures,
                        collapse = ", "
                )
        )
}

message(
        "Antall ldz-variabler med absolutt endringsgrense på ",
        format(
                ldz_tolerance,
                scientific = FALSE
        ),
        ": ",
        length(ldz_measures)
)

if (length(ldz_measures) > 0) {
        
        message(
                "ldz-variabler med absolutt endringsgrense: ",
                paste(
                        ldz_measures,
                        collapse = ", "
                )
        )
}

message(
        "Antall rab-variabler med relativ endringsgrense: ",
        length(rab_relative_measures)
)

if (length(rab_relative_measures) > 0) {
        
        message(
                "Rab-variabler med relativ endringsgrense: ",
                paste(
                        rab_relative_measures,
                        collapse = ", "
                )
        )
        
        message(
                "Den relative endringen for rab-variablene må være ",
                "større enn ",
                rab_relative_limit * 100,
                " prosent."
        )
}

message(
        "År som finnes i begge datasettene: ",
        paste(
                shared_years,
                collapse = ", "
        )
)

if (length(excluded_years) > 0) {
        
        message(
                "År som er utelatt fordi de ikke finnes i begge datasettene: ",
                paste(
                        excluded_years,
                        collapse = ", "
                )
        )
        
} else {
        
        message(
                "Ingen hele år er utelatt fra sammenligningen."
        )
}

message(
        "Antall felles kombinasjoner av id og år: ",
        nrow(shared_id_years)
)

message(
        "Antall id-år-kombinasjoner som er utelatt: ",
        nrow(excluded_id_years)
)

message(
        "Antall forskjeller som oppfyller kontrollreglene: ",
        nrow(kontroll_diff)
)


# Resultatobjekter ---------------------------------------------------------

# Variabler som inngår i sammenligningen:
common_measures

# Variabler med fp_-prefiks som er utelatt:
excluded_fp_measures

# Særskilt angitte variabler som er utelatt:
excluded_specific_measures

# Ldz-variabler med absolutt endringsgrense:
ldz_measures

# Absolutt endringsgrense for ldz-variabler:
ldz_tolerance

# Rab-variabler med relativ endringsgrense:
rab_relative_measures

# Relativ endringsgrense for rab-variablene:
rab_relative_limit

# Kontroll av at rab-variablene gjenkjennes riktig:
rab_rule_check

# År som finnes i begge datasettene:
shared_years

# År som ikke finnes i begge datasettene:
excluded_years

# Kombinasjoner av id og år som finnes i begge datasettene:
shared_id_years

# Kombinasjoner av id og år som er utelatt:
excluded_id_years

# Bredt datasett med referanseverdi, ny verdi og differanse:
Grunnlagsdata_ld_pairwise

# Langt datasett med forskjeller som oppfyller relevant kontrollregel:
kontroll_diff

# Antall endringer per variabel og endringstype:
oppsummering_diff

# Antall endringer per selskap og år:
oppsummering_diff_id_year