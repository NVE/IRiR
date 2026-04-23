# Kode for aa beregne annuiteter

# Tenkt struktur 

# Loop gjennom alle "i" selskaps unike orgnr i dat
# Hent data basert på matchende ORGN fra

# Bruttoinvesteringer .\Data\BaseData\Annuities_Gross_Investments Stopp koden om det ikke finnes data for gitt selskap i

# Kjopte anlegg .\Data\BaseData\Annuities_Bought_Investments Stopp koden om det ikke finnes data for gitt selskap i

# Restlevetider kjopte anlegg .\Data\BaseData\Annuities_RL_Bought_Investments Stopp koden om det ikke finnes data for gitt selskap i

# Sjekk at rapporteringsaarene stemmer overens med aarene i y.avg og at det er rapportert for alle aar. Hvis ikke stopp koden gi beskjed om selskap og aar.

# Regn annuitetene

# Hvis laveste y < 1996. (Selskapet har rapportert anskaffelskostnadene direkte på de aarene anleggene ble bygd)
# Inflasjonsjuster
# Regn sum annuiteter aar for aar


# Hvis laveste y = 1996 OG SUM for alle anleggskategorier i 1996 > 0 (Selskapene har en "aapningsbalanse" fra 1996)
# Lag logikk for fordele anskaffelseskostnadene pr 1996 bakover basert paa TEK-vekter.
# Her har vi logikk vi kan ta utgangspunkt i fra Statnett

# Inflasjonsjuster
# Regn summer aar for aar

# Hvis laveste y=1996 og SUM for alle anleggskategorier i 1996 = (selskapet har ingen anlegg eldre enn 1996)

# Inflasjonsjuster
# Regn summer aar for aar

# Lagre annuitetene pr id.y (ny variabel r_ann.capex)


# Merge annuitetene pr id.y til dat

# Sjekk at ingen selskap har r_bv.sf eller r_bv.gf uten aa ha r_ann.capex

Sys.setlocale("LC_CTYPE", "nb_NO.UTF-8")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readxl)
  library(fs)
  library(pxweb)
  library(openxlsx)
})

# ============================================================
# 0) Konfig
# ============================================================

annuity_cfg_default <- list(
  base_dir = "Data/BaseData",
  dir_gi   = "Annuities_Gross_Investments",
  dir_bi   = "Annuities_Bought_Investments",
  dir_birl = "Annuities_RL_Bought_Investments",
  
  pattern_gi   = "^GI_(\\d+).*\\.(xlsx|xls)$",
  pattern_bi   = "^BI_(\\d+).*\\.(xlsx|xls)$",
  pattern_birl = "^BI_RL_(\\d+).*\\.(xlsx|xls)$",
  
  r = 0.04,
  base_price_year = 2024,
  
  missing_file_policy = "stop",  # "stop" eller "skip"
  
  parse_numbers_no = TRUE,
  tol = 1e-8,
  
  # Hvor mange "eksempelrader" som vises i feilmeldinger
  n_examples = 8,
  
  update_kpi = TRUE,
  update_tek = FALSE
)

# ============================================================
# 1) Filhåndtering
# ============================================================

find_company_file <- function(dir_path, pattern, orgnr) {
  files <- fs::dir_ls(dir_path, regexp = pattern, recurse = FALSE)
  if (length(files) == 0) return(NA_character_)
  tibble(path = files) |>
    mutate(file = basename(path),
           org = str_match(file, pattern)[,2]) |>
    filter(!is.na(org), org == as.character(orgnr)) |>
    slice(1) |>
    pull(path) |>
    (\(x) if (length(x) == 0) NA_character_ else x)()
}

match_year_sheet <- function(sheets, year) {
  y_chr <- as.character(year)
  idx <- which(str_trim(sheets) == y_chr)
  if (length(idx) == 0) return(NA_character_)
  sheets[idx[1]]
}

assert_year_sheets_exist <- function(path, years, label) {
  sheets <- readxl::excel_sheets(path)
  missing <- years[is.na(map_chr(years, \(yy) match_year_sheet(sheets, yy)))]
  if (length(missing) > 0) {
    stop(label, ": Mangler faner for år: ", paste(missing, collapse = ", "),
         " i fil: ", basename(path))
  }
  invisible(TRUE)
}

# ============================================================
# 2) Tall-parsing / kategorinormalisering
# ============================================================

parse_num_no <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\s", "")
  x <- str_replace_all(x, "\u00A0", "")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

to_num <- function(x, parse_no = TRUE) {
  if (parse_no) parse_num_no(x) else suppressWarnings(as.numeric(x))
}

norm_cat <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    str_replace_all("\\s+", " ")
}

detect_year_col <- function(nms) {
  candidates <- c("år", "aar", "year", "y")
  hit <- intersect(tolower(nms), candidates)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# ============================================================
# 3) Lesing: faner = rapporteringsår, og "År" inne i fanen
#    Return long: y_rep, y_vintage, category, value
#    Viktig: Tomme celler => 0 (ikke droppes)
# ============================================================

standardize_sheet_vintage_table <- function(df, y_rep, parse_no = TRUE, label = "GI/BI") {
  if (is.null(df) || nrow(df) == 0) stop(label, ": Tom fane for ", y_rep)
  
  names(df) <- tolower(names(df))
  year_col <- detect_year_col(names(df))
  if (is.na(year_col)) {
    stop(label, ": Fant ikke 'År'-kolonne i fane ", y_rep,
         ". Forventer kolonnenavn som År/Aar/Year/y.")
  }
  
  out <- df |>
    mutate(y_vintage = as.integer(.data[[year_col]])) |>
    select(-all_of(year_col)) |>
    pivot_longer(cols = everything(), names_to = "category", values_to = "value_raw") |>
    mutate(
      y_rep     = as.integer(y_rep),
      y_vintage = as.integer(y_vintage),
      category  = norm_cat(category),
      value     = to_num(value_raw, parse_no)
    ) |>
    select(y_rep, y_vintage, category, value) |>
    filter(!is.na(y_vintage))
  
  # Tomme celler betyr 0:
  out <- out |> mutate(value = coalesce(value, 0))
  
  # SJEKK: siste År (maks) skal være lik fanenavn/rapporteringsår
  max_y <- suppressWarnings(max(out$y_vintage, na.rm = TRUE))
  if (is.finite(max_y) && max_y != as.integer(y_rep)) {
    stop(label, ": I fane ", y_rep, " er max(År)=", max_y,
         ", men skal være lik rapporteringsår/fanenavn=", y_rep, ".")
  }
  
  out
}

read_workbook_vintage <- function(path, years, parse_no = TRUE, label = "GI/BI") {
  assert_year_sheets_exist(path, years, label)
  sheets <- readxl::excel_sheets(path)
  
  map_dfr(years, \(yy) {
    sh <- match_year_sheet(sheets, yy)
    df <- readxl::read_excel(path, sheet = sh, .name_repair = "unique_quiet")
    standardize_sheet_vintage_table(df, y_rep = yy, parse_no = parse_no, label = label)
  })
}

# RL har samme struktur; tomme celler => 0 år
read_workbook_rl <- function(path, years, parse_no = TRUE, label = "BI_RL") {
  x <- read_workbook_vintage(path, years, parse_no, label)
  x |> rename(rl = value) |> mutate(rl = coalesce(rl, 0))
}

# ============================================================
# 4) 1996-logikk per rapporteringsår (fane) for GI/BI
# ============================================================

allocate_opening_balance_1996_backwards <- function(df_yrep, tek_weights_tbl) {
  # tek_weights_tbl: category, y (vintage), weight (sum=1 pr category)

  
  opening <- df_yrep |> filter(y_vintage == 1996) |> select(category, value)
  
  pre <- opening |>
    left_join(tek_weights_tbl, by = "category") |>
    transmute(
      y_rep     = unique(df_yrep$y_rep)[1],
      y_vintage = as.integer(y),
      category  = category,
      value     = value * weight
    )
  
  bind_rows(df_yrep, pre)
}

apply_1996_logic_per_yrep <- function(df_long, tek_weights_tbl = NULL) {
  df_long |>
    group_by(y_rep) |>
    group_modify(\(.x, .g) {
      min_y <- suppressWarnings(min(.x$y_vintage, na.rm = TRUE))
      sum_1996 <- .x |>
        filter(y_vintage == 1996) |>
        summarise(s = sum(value, na.rm = TRUE)) |>
        pull(s)
      sum_1996 <- ifelse(is.na(sum_1996), 0, sum_1996)
      
      if (min_y < 1996) return(.x)
      if (min_y == 1996 && sum_1996 > 0) return(allocate_opening_balance_1996_backwards(.x, tek_weights_tbl))
      .x
    }) |>
    ungroup()
}

# ============================================================
# 5) Inflasjon etter vintage-år
# ============================================================

inflate_to_base <- function(df_long, kpi_tbl, base_year) {
  if (is.null(kpi_tbl) || !all(c("y","kpi") %in% names(kpi_tbl))) {
    stop("kpi_tbl må ha kolonnene y og kpi.")
  }
  kpi_base <- kpi_tbl |> filter(y == base_year) |> pull(kpi)
  if (length(kpi_base) != 1) stop("Fant ikke unik KPI for base_year=", base_year)
  
  df_long |>
    left_join(kpi_tbl, by = c("y_vintage" = "y")) |>
    mutate(
      kpi = coalesce(kpi, NA_real_),
      value_inflated = value * (kpi_base / kpi)
    ) |>
    select(-kpi)
}

# ============================================================
# 6) Annuitet: standard og BI med RL ved kjøpsår
# ============================================================

annuity_payment <- function(P, r, n) {
  ifelse(r == 0, P / n, P * (r / (1 - (1 + r)^(-n))))
}

compute_annuity_standard <- function(df_infl, lifetime_map, r) {
  x <- df_infl |> left_join(lifetime_map, by = "category")
  
  if (any(is.na(x$n_years))) {
    bad <- x |> filter(is.na(n_years)) |> distinct(category) |> pull(category)
    stop("Mangler standard levetid i lifetime_map for: ", paste(bad, collapse = ", "))
  }
  
  x |>
    mutate(active = (y_rep >= y_vintage) & (y_rep <= (y_vintage + n_years - 1))) |>
    filter(active) |>
    mutate(A = annuity_payment(value_inflated, r = r, n = n_years)) |>
    group_by(y = y_rep) |>
    summarise(r_ann = sum(A, na.rm = TRUE), .groups = "drop")
}

# RL er "gjenstående levetid ved kjøpsår (= y_vintage)" og står uendret i alle faner.
# Derfor: aktiv i rapportår hvis y_rep <= y_vintage + rl - 1
compute_annuity_bi_with_rl_purchaseyear <- function(df_infl_bi, rl_tbl, r) {
  x <- df_infl_bi |>
    left_join(rl_tbl, by = c("y_rep","y_vintage","category")) |>
    mutate(rl = coalesce(rl, 0))
  
  x |>
    mutate(active = (value_inflated > 0) & (rl > 0) &
             (y_rep >= y_vintage) & (y_rep <= (y_vintage + rl - 1))) |>
    filter(active) |>
    mutate(A = annuity_payment(value_inflated, r = r, n = rl)) |>
    group_by(y = y_rep) |>
    summarise(r_ann = sum(A, na.rm = TRUE), .groups = "drop")
}

# ============================================================
# 7) GI - BI først (celle for celle)
# ============================================================

compute_gi_minus_bi <- function(gi_long, bi_long, tol = 1e-8) {
  x <- full_join(
    gi_long |> rename(gi = value),
    bi_long |> rename(bi = value),
    by = c("y_rep","y_vintage","category")
  ) |>
    mutate(
      gi = coalesce(gi, 0),
      bi = coalesce(bi, 0),
      value = gi - bi
    ) |>
    select(y_rep, y_vintage, category, value)
  
  # sanity: BI bør ikke overstige GI i samme celle
  bad <- x |> filter(value < -tol)
  if (nrow(bad) > 0) {
    ex <- bad |> slice(1)
    stop("GI - BI ga negative verdier (< -tol). Eksempel: y_rep=", ex$y_rep,
         ", År=", ex$y_vintage, ", kategori=", ex$category,
         ", GI-BI=", ex$value, ". (BI > GI eller kategori-mismatch).")
  }
  
  x
}

# ============================================================
# 8) Konsistenssjekk: BI>0 krever BI_RL>0 i samme celle
# ============================================================

check_bi_requires_rl <- function(bi_long, rl_long, orgnr, n_examples = 8) {
  # bi_long: y_rep,y_vintage,category,value
  # rl_long: y_rep,y_vintage,category,rl
  chk <- bi_long |>
    filter(value > 0) |>
    left_join(rl_long, by = c("y_rep","y_vintage","category")) |>
    mutate(rl = coalesce(rl, 0)) |>
    filter(rl <= 0)
  
  if (nrow(chk) > 0) {
    ex <- chk |>
      select(y_rep, y_vintage, category, bi_value = value, rl) |>
      arrange(y_rep, y_vintage, category) |>
      slice_head(n = n_examples)
    
    stop(
      "BI>0 uten tilsvarende BI_RL>0 for orgnr=", orgnr, ". Eksempler:\n",
      paste(apply(ex, 1, \(r) {
        paste0("  y_rep=", r[["y_rep"]],
               ", År=", r[["y_vintage"]],
               ", kategori='", r[["category"]], "'",
               ", BI=", r[["bi_value"]],
               ", RL=", r[["rl"]])
      }), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

# ============================================================
# 9) Beregn pr orgnr
# ============================================================

compute_annuity_for_orgnr <- function(dat_org,
                                      cfg,
                                      kpi_tbl,
                                      lifetime_map,
                                      tek_weights_tbl = NULL) {
  
  orgnr <- unique(dat_org$orgnr)
  if (length(orgnr) != 1) stop("dat_org må inneholde ett orgnr.")
  
  years_model <- sort(unique(dat_org$y))
  if (length(years_model) == 0) stop("Fant ingen år i dat_org$y.")
  
  dir_gi   <- file.path(cfg$base_dir, cfg$dir_gi)
  dir_bi   <- file.path(cfg$base_dir, cfg$dir_bi)
  dir_birl <- file.path(cfg$base_dir, cfg$dir_birl)
  
  gi_path   <- find_company_file(dir_gi,   cfg$pattern_gi,   orgnr)
  bi_path   <- find_company_file(dir_bi,   cfg$pattern_bi,   orgnr)
  birl_path <- find_company_file(dir_birl, cfg$pattern_birl, orgnr)
  
  missing_any <- any(is.na(c(gi_path, bi_path, birl_path)))
  if (missing_any) {
    msg <- paste0(
      "Mangler annuitetsfiler for orgnr=", orgnr, ": ",
      ifelse(is.na(gi_path), "GI ", ""),
      ifelse(is.na(bi_path), "BI ", ""),
      ifelse(is.na(birl_path), "BI_RL ", "")
    )
    if (identical(cfg$missing_file_policy, "stop")) stop(msg)
    if (identical(cfg$missing_file_policy, "skip")) {
      message(msg, " -> returnerer NA for r_ann.capex")
      return(dat_org |> distinct(id, y) |> mutate(r_ann.capex = NA_real_))
    }
  }
  
  # Les og normaliser
  gi_long <- read_workbook_vintage(gi_path, years_model, cfg$parse_numbers_no, label = "GI") |>
    mutate(category = norm_cat(category))
  bi_long <- read_workbook_vintage(bi_path, years_model, cfg$parse_numbers_no, label = "BI") |>
    mutate(category = norm_cat(category))
  rl_long <- read_workbook_rl(birl_path, years_model, cfg$parse_numbers_no, label = "BI_RL") |>
    mutate(category = norm_cat(category))
  
  # 1996-logikk per rapportår (GI/BI)
  gi_long <- apply_1996_logic_per_yrep(gi_long, tek_weights_tbl)
  bi_long <- apply_1996_logic_per_yrep(bi_long, tek_weights_tbl)
  
  # Konsistenskrav: BI>0 => RL>0 i samme (y_rep,År,kategori)
  check_bi_requires_rl(bi_long, rl_long, orgnr, n_examples = cfg$n_examples)
  
  # GI - BI først
  gi_net_long <- compute_gi_minus_bi(gi_long, bi_long, tol = cfg$tol)
  
  # Inflasjon etter vintage
  gi_net_infl <- inflate_to_base(gi_net_long, kpi_tbl, cfg$base_price_year)
  bi_infl     <- inflate_to_base(bi_long,     kpi_tbl, cfg$base_price_year)
  
  # Annuiteter:
  # 1) Egenbygde: GI - BI (standard levetid)
  ann_gi_net <- compute_annuity_standard(gi_net_infl, lifetime_map, r = cfg$r)
  
  # 2) Kjøpte: BI (RL ved kjøpsår, aktiv frem til kjøpsår + RL - 1)
  ann_bi_rl  <- compute_annuity_bi_with_rl_purchaseyear(bi_infl, rl_long, r = cfg$r)
  
  # Kombiner pr rapporteringsår
  r_ann <- tibble(y = years_model) |>
    left_join(ann_gi_net, by = "y") |>
    rename(r_ann_gi_net = r_ann) |>
    left_join(ann_bi_rl, by = "y") |>
    rename(r_ann_bi = r_ann) |>
    mutate(r_ann.capex = coalesce(r_ann_gi_net, 0) + coalesce(r_ann_bi, 0)) |>
    select(y, r_ann.capex)
  
  dat_org |>
    distinct(id, y) |>
    left_join(r_ann, by = "y")
}

# ============================================================
# 10) Kjør for alle orgnr, merge og sluttkontroll
# ============================================================

compute_annuiteter_all <- function(dat,
                                   lifetime_map,
                                   tek_weights_tbl = NULL,
                                   cfg = annuity_cfg_default) {
  
  stopifnot(all(c("orgnr","id","y") %in% names(dat)))
  stopifnot(all(c("category","n_years") %in% names(lifetime_map)))
  
  # KPI
  if (is.null(kpi_tbl) || isTRUE(cfg$update_kpi)) {
    kpi_tbl <- loading_kpi(cfg$update_kpi)
  }
  
  # TEK
  if (is.null(tek_weights_tbl)) || isTRUE(cfg$update_tek)) {
    tek_weights_tbl <- loading_tek_weights(cfg$update_tek)
  }
  
  
  ann_tbl <- dat |>
    distinct(orgnr) |>
    arrange(orgnr) |>
    mutate(res = map(orgnr, \(o) {
      dat_org <- dat |> filter(orgnr == o)
      compute_annuity_for_orgnr(dat_org, cfg, kpi_tbl, lifetime_map, tek_weights_tbl)
    })) |>
    select(res) |>
    unnest(res)
  
  dat2 <- dat |>
    left_join(ann_tbl, by = c("id","y"))
  
  # Sluttsjekk: Ingen selskap har r_bv.sf eller r_bv.gf uten r_ann.capex
  bv_cols <- intersect(c("r_bv.sf","r_bv.gf"), names(dat2))
  if (length(bv_cols) > 0) {
    bad <- dat2 |>
      mutate(has_bv = rowSums(across(all_of(bv_cols), ~ as.numeric(.) > 0), na.rm = TRUE) > 0) |>
      filter(has_bv, is.na(r_ann.capex)) |>
      distinct(orgnr, id)
    
    if (nrow(bad) > 0) {
      stop("Fant selskap med r_bv.sf/gf > 0 men uten r_ann.capex. Orgnr: ",
           paste(bad$orgnr, collapse = ", "))
    }
  }
  
  dat2
}



## Tester med innlesing av tilsendte data

setwd("~/GitHub/IRiR/")

# Orgnr. på de som foreløpig har innlevert
innleverte_data <- tibble::tibble(
  orgn = c(882783022, 926377841)
)

# Innleser navn og id
dat_test <- readxl::read_excel(paste0(annuity_cfg_default$base_dir,"/id_ir_26.xlsx",sep="")) |>
  right_join(innleverte_data)

##Levetider for de ulike anleggsklassene####
# Standard lifetimes hentet fra Sumicsid hovedrapport s 23
# 1991 verdier levert av statnett
# Levetider for kjøpteanlegg leveres med datasettet årlig

lifetimes <- tibble::tibble(
  category = c("linjer", "jordkabler", "sjøkabler", "hovedtransformator", "annet", "målere", "annet, kundespesifikke anlegg"),
  lifetimes = c(60,50,50,40,20,20,20))|>
  mutate(category = norm_cat(category))




# ============================================================
# XX) Henter KPI-tabell
# ============================================================

standardiser_kpi_df <- function(df) {
  names(df)[1:2] <- c("year", "kpi")
  df$year <- as.numeric(df$year)
  df
}


loading_kpi <- function(update_kpi){
  ## Import av KPI-data
  out_path <- file.path(
    annuity_cfg_default$base_dir,
    "df_kpi_annuities.xlsx"
  ) 
  
    if (isTRUE(update_kpi)){

    pxweb_query_list_09181 <- 
      list("NACE"=c("pub2X35"),
           "ContentsCode"=c("BIF4"),
           "Tid"=("*"))
    
    px_data_09181 <- 
      pxweb_get(url = "https://data.ssb.no/api/v0/no/table/09181",
                query = pxweb_query_list_09181)

    df_ssb_09181 <- as.data.frame(px_data_09181, column.name.type = "text", 
                                  variable.value.type = "text")
    df_ssb_09181 <- standardiser_kpi_df(df_ssb_09181[2:3])
    df_ssb_09181 <- df_ssb_09181 |> 
      filter(year>1970)

    pxweb_query_list_08184 <- 
      list("ContentsCode"=c("KpiAar"),
           "Tid"=c("*"))
    
    px_data_08184 <-
      pxweb_get(url= "https://data.ssb.no/api/v0/no/table/08184",
                query = pxweb_query_list_08184)
    
    df_ssb_08184 <- as.data.frame(px_data_08184, column.name.type = "text", 
                                  variable.value.type = "text")
    df_ssb_08184 <- standardiser_kpi_df(df_ssb_08184)
    df_ssb_08184 <- df_ssb_08184 |> 
      filter(between(year,1949,1970))
    
    df_kpi <- bind_rows(df_ssb_08184,df_ssb_09181) |>
      arrange(year) |>
            mutate(def_fac = cumprod(1 + kpi/100)) |>
      select(
        y =year,
        kpi=def_fac
      )
    
    
    write.xlsx(df_kpi, out_path, overwrite = TRUE)
    
    
  }
  else {
    df_kpi <- readxl::read_excel(out_path)
    
  }
  if is.null(df_kpi){
    stop("kpi er ikke innlastet")
  }
  return(df_kpi)
}


# ============================================================
# XX) Henter TEK-tabell
# ============================================================
loading_tek_weights <- function(update_tek){
  out_path <- file.path(
    annuity_cfg_default$base_dir,
    "df_tek_weights_annuities.xlsx"
  ) 
  
  if (isTRUE(update_kpi)){
    
    ###############################
    #### Denne delen skal erstattes med TEK-spørring
    ###############################
    
    set.seed(1234)
    orgnrs <- innleverte_data$orgn
    
    categories <- c(
      "linjer",
      "jordkabler",
      "sjøkabler",
      "hovedtransformator",
      "annet",
      "målere",
      "annet, kundespesifikke anlegg"
    )
    
    years <- 1975:1995
    
    df_tek_weights <- tibble(
      orgnr = orgnrs,
      category = categories
    ) |>
      expand_grid(y = years) |>
      group_by(orgnr, category) |>
      mutate(
        weight_raw = runif(n()),          # alltid > 0
        weight = weight_raw / sum(weight_raw)
      ) |>
      ungroup() |>
      select(orgnr, category, y, weight)
    
    ###############################
    ###############################
    ###############################
    
    write.xlsx(df_tek_weights, out_path, overwrite = TRUE)
  }
  
  else{
    df_tek_weights <- readxl::read_excel(out_path)
  }
  
}
