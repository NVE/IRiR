
############################################################
# RUN_IRIR_PANEL.R
# Wrapper for IRiR – panelanalyse over år + KILE-varianter
############################################################

# ---- 1. Setup -------------------------------------------------

rm(list = ls())

# Sett working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- 2. Henter KPI --------------------------------------------
# KPI til sammenstilling av resultater
source("load_kpi.R")


# ---- 3. Resultatbeholdere -------------------------------------

results_revcap <- list()
results_stage2 <- list()

save_stage2 <- TRUE
recompute_forutsetninger <- TRUE

# ---- 4. Loop over år ------------------------------------------

for (yr in years) {
  
  cat("\n==============================\n")
  cat("Running year:", yr, "\n")
  cat("==============================\n")
  
  current_year <- yr
  decision <- ifelse(yr == 2026, 0, 1)
  
  # Innleser/beregner forutsetninger
  file_path <- paste0("Data/forutsetninger_", current_year, ".Rdata")
  
  
  if (recompute_forutsetninger) {
    source("forutsetninger_script.R")
  } else {
    load(file_path)
  }
  
  }
  
  ############################################################
  # A: MED KILE
  ############################################################
  
  use_kile <- TRUE
  
  cat("Scenario: WITH KILE\n")
  
  source("NVE_IRiR.R")
  
  tmp <- RevCap
  tmp$year <- yr
  tmp$scenario <- "with_kile"
  
  results_revcap[[paste0(yr, "_kile")]] <- tmp
  
  if (save_stage2) {
    results_stage2[[paste0(yr, "_kile")]] <- stage2_results
  }
  
  # RESET miljø
  rm(list = setdiff(ls(), c("years", "results_revcap", "results_stage2",
                            "kpi_data", "current_year", "decision",
                            "yr", "save_stage2")))
  
  ############################################################
  # B: UTEN KILE
  ############################################################
  
  use_kile <- FALSE
  
  cat("Scenario: NO KILE\n")
  
  source("NVE_IRiR.R")
  
  # ---- B1: Ren uten KILE ----
  
  tmpA <- RevCap
  tmpA$year <- yr
  tmpA$scenario <- "no_kile_A"
  
  results_revcap[[paste0(yr, "_no_kile_A")]] <- tmpA
  
  if (save_stage2) {
    results_stage2[[paste0(yr, "_no_kile")]] <- stage2_results
  }
  
  # ---- B2: Uten KILE + aggregert KILE ----
  
  total_kile <- sum(KILE_vector, na.rm = TRUE)
  
  total_cost_A <- sum(totex_ex_kile, na.rm = TRUE)
  total_cost_B <- total_cost_A + total_kile
  
  calib_factor_A <- total_cost_A / sum(cost_norm, na.rm = TRUE)
  calib_factor_B <- total_cost_B / sum(cost_norm, na.rm = TRUE)
  
  cost_norm_B <- cost_norm * calib_factor_B
  
  # ⚠️ Krever egen funksjon basert på Stage 4
  RevCap_B <- calc_revcap(
    cost_norm = cost_norm_B,
    actual_cost = totex_ex_kile,
    other_inputs = other_inputs   # må defineres i IRiR
  )
  
  tmpB <- RevCap_B
  tmpB$year <- yr
  tmpB$scenario <- "no_kile_B"
  
  results_revcap[[paste0(yr, "_no_kile_B")]] <- tmpB
  
}

# ---- 7. Samle resultater --------------------------------------

revcap_df <- do.call(rbind, results_revcap)

# ---- 8. Deflater til 2025-priser ------------------------------

base_year <- 2025
kpi_base <- kpi_data$kpi[kpi_data$year == base_year]

revcap_df <- merge(revcap_df, kpi_data, by = "year", all.x = TRUE)

revcap_df$revcap_real <- revcap_df$revcap_nominal * (kpi_base / revcap_df$kpi)

# ---- 9. Lagre -------------------------------------------------

write.csv(revcap_df, "Results/revcap_panel.csv", row.names = FALSE)

cat("\nFerdig!\n")
