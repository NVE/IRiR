
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


# ---- 2. Avgrensninger------------------------------------------
recompute_forutsetninger <- TRUE
years <- c(2025:2026)
panel <- TRUE

# ---- 3. Resultatbeholdere -------------------------------------

results_revcap <- list()
results_stage2 <- list()



# ---- 5. Loop over år ------------------------------------------

for (yr in years) {
  
  cat("\n==============================\n")
  cat("Running year:", yr, "\n")
  cat("==============================\n")
  
  current_year <- yr
  decision <- ifelse(yr == 2026, 0, 1)
  
  # Innleser/beregner forutsetninger
  file_path <- paste0("Data/forutsetninger_", current_year, ".Rdata")
  
  if (recompute_forutsetninger) {
    source("Forutsetninger.R")
  } else {
    load(file_path)
  }
  
  
  
  ############################################################
  # A: MED KILE
  ############################################################
  
  use_kile <- TRUE
  
  cat("Scenario: WITH KILE\n")
  
  source("IRiR.R")
  
  # Henter data fra inntektsrammeberegning
  tmp <- RevCap
  
  tmp$year     <- yr
  tmp$scenario <- "with_kile"
  tmp   <- tmp %>% 
    left_join(ld_EVAL %>% select(ld_eff.s2.cb, id), by="id")
  #tmp$rd_eff   <- ld_EVAL$rd_eff.s2.cb
  
  results_revcap[[paste0(yr, "_",  tmp$scenario)]] <- tmp
  
  # Henter data fra rammevilkårskorrigering
  reg_summary <- summary(ldz.reg$res.regr.NVE)
  
  coef_df_ld <- data.frame(
    variable  = rownames(reg_summary$coefficients),
    estimate  = reg_summary$coefficients[, "Estimate"],
    std_error = reg_summary$coefficients[, "Std. Error"],
    t_value   = reg_summary$coefficients[, "t value"],
    p_value   = reg_summary$coefficients[, "Pr(>|t|)"],
    row.names = NULL
  )
  
  coef_df_rd$year     <- current_year
  coef_df_rd$scenario <- "with_kile"
  
  
  results_reg[[paste0(yr, "_", coef_df_rd$scenario)]] <- coef_df_ld
  
  
  # RESET miljø
  rm(list = setdiff(ls(), c("years", "results_revcap", "results_reg",
                            "kpi_data", "current_year", "decision",
                            "yr")))
  
  ############################################################
  # B: UTEN KILE
  ############################################################
  
  use_kile <- FALSE
  
  cat("Scenario: NO KILE\n")
  
  source("NVE_IRiR.R")
  
  
  # Henter data fra inntektsrammeberegning
  tmp <- RevCap
  
  tmp$year     <- yr
  tmp$scenario <- "no_kile"
  tmp   <- tmp %>% 
    left_join(ld_EVAL %>% select(ld_eff.s2.cb, id), by="id")
  #tmp$rd_eff   <- ld_EVAL$rd_eff.s2.cb
  
  name <- paste0(yr, "_",  tmp$scenario[1])
  results_revcap[[name]] <- tmp
  
  # Henter data fra rammevilkårskorrigering
  reg_summary <- summary(ldz.reg$res.regr.NVE)
  
  coef_df_ld <- data.frame(
    variable  = rownames(reg_summary$coefficients),
    estimate  = reg_summary$coefficients[, "Estimate"],
    std_error = reg_summary$coefficients[, "Std. Error"],
    t_value   = reg_summary$coefficients[, "t value"],
    p_value   = reg_summary$coefficients[, "Pr(>|t|)"],
    row.names = NULL
  )
  
  coef_df_rd$year     <- current_year
  coef_df_rd$scenario <- "no_kile"
  
  
  results_reg[[paste0(yr, "_", coef_df_rd$scenario[1])]] <- coef_df_ld
  
  
  # RESET miljø
  rm(list = setdiff(ls(), c("years", "results_revcap", "results_reg",
                            "kpi_data", "current_year", "decision",
                            "yr")))
  
  
  
}

# ---- 6. Samle resultater --------------------------------------

revcap_df <- do.call(rbind, results_revcap)
reg_df <- do.call(rbind, results_reg)

# ---- 7. Deflater til 2025-priser ------------------------------

base_year <- 2025
kpi_base <- kpi_data$kpi[kpi_data$year == base_year]

revcap_df <- merge(revcap_df, kpi_data, by = "year", all.x = TRUE)

revcap_df$revcap_real <- revcap_df$revcap_nominal * (kpi_base / revcap_df$kpi)

# ---- 8. Lagre -------------------------------------------------

write.csv(revcap_df, "Results/revcap_panel.csv", row.names = FALSE)
write.csv(reg_df, "Results/reg_panel.csv", row.names = FALSE)


