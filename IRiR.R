#### IRiR - InntektsRammer i R / Revenue cap calculation in R ####

#### R set up ####
  remove(list=ls()) # Remove all objects from memory
  start.time =  Sys.time()
  options(scipen = 2000) # Avoid showing large numbers in scientific mode

# Install/load packages
  if (!"Benchmarking" %in% installed.packages()) install.packages("Benchmarking")
  if (!"plyr" %in% installed.packages()) install.packages("plyr")
  if (!"dplyr" %in% installed.packages()) install.packages("dplyr")
  if (!"openxlsx" %in% installed.packages()) install.packages("openxlsx")
  if (!"rstudioapi" %in% installed.packages()) install.packages("rstudioapi")
  if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
  if (!"writexl" %in% installed.packages()) install.packages("writexl")
  if (!"readxl" %in% installed.packages()) install.packages("readxl")
  library(tidyverse)
  library(Benchmarking)
  library(dplyr)
  library(openxlsx)
  library(rstudioapi)
  library(writexl)
  library(readxl)
  
  # Automatically setting working directory to where the data file is located. 
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd() # Get current working directory
  results_dir = file.path(getwd(), "Results") # Define directory where results are written
  timestamp = format(start.time, "%Y-%m-%d_%H-%M-%S") 
  run_dir = file.path(results_dir, paste0("Run_", timestamp)) # Creates directory for given run within results
  dir.create(run_dir, recursive = TRUE) # Actual creation of run_dir
  
# Bootstrap settings
# Dummy variable determining whether to calculate new bootstrap estimates (1) or reuse last calculation (0)
  BS.new = 0
  BS.ite = 100000 # Number of iterations in bootstrap calculation
  Statnett_calc = FALSE #


# Sett inn riktig beregningstype til lagring i DWH
  versjon <- data.frame("type" = as.integer(2))
  # 0 Annet
  # 1 Forel?pig beregning
  # 2 Varsel
  # 3 Vedtak
  # 4 Etter klagebehandling (forel?pig) ( ved varsel om IR )
  # 5 Etter klagebehandling (endelig)   ( ved vedtak om IR )

# Beregning av forutsetninger
  #source("./Forutsetninger.R") 

#### Calculating revenue caps ####
  source("./R-script/functions_nve.R")               # File containing functions created for/by NVE
  source("./R-script/0_1_Config_Assumptions_Data.R") # Defining parameters and importing base data
  source("./R-script/0_2_Merging_Z-variables.R")     # Merging Z-variables
  write.dat = dat[,c("orgn", "y", "comp", "ld_OPEXxS", "ld_sal", "ld_sal.cap", "ld_pens", "ld_pens.eq", "ld_impl", "ld_391", "ld_elhub", "ld_usla", "rd_OPEXxS",
                     "rd_sal", "rd_sal.cap", "rd_pens", "rd_pens.eq", "rd_impl", "rd_391", "rd_elhub", "rd_cga", "rd_cga_tidl", "rd_coord", "rd_usla", "t_OPEXxS", "t_sal", "t_sal.cap",
                     "t_pens", "t_pens.eq", "t_impl", "t_391", "t_elhub", "ld_bv.sf", "ld_dep.sf", "ld_bv.gf", "ld_dep.gf", "rd_bv.sf", "rd_dep.sf", "rd_bv.gf",
                     "rd_dep.gf", "t_bv.sf", "t_dep.sf", "ld_cens", "rd_cens", "t_cens", "ld_nl", "rd_nl", "ld_sub", "ld_hvoh", "ld_hvug", "ld_hvsc", "ld_hv",
                     "ld_ss", "rd_wv.ol", "rd_wv.uc", "rd_wv.sc", "rd_wv.ss","ldz_salt", "ldz_coast_wind", "ldz_water", "ldz_incline", "ldz_prod",
                     "ldz_snow_trees", "ldz_forest_broadleaf", "ldz_snowdrift", "ldz_snow_400", "ldz_wind_99", "ldz_frosthours", "ldz_forest_mixed_conf",
                     "ldz_mgc", "ap.t_2", "pnl.rc")]

  csv_file = file.path(run_dir, paste0(Sys.Date(), "_grunnlagsdata.csv"))
  xlsx_file = file.path(run_dir, paste0(Sys.Date(), "_grunnlagsdata.xlsx"))
  write.csv(write.dat, file = csv_file)
  write.xlsx(write.dat, file = xlsx_file, overwrite = T)
        
  source("./R-script/0_3_Calculated_Input_Values.R")    # Calculating input values for DEA
  source("./R-script/0_4_Company_Selection.R")          # Preparing for special treatment
  source("./R-script/1_0_DEA.R")                        # Stage 1 - DEA
  source("./R-script/2_0_GeoCorrection.R")       # Stage 2 - Z factor adjustment using OLS
  source("./R-script/3_0_Calibration.R")         # Calibration on RAB including gf investments
  source("./R-script/Spec_OOTO-model.R")                # Companies exempted from DEA - Special models
  source("./R-script/Spec_AvEff-model.R")               # Companies exempted from DEA - Special models
  source("./R-script/4_0_Revenue_Cap_Calculation.R")    # Calculating Revenue caps

  end.time =  Sys.time()
  calc.time = end.time - start.time
  calc.time
  source("./R-script/Key_figures.R")      # Script creating data frame for printing results
  source("./R-script/Print_results.R")    # Script for printing results

  write.csv(RevCap, file = "RevCap.csv")
  # Kathrine var her