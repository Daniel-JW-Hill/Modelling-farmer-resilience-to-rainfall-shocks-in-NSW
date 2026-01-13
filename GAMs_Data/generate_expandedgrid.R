
# ------------------------------
# Full SPI additive surface reconstruction from Excel (one workbook)
# ------------------------------
rm(list = ls())
library(dplyr)
library(readxl)
setwd("C:\\Users\\hill8\\Documents\\ARC Linkage\\Sep 2025\\GAMs results")

# ------------------------------
# 1. Define workbook path and sheets
# ------------------------------
regions = c("CentralWest",
            "FarWest",
            "MurrayRiverina",
            "NorthernTablelands")
spi_lags = c("L4", "L3", "L2", "L1", "L0", "LEAD")

wb_path = "fullexpansion_SPI.xlsx"

# ------------------------------
# 2. Calc for each region
# ------------------------------

thin_n = 65

for (region in regions){
  pred_vals_long = data.frame()
  #find intercept/parametric val to scale results. 
  coefs = read_excel(paste(region, wb_path, sep  = "_"), sheet = "coefs")
  covars = read_excel(paste(region, wb_path, sep  = "_"), sheet = "covars")
  intercept = coefs$coef[which(coefs$var == "Intercept")]
  coefs = coefs[which(coefs$var != "Intercept" ),]
  parametric_value = intercept + sum(coefs$coef * covars$value)
  
  #get pred values for each lag length. 
  for(lag_name in spi_lags){
    sheet_name  = paste0("pred_SPI_", lag_name)
    pred_vals = read_excel(paste(region, wb_path, sep  = "_"), sheet = sheet_name)
    pred_vals$lag = lag_name
    pred_vals$predictions = pred_vals$predictions - mean(pred_vals$predictions, na.rm = TRUE) #centres results
    pred_vals = slice(pred_vals, c(seq(1, nrow(pred_vals), by = thin_n), nrow(pred_vals))) # thins to avoid explosion in permutations. Adds max back in
    pred_vals_long = rbind(pred_vals_long, pred_vals)
  }
  
  spi_support = unique(pred_vals_long$SPI)
  SPI_expanded_grid = expand.grid(spi_support,
                                    spi_support,
                                    spi_support,
                                    spi_support,
                                    spi_support,
                                    spi_support)
  

  colnames(SPI_expanded_grid) = c("SPIL4", "SPIL3", "SPIL2", "SPIL1", "SPICurrent", "SPILead")

  SPI_expanded_grid$predL4 = SPI_expanded_grid$predL3 = SPI_expanded_grid$predL2 = SPI_expanded_grid$predL1 = SPI_expanded_grid$predCurrent = SPI_expanded_grid$predLead = NA

  
  for (r in 1:length(spi_support)){
    print(r)
    SPI = spi_support[r]
    row_idx = which(SPI_expanded_grid$SPIL4 == SPI)
    SPI_expanded_grid$predL4[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "L4")]
    
    row_idx = which(SPI_expanded_grid$SPIL3 == SPI)
    SPI_expanded_grid$predL3[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "L3")]
    
    row_idx = which(SPI_expanded_grid$SPIL2 == SPI)
    SPI_expanded_grid$predL2[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "L2")]
    
    row_idx = which(SPI_expanded_grid$SPIL1 == SPI)
    SPI_expanded_grid$predL1[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "L1")]
    
    row_idx = which(SPI_expanded_grid$SPICurrent == SPI)
    SPI_expanded_grid$predCurrent[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "L0")]
    
    row_idx = which(SPI_expanded_grid$SPILead == SPI)
    SPI_expanded_grid$predLead[row_idx] = pred_vals_long$predictions[which(pred_vals_long$SPI == SPI & pred_vals_long$lag == "LEAD")]
  }
  
  SPI_expanded_grid$y_smooth = rowSums(SPI_expanded_grid[,c("predL4", "predL3", "predL2", "predL1", "predCurrent", "predLead")])
  SPI_expanded_grid$y_hat = SPI_expanded_grid$y_smooth + parametric_value
  
  #Drop columns no longer needed. 
  SPI_expanded_grid$predLead = NULL
  SPI_expanded_grid$predCurrent = NULL
  SPI_expanded_grid$predL1 = NULL
  SPI_expanded_grid$predL2 = NULL
  SPI_expanded_grid$predL3 = NULL
  SPI_expanded_grid$predL4 = NULL
  
  # Save multiple files to keep manageable. 
  # Each file corresponds to a different current outcome based on lags and leads which we call dynamically in the app. 
  n_files = length(spi_support)
  for (n in 1:n_files){
    subset = subset(SPI_expanded_grid, SPI_expanded_grid$SPICurrent == spi_support[n])
    save(subset, file = paste(region, n, "expandedgrid_SPI.RData", sep = "_"))
  }
  
  rm(SPI_expanded_grid)
}


