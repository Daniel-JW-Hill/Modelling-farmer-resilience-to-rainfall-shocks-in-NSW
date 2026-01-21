

# Retrieves the estimated GAMs results given the realisation of rainfall. 

getRainfallIndices = function(yrs, 
                              gams_path, 
                              spi_t4, 
                              spi_t3,
                              spi_t2,
                              spi_t1,
                              spi_current,
                              spi_lead){
  
  
  #Load single version of GAMs data to get unique values 
  load(file.path("GAMs_Data", paste("pred_SPI", gams_path, ".RData", sep = "_")))
  rainfall_data = pred_vals_long
  rm(pred_vals_long)
  rainfall_options = sort(unique(rainfall_data$SPI))
  constant = rainfall_data$constant[1]

  rainfall_index_baseline_idx = which.min(abs(rainfall_options)) # 'typical' conditions - as close as zero as possible for all years
  rainfall_index_baseline = rainfall_options[rainfall_index_baseline_idx]
  rainfall_index_scenario = rep(rainfall_index_baseline, yrs+5) 
  
  # First 4 entries equal to zero as normal conditions before first drought
  rainfall_index_scenario[5] = rainfall_options[which.min(abs(rainfall_options - spi_t4))]
  rainfall_index_scenario[6] = rainfall_options[which.min(abs(rainfall_options - spi_t3))]
  rainfall_index_scenario[7] = rainfall_options[which.min(abs(rainfall_options - spi_t2))]
  rainfall_index_scenario[8] = rainfall_options[which.min(abs(rainfall_options - spi_t1))]
  rainfall_index_scenario[9] = rainfall_options[which.min(abs(rainfall_options - spi_current))]
  rainfall_index_scenario[10] = rainfall_options[which.min(abs(rainfall_options - spi_lead))]
 # remaining years assumed 'normal' conditions as we are modelling resilience for 'current' year. 
  
  index_baseline = index_scenario = rep(NA, yrs + 5) # lag + lead years added in. NA years will be dropped anyway. 
  index_normal = sum(c(rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "L4")],
                       rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "L3")],
                       rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "L2")],
                       rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "L1")],
                       rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "L0")],
                       rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_baseline & rainfall_data$lag == "LEAD")],
                       constant))
  
  for (y in 1:(yrs)) { 
    index_baseline[y+4] = index_normal
    index_scenario[y+4] = sum(c(rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y] & rainfall_data$lag == "L4")],
                                rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y+1] & rainfall_data$lag == "L3")],
                                rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y+2] & rainfall_data$lag == "L2")],
                                rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y+3] & rainfall_data$lag == "L1")],
                                rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y+4] & rainfall_data$lag == "L0")],
                                rainfall_data$predictions[which(rainfall_data$SPI == rainfall_index_scenario[y+5] & rainfall_data$lag == "LEAD")],
                                constant))
}
  
  return(list(rainfall_index_baseline, 
              rainfall_index_scenario, 
              index_baseline,
              index_scenario))
  
  
}
