

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
  load(file.path("GAMs_Data", paste(gams_path, 1, 'expandedgrid_SPI.RData', sep = "_")))
  rainfall_data_dummy = subset
  rm(subset)
  rainfall_options = sort(unique(rainfall_data_dummy$SPIL4))
  rm(rainfall_data_dummy)

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
  
  load(file.path("GAMs_Data", paste(gams_path, rainfall_index_baseline_idx, 'expandedgrid_SPI.RData', sep = "_")))
  rainfall_data_normal = subset
  rm(subset)
  index_normal = rainfall_data_normal$y_hat[rainfall_data_normal$SPIL4 == rainfall_index_baseline & 
                                            rainfall_data_normal$SPIL3 == rainfall_index_baseline & 
                                            rainfall_data_normal$SPIL2 ==  rainfall_index_baseline & 
                                            rainfall_data_normal$SPIL1 == rainfall_index_baseline & 
                                            rainfall_data_normal$SPICurrent == rainfall_index_baseline & 
                                            rainfall_data_normal$SPILead == rainfall_index_baseline]
  rm(rainfall_data_normal)
  
  for (y in 1:(yrs)) { 
    index_baseline[y+4] = index_normal
    
    rainfall_index_scenario_idx  = which(rainfall_options == rainfall_index_scenario[y+4]) # what is the current year's rainfall realisation
    load(file.path("GAMs_Data", paste(gams_path, rainfall_index_scenario_idx, 'expandedgrid_SPI.RData', sep = "_"))) # load data
    rainfall_data = subset
    rm(subset)
    
    index_scenario[y+4] = rainfall_data$y_hat[rainfall_data$SPIL4 == rainfall_index_scenario[y] & 
                                              rainfall_data$SPIL3 == rainfall_index_scenario[y+1] & 
                                              rainfall_data$SPIL2 ==  rainfall_index_scenario[y+2] & 
                                              rainfall_data$SPIL1 == rainfall_index_scenario[y+3] & 
                                              rainfall_data$SPICurrent == rainfall_index_scenario[y+4] & 
                                              rainfall_data$SPILead == rainfall_index_baseline] # finds corresponding rainfall index given historical rainfall. Ignore lead var for expectations. 
}
  
  return(list(rainfall_index_baseline, 
              rainfall_index_scenario, 
              index_baseline,
              index_scenario))
  
  
}
