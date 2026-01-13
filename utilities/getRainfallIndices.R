

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
  rainfall_data_dummy = load(file.path("GAMs_Data", paste(gams_path, 1, 'simulated_data.RData', sep = "_")))
  #sort lag 4 for unique
  #drop dummy
  #get baseline :)
  #then we can dynamically load the data based on which current period we have, find and drop. 
  
  rainfall_options = sort(unique(gams$SPI)) # simulated data options
  rainfall_index_baseline = min(abs(rainfall_options)) # 'typical' conditions - as close as zero as possible for all years
  rainfall_index_scenario = rep(rainfall_index_baseline, yrs+5) 
  
  # First 4 entries equal to zero as normal conditions before first drought
  rainfall_index_scenario[5] = rainfall_options[which.min(abs(rainfall_options - spi_t4))]
  rainfall_index_scenario[6] = rainfall_options[which.min(abs(rainfall_options - spi_t3))]
  rainfall_index_scenario[7] = rainfall_options[which.min(abs(rainfall_options - spi_t2))]
  rainfall_index_scenario[8] = rainfall_options[which.min(abs(rainfall_options - spi_t1))]
  rainfall_index_scenario[9] = rainfall_options[which.min(abs(rainfall_options - spi_current))]
  rainfall_index_scenario[10] = rainfall_options[which.min(abs(rainfall_options - spi_lead))]
 # remaining years assumed 'normal' conditions as we are modelling resilience for 'current' year. 
  
  index_baseline = index_scenario = rep(0, yrs + 6) # lag + lead years added in. 
  index_normal = gams$y_hat[gams$SPIL4 == rainfall_index_baseline & 
                              gams$SPIL3 == rainfall_index_baseline & 
                              gams$SPIL2 ==  rainfall_index_baseline & 
                              gams$SPIL1 == rainfall_index_baseline & 
                              gams$SPICurrent == rainfall_index_baseline & 
                              gams$SPILead == rainfall_index_baseline]
  for (y in 1:(yrs+5)) { 
    index_baseline[y] = index_normal
    index_scenario = gams$y_hat[gams$SPIL4 == rainfall_index_scenario[y] & 
                                gams$SPIL3 == rainfall_index_scenario[y+1] & 
                                gams$SPIL2 ==  rainfall_index_scenario[y+2] & 
                                gams$SPIL1 == rainfall_index_scenario[y+3] & 
                                gams$SPICurrent == rainfall_index_scenario[y+4] & 
                                gams$SPILead == rainfall_index_scenario[y+5]]
  }
  
  return(list(rainfall_index_baseline, 
              rainfall_index_scenario, 
              index_baseline,
              index_scenario))
  
  
}
