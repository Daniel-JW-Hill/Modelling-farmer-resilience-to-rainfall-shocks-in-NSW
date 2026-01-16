

establish_analysis_dataframe = function() {
  
  #Load in dataframe - 20 year annual dataframe.  
  data_annual_20yr = read.csv(file.path(root_wd, data_path, "data_20yr_forcointegration.csv"))
  data_annual_20yr = data_annual_20yr[,-which(names(data_annual_20yr) == 'X')]
  
  # All time series are nominal. Deflate by livestock PPI (inputs and outputs).
  source(file.path(root_wd, utilities_path, "deflate_series.R"))
  data_annual_20yr = deflate_series(data_annual_20yr, 
                                    freq = "annual", 
                                    vars = c("profit_net_operating_perha", 
                                             "gross_returns_perha",
                                             "EBIT_perha",  
                                             "exp_total_perha",
                                             "stock_opening", 
                                             "stock_closing", 
                                             "assets_less_stock_perha", 
                                             "liability_total_perha"))
  
  # Subset for livestock only businesses based on livestock only ANZSIC codes
  # And omit any businesses with missing observations  
  source(file.path(utilities_path, "subset_for_livestock.R"))
  data_annual_20yr = subset_for_livestock(data_annual_20yr, save_name ="data_20yr_forcointegration_livestock.csv")
  
  # Logarithmic transformation of key variables through inverse IHS function
  source(file.path(utilities_path, "log_transform_variables.R"))
  data_annual_20yr = log_transform_variables(data_annual_20yr, freq ="annual")
  
  # Structure as panel data. 
  library(plm)
  data_annual_20yr = pdata.frame(data_annual_20yr, index = c('bn', 'tsid'))
  
  # Generate lagged variables 
  source(file.path(utilities_path, "get_lagged_fdiff_variables.R"))
  data_annual_20yr = get_lagged_variables(data_annual_20yr, freq ="annual")
  
  #Remove obs with NA opex values
  bn_omit = data_annual_20yr$bn[which(is.na(data_annual_20yr$exp_total_perha_log))]
  bn_omit = as.vector(bn_omit)
  data_annual_20yr = data_annual_20yr[-which(data_annual_20yr$bn %in% bn_omit),]
  
  
  #Remove obs where assets do not change nominally (returns NA results in mg estimations)
  #and remove any where time series is less than 3
  unique_bn = unique(data_annual_20yr$bn)
  bn_omit = c()
  for (i in 1:length(unique_bn)){
    data_subset = subset(data_annual_20yr, data_annual_20yr$bn == as.character(unique_bn[i]))
    if (length(unique(data_subset$assets_total))<=3){
      bn_omit = c(bn_omit, as.character(unique_bn[i]))
    } 
  }
  data_annual_20yr = data_annual_20yr[-which(data_annual_20yr$bn %in% bn_omit),]
  

  # Rename data
  data = data_annual_20yr
  rm(data_annual_20yr)
  rm(data_subset)
  
  # NSW-Level climate variables
  data$year = as.numeric(as.character(data$tsid))
  data$ESI_average_NSW =  data$SPI_average_NSW = NA
  unique_yr = unique(data$year)
  for (t in unique_yr){
    row_idx = which(as.vector(data$year) == t)
    data_subset_ESI = data$ESI_annual[row_idx]
    data_subset_SPI = data$SPI_annual[row_idx]
    if (is.na(mean(data_subset_ESI, na.rm = TRUE)) == FALSE){
      data$ESI_average_NSW[row_idx] = mean(data_subset_ESI, na.rm = TRUE)
    }
    if (is.na(mean(data_subset_SPI, na.rm = TRUE)) == FALSE){
      data$SPI_average_NSW[row_idx] = mean(data_subset_SPI, na.rm = TRUE)
    }
  }
  
  # Get output prices as control variables
  PPI = read.csv(file.path(root_wd, data_path, "PPI_series.csv"))
  
  unique_year = unique(data$year)
  data$output_prices = NA
  
  for (y in 1:length(unique_year)){
    y_idx = unique_year[y]
    price_index_sheep = mean(PPI$sheep_index[which(PPI$Year == y_idx+2000)])
    price_index_cattle = mean(PPI$cattle_index[which(PPI$Year == y_idx+2000)])
    price_index_PPI = mean(PPI$PPI[which(PPI$Year == y_idx+2000)])
    row_idx_cattle = which(data$year == y_idx & data$latest_anzsic %in% c(0142, 0143))
    data$output_prices[row_idx_cattle] = price_index_cattle / price_index_PPI  
    row_idx_sheep= which(data$year == y_idx & data$latest_anzsic == 0141)
    data$output_prices[row_idx_sheep] = price_index_sheep/ price_index_PPI  
    row_idx_mix= which(data$year == y_idx & data$latest_anzsic == 0144)
    data$output_prices[row_idx_mix] = mean(c(price_index_sheep,price_index_cattle))/ price_index_PPI  
  }
  
  data$output_prices_lag = lag(data$output_prices)
  
  # Establish regions:
  # Functionally SA4 regions, but aggregate those with smaller samples
  data$analysis_regions = NA
  data$analysis_regions[which(data$SA4_region == 103)] = "Central_West" # Central West
  data$analysis_regions[which(data$SA4_region == 105)] = "Far_West" # Far west
  data$analysis_regions[which(data$SA4_region == 109)] = "Murray_Riverina" # Murray region
  data$analysis_regions[which(data$SA4_region == 110)] = "Northern_Tablelands" # New England
  data$analysis_regions[which(data$SA4_region == 113)] = "Murray_Riverina" # Riverina
 
  data$analysis_regions[which(data$SA4_region == 112)] = "North_coast" # Richmond Tweed
  data$analysis_regions[which(data$SA4_region == 104)] = "North_coast" # Coffs-Grafton
  data$analysis_regions[which(data$SA4_region == 108)] = "Mid_North_coast" # Mid north coast

  data$analysis_regions[which(data$SA3_region == 11401)] = "Coastal" # Shoalhaven
  data$analysis_regions[which(data$SA3_region == 10104)] = "Coastal" # South Coast
  data$analysis_regions[which(data$SA4_region == 111)] = "Coastal"  # Newcastle and Lake Macquarie 
  data$analysis_regions[which(data$SA4_region == 107)] = "Coastal" # Illawarra
  data$analysis_regions[which(data$SA4_region == 115)] = "Coastal" # Hawksebury
  data$analysis_regions[which(data$SA3_region == 10601)] = "Coastal" # Lower Hunter
  data$analysis_regions[which(data$SA3_region == 10602)] = "Coastal" # Maitland
  data$analysis_regions[which(data$SA3_region == 10603)] = "Coastal" #Port Stephens
 
  data$analysis_regions[which(data$SA3_region == 10103)] = "Southern_Tablelands" # Snowy mountains
  data$analysis_regions[which(data$SA3_region == 10102)] = "Southern_Tablelands" # Queanbeyan
  data$analysis_regions[which(data$SA3_region == 11402)] = "Southern_Tablelands" # Southern highlands
  data$analysis_regions[which(data$SA3_region == 10106)] = "Southern_Tablelands" # Young-Yass
  data$analysis_regions[which(data$SA3_region == 10105)] = "Southern_Tablelands" # Goulburn
  data$analysis_regions[which(data$SA3_region == 10301)] = "Southern_Tablelands" # Bathhurst
  data$analysis_regions[which(data$SA3_region == 10604)] = "Southern_Tablelands" # Upper Hunter
  
  # return to dataframe to unfactor bn
  data$bn = as.character(data$bn)
  
  # Final check of time series lengths
  # We do this at a region level just in case regions change between bn units
  # these are likely problematic bn obs where abn shifts properties
  unique_region = unique(data$analysis_regions)
  bn_to_omit = c()
  for (i in 1:length(unique_region)){
    ridx = unique_region[i]
    data_subset = subset(data, data$analysis_regions == ridx)
    panel_lengths = table(data_subset$bn)
    bn_to_omit = c(bn_to_omit, as.character(names(which(panel_lengths<=4))))
  }
  data = data[-which(data$bn %in% bn_to_omit),]
  
  # remove outlier in expenditure. 
  far_west_opex = data$exp_total_perha[which(data$analysis_regions == "Far_West")]
  bn_to_omit = data$bn[which(data$analysis_regions == "Far_West" & data$exp_total_perha>=max(far_west_opex))]
  data = data[-which(data$bn %in% bn_to_omit),]
  
  save(data, file = file.path(root_wd, data_path, "final_analysis_data.Rdata"))
  
  return(data)
  
}

# End of function



