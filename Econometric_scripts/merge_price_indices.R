
#Retrieves PPI and output price indices

merge_price_indices  = function(data, data_path){
  
  PPI = read.csv(file.path(data_path, "PPI_series.csv"))
  
  data$input_price_index = NA
  data$output_price_index = NA
  data$TermsOfTrade_index = NA
  
  unique_years = as.numeric(unique(data$tsid))
  
  for (y in 1:length(unique_years)){
    y_idx = unique_years[y]+2000
    input_index = mean(PPI$PPI[which(PPI$Year == y_idx)]/100, na.rm = TRUE)
    output_index_sheep = mean(PPI$sheep_index[which(PPI$Year == y_idx)])
    output_index_cattle = mean(PPI$cattle_index[which(PPI$Year == y_idx)])
    
    row_idx = which(as.numeric(data$tsid) == (y_idx-2000))
    data$input_price_index = input_index
    
    row_idx_cattle = which(as.numeric(data$tsid) == (y_idx-2000) & data$latest_anzsic %in% c(0142, 0143))
    data$output_price_index[row_idx_cattle] = output_index_cattle
    data$TermsOfTrade_index[row_idx_cattle] = output_index_cattle/input_index
    
    row_idx_sheep = which(as.numeric(data$tsid) == (y_idx-2000) & data$latest_anzsic == 0141)
    data$output_price_index[row_idx_sheep] = output_index_sheep
    data$TermsOfTrade_index[row_idx_sheep] = output_index_sheep/input_index
    
    row_idx_mix = which(as.numeric(data$tsid) == (y_idx-2000) & data$latest_anzsic == 0144)
    data$output_price_index[row_idx_mix] = mean(c(output_index_sheep,output_index_cattle))
    data$TermsOfTrade_index[row_idx_mix] = mean(c(output_index_sheep, output_index_cattle))/ input_index
    
  }
  
  # Drop old index (same as terms of trade)
  data$output_prices = NULL
  data$output_prices_lag = NULL
  
  return(data)
  
}

# End of script