
# creates first differenced data with lags

get_thirdstage_data = function(data){
  
  colnames(data)[which(colnames(data) == "pred ALL_SPI")] = "weather_index"
  
  # lag weather index
  data = pdata.frame(data, index = c("bn", "tsid"))
  data$weather_index_lag = lag(data$weather_index, k = 1)

  # Unfactor everything into numeric except the bn which we retain as a string. 
  data_PGMM = data.frame(bn = as.character(data$bn),
                         year = as.numeric(data$year),
                         revenue = as.numeric(data$revenue),
                         revenue_lag = as.numeric(data$revenue_lag),
                         exp = as.numeric(data$exp),
                         exp_lag = as.numeric(data$exp_lag),
                         stock = as.numeric(data$stock),
                         stock_lag = as.numeric(data$stock_lag),
                         assets = as.numeric(data$assets),
                         assets_lag = as.numeric(data$assets_lag),
                         termsoftrade_index = as.numeric(data$termsoftrade_index),
                         weather_index = as.numeric(data$weather_index),
                         weather_index_lag = as.numeric(data$weather_index_lag),
                         NTablelands_trend = as.numeric(data$NTablelands_trend),
                         CWest_trend = as.numeric(data$CWest_trend),
                         FarWest_trend = as.numeric(data$FarWest_trend),
                         Murray_trend = as.numeric(data$Murray_trend))
                         
                         
  return(data_PGMM)
}
