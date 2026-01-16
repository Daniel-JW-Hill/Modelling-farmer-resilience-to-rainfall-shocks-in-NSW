
#Reorganises data for GAMs models

get_GAMs_data <- function(data) {
  
  data = as.data.frame(data)
  data_GAMs = as.data.frame(cbind(data$bn, 
                                  data$tsid, 
                                  data$year,
                                  data$analysis_regions, 
                                  data$SA3_region,
                                  data$sa2_code_2021,
                                  
                                  data$quantity_index,
                                  data$gross_returns_perha_log, 
                                  data$gross_returns_perha_log_lag,
                                  data$EBIT_perha_log,
                                  data$EBIT_perha_log_lag,
                                  
                                  data$exp_total_perha_log, 
                                  data$exp_total_perha_log_lag,
                                  
                                  data$assets_less_stock_perha_log, 
                                  data$assets_less_stock_perha_log_lag,
                                  
                                  data$stock_opening_perha_log, 
                                  data$stock_opening_perha_log_lag, 
                                  
                                  data$input_price_index,
                                  data$output_price_index,
                                  data$TermsOfTrade_index,
                                  
                                  data$SPI_annual))
  
  colnames(data_GAMs) = c("bn", 
                          "tsid", 
                          "year", 
                          "analysis_regions", 
                          "SA3_region",
                          "SA2_region",
                          
                          "quantity_index",
                          "revenue", 
                          "revenue_lag",
                          "EBIT",
                          "EBIT_lag",
                          
                          "exp",
                          "exp_lag", 
                          "assets",
                          "assets_lag", 
                          "stock",
                          "stock_lag", 
                          
                          "input_price_index",
                          "output_price_index",
                          "termsoftrade_index",
                          
                          "SPI_index")

  data_GAMs$year = as.numeric(data_GAMs$year)
  data_GAMs$quantity_index = as.numeric(data_GAMs$revenue)
  data_GAMs$revenue = as.numeric(data_GAMs$revenue)
  data_GAMs$revenue_lag = as.numeric(data_GAMs$revenue_lag)
  data_GAMs$EBIT = as.numeric(data_GAMs$EBIT)
  data_GAMs$EBIT_lag = as.numeric(data_GAMs$EBIT_lag)
  
  data_GAMs$exp = as.numeric(data_GAMs$exp)
  data_GAMs$exp_lag = as.numeric(data_GAMs$exp_lag)
  data_GAMs$assets = as.numeric(data_GAMs$assets)
  data_GAMs$assets_lag = as.numeric(data_GAMs$assets_lag)
  data_GAMs$stock = as.numeric(data_GAMs$stock)
  data_GAMs$stock_lag = as.numeric(data_GAMs$stock_lag)
  data_GAMs$input_price_index = as.numeric(data_GAMs$input_price_index)
  data_GAMs$output_price_index = as.numeric(data_GAMs$output_price_index)
  data_GAMs$termsoftrade_index = as.numeric(data_GAMs$termsoftrade_index)
  data_GAMs$SPI_index = as.numeric(data_GAMs$SPI_index)
  
  # Demeaned versions of data
  # Also save mean value of outputs to add back into fitted values. 
  library(fixest)
  setFixest_notes(FALSE)
  data_demeaned = fixest::demean(data_GAMs[,c("revenue", 
                                              "exp", 
                                              "stock", 
                                              "assets" ,
                                              "termsoftrade_index")], 
                                 f = data_GAMs[,"bn"], 
                                 na.rm = FALSE)
  
  colnames(data_demeaned) = c("demeaned_revenue", 
                              "demeaned_exp", 
                              "demeaned_stock", 
                              "demeaned_assets" ,
                              "demeaned_termsoftrade_index")
  data_GAMs = cbind(data_GAMs, data_demeaned)
  data_GAMs$mean_revenue = data_GAMs$revenue - data_GAMs$demeaned_revenue  

  # Lag the demeaned data
  data_GAMs = pdata.frame(data_GAMs, index = c("bn", "tsid"))
  data_GAMs$demeaned_revenue_lag = lag(data_GAMs$demeaned_revenue,1)
  data_GAMs$demeaned_exp_lag = lag(data_GAMs$demeaned_exp,1)
  data_GAMs$demeaned_assets_lag = lag(data_GAMs$demeaned_assets,1)
  data_GAMs$demeaned_stock_lag = lag(data_GAMs$demeaned_stock,1)
  
  # SPI Indices
  data_GAMs$SPI_index_Lag1 = lag(data_GAMs$SPI_index)
  data_GAMs$SPI_index_Lag2 = lag(data_GAMs$SPI_index, 2)
  data_GAMs$SPI_index_Lag3 = lag(data_GAMs$SPI_index, 3)
  data_GAMs$SPI_index_Lag4 = lag(data_GAMs$SPI_index, 4)
  data_GAMs$SPI_index_Lead1 = lead(data_GAMs$SPI_index, 1)
  
  data_GAMs$CWest = ifelse(data_GAMs$analysis_regions == "Central_West",1,0)
  data_GAMs$FarWest = ifelse(data_GAMs$analysis_regions == "Far_West",1,0)
  data_GAMs$NTablelands = ifelse(data_GAMs$analysis_regions == "Northern_Tablelands",1,0)
  data_GAMs$Murray = ifelse(data_GAMs$analysis_regions == "Murray_Riverina",1,0)
  
  data_GAMs$CWest_trend = data_GAMs$CWest * data_GAMs$year
  data_GAMs$FarWest_trend =  data_GAMs$FarWest * data_GAMs$year
  data_GAMs$NTablelands_trend = data_GAMs$NTablelands * data_GAMs$year
  data_GAMs$Murray_trend = data_GAMs$Murray * data_GAMs$year
  
  return(data_GAMs)
  
}