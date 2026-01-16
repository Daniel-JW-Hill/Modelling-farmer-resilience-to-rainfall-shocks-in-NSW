
#Generates standarised precipitation index from rainfall. 

generate_annual_SPI = function(data, weather_data){
  
  years = as.vector(unique(data$tsid))
  years = as.numeric(years)+2000 
  months = 1:12
  sa2_code_2021 = unique(weather_data$sa2_code_2021)
  
  # Now generate annual SPI variables. 
  weather_data$mean_SPI = weather_data$sd_SPI = rep(0, nrow(weather_data))
  for (r in sa2_code_2021){
    for (m in months){
      row_idx = which(weather_data$sa2_code_2021 == r & weather_data$month == m)
      weather_data$mean_SPI[row_idx] = mean(weather_data$monthly_rain[row_idx])
      weather_data$sd_SPI[row_idx] = sd(weather_data$monthly_rain[row_idx])
    }
  }
  weather_data$SPI = (weather_data$monthly_rain - weather_data$mean_SPI)/weather_data$sd_SPI
  
  # Repeat this using the mean and sd from the NSW sample
  weather_data$mean_SPI_NSW = weather_data$sd_SPI_NSW = rep(0, nrow(weather_data))
  for (m in months){
    row_idx = which( weather_data$month == m)
    weather_data$mean_SPI_NSW[row_idx] = mean(weather_data$monthly_rain[row_idx])
    weather_data$sd_SPI_NSW[row_idx] = sd(weather_data$monthly_rain[row_idx])
  }
  weather_data$SPI_NSW = (weather_data$monthly_rain - weather_data$mean_SPI_NSW)/weather_data$sd_SPI_NSW
  
  #Generate SPI annuals (relative to sa2 level)
  SPI_annual = expand.grid(sa2_code_2021, years)
  colnames(SPI_annual) = c("sa2_code_2021", "YEAR")
  SPI_annual$SPI_ANNUAL = NA
  SPI_annual$SPI_ANNUAL_median = NA

  SPI_annual_NSW = NA
  
  for (r in 1:nrow(SPI_annual)){
    region = SPI_annual[r,1]
    yr = SPI_annual[r,2]
    row_idx = which(weather_data$sa2_code_2021 == region & weather_data$year == yr)
    SPI_annual$SPI_ANNUAL[r] = mean(weather_data$SPI[row_idx])
    SPI_annual$SPI_ANNUAL_median[r] = median(weather_data$SPI[row_idx])
    SPI_annual$SPI_ANNUAL_NSW[r] = mean(weather_data$SPI_NSW[row_idx]) 
  }
  
  # Merge into panel dataframe
  data$SPI_annual = rep(0, nrow(data))
  data$SPI_annual_NSW = rep(0, nrow(data))
  data$SPI_annual_median = rep(0, nrow(data))
  data$SPI_annual_SUMMER = rep(0, nrow(data))
  data$SPI_annual_WINTER = rep(0, nrow(data))
  data$SPI_annual_SPRING = rep(0, nrow(data))
  
  data_sa2_regions = unique(data$sa2_code_2021)
  for (r in 1:length(data_sa2_regions)){
    region = as.character(data_sa2_regions[r])
    if (region %in% sa2_code_2021){
      for (yr in years){
        SPI_annual_i = SPI_annual$SPI_ANNUAL[which(SPI_annual$sa2_code_2021 == region & SPI_annual$YEAR == yr)]
        data$SPI_annual[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = SPI_annual_i 
        SPI_annual_i = SPI_annual$SPI_ANNUAL_NSW[which(SPI_annual$sa2_code_2021 == region & SPI_annual$YEAR == yr)]
        data$SPI_annual_NSW[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = SPI_annual_i
        SPI_annual_i = SPI_annual$SPI_ANNUAL_median[which(SPI_annual$sa2_code_2021 == region & SPI_annual$YEAR == yr)]
        data$SPI_annual_NSW[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = SPI_annual_i
      }
    }
  }
  
  return(data)
}

#End of function
