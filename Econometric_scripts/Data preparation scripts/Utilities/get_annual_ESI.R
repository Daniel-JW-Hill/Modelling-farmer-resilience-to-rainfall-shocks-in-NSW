
# This function adds in the annual ESI data into the farm business data frame

generate_annual_ESI = function(data, weather_data){

  years = as.vector(unique(data$tsid))
  years = as.numeric(years)+2000
  months = 1:12
  sa2_code_2021 = unique(weather_data$sa2_code_2021)
  
  # Now generate annual ESI variables.  
  weather_data$ESI_ratio = weather_data$et_morton_actual/weather_data$et_morton_potential
  weather_data$mean_ESI_ratio = weather_data$sd_ESI_ratio = rep(0, nrow(weather_data))
  
  for (r in sa2_code_2021){
    for (m in months){
      row_idx = which(weather_data$sa2_code_2021 == r & weather_data$month == m)
      weather_data$mean_ESI_ratio[row_idx] = mean(weather_data$ESI_ratio[row_idx])
      weather_data$sd_ESI_ratio[row_idx] = sd(weather_data$ESI_ratio[row_idx])
    }
  }
  
  weather_data$ESI = (weather_data$ESI_ratio - weather_data$mean_ESI_ratio)/weather_data$sd_ESI_ratio
  
  # Repeat this using the mean and sd from the NSW sample
  weather_data$mean_ESI_NSW_ratio = weather_data$sd_ESI_NSW_ratio = rep(0, nrow(weather_data))
  for (m in months){
    row_idx = which( weather_data$month == m)
    weather_data$mean_ESI_NSW_ratio[row_idx] = mean(weather_data$ESI_ratio[row_idx])
    weather_data$sd_ESI_NSW_ratio[row_idx] = sd(weather_data$ESI_ratio[row_idx])
  }
  
  weather_data$ESI_NSW = (weather_data$ESI_ratio - weather_data$mean_ESI_NSW_ratio)/weather_data$sd_ESI_NSW_ratio
  
  #Generate ESI annuals (relative to sa2 level)
  ESI_annual = expand.grid(sa2_code_2021, years)
  colnames(ESI_annual) = c("sa2_code_2021", "YEAR")
  ESI_annual$ESI_ANNUAL = NA
  ESI_annual$ESI_ANNUAL_median = NA
  ESI_annual$ESI_SUMMER = NA
  ESI_annual$ESI_WINTER = NA
  ESI_annual$ESI_SPRING = NA
  ESI_annual_NSW = NA
  
  for (r in 1:nrow(ESI_annual)){
    region = ESI_annual[r,1]
    yr = ESI_annual[r,2]
    row_idx = which(weather_data$sa2_code_2021 == region & weather_data$year == yr)
    ESI_annual$ESI_ANNUAL[r] = mean(weather_data$ESI[row_idx])
    ESI_annual$ESI_ANNUAL_median[r] = median(weather_data$ESI[row_idx])
    ESI_annual$ESI_ANNUAL_NSW[r] = mean(weather_data$ESI_NSW[row_idx]) 
    row_idx = which(weather_data$sa2_code_2021 == region &  weather_data$year == yr & weather_data$month %in% c(12,1,2))
    ESI_annual$ESI_SUMMER[r] = mean(weather_data$ESI[row_idx])
    row_idx = which(weather_data$sa2_code_2021 == region &  weather_data$year == yr & weather_data$month %in% c(3,4,5,6))
    ESI_annual$ESI_WINTER[r] = mean(weather_data$ESI[row_idx])
    row_idx = which(weather_data$sa2_code_2021 == region & weather_data$year == yr &  weather_data$month %in% c(7,8,9,10,11))
    ESI_annual$ESI_SPRING[r] = mean(weather_data$ESI[row_idx]) 
  }
  
  # Merge into panel dataframe
  data$ESI_annual = rep(0, nrow(data))
  data$ESI_annual_NSW = rep(0, nrow(data))
  data$ESI_annual_median = rep(0, nrow(data))
  data$ESI_annual_SUMMER = rep(0, nrow(data))
  data$ESI_annual_WINTER = rep(0, nrow(data))
  data$ESI_annual_SPRING = rep(0, nrow(data))
  
  data_sa2_regions = unique(data$sa2_code_2021)
  for (r in 1:length(data_sa2_regions)){
    region = as.character(data_sa2_regions[r])
    if (region %in% sa2_code_2021){
      for (yr in years){
        ESI_annual_i = ESI_annual$ESI_ANNUAL[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = ESI_annual_i 
        ESI_annual_i = ESI_annual$ESI_ANNUAL_NSW[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual_NSW[which(data$sa2_code_2021 == region & data$tsid == yr-2000)] = ESI_annual_i
        ESI_annual_i = ESI_annual$ESI_ANNUAL_median[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual_median[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = ESI_annual_i
        ESI_annual_i = ESI_annual$ESI_SUMMER[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual_SUMMER[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = ESI_annual_i
        ESI_annual_i = ESI_annual$ESI_WINTER[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual_WINTER[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = ESI_annual_i
        ESI_annual_i = ESI_annual$ESI_SPRING[which(ESI_annual$sa2_code_2021 == region & ESI_annual$YEAR == yr)]
        data$ESI_annual_SPRING[which(data$sa2_code_2021 == region & data$tsid == yr-2000)]  = ESI_annual_i
      }
    }
  }
  
  return(data)
}

#End of function



