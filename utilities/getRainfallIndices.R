

# Retrieves the estimated GAMs results given the realisation of rainfall. 

getRainfallIndices = function(opex_gams, 
                              stock_gams, 
                              revenue_gams, 
                              inputyear1, 
                              inputyear2,
                              inputyear3,
                              inputyear4,
                              inputyear5,
                              inputyear6,
                              inputyear7,
                              inputyear8,
                              inputyear9,
                              inputyear10){
  
  weather_options = sort(unique(opex_gams$SPI_index_L1)) # simulated data options
  weather_index_baseline = rep(weather_options[7], 15) # 'typical' conditions - as close as zero as possible for all
  weather_index_scenario = weather_index_baseline
  weather_choices = c(
    'Very dry (-1)',-0.8,-0.6,-0.45,-0.3,-0.15,
    'Typical rainfall (0)',
    0.2, 0.35, 0.5, 0.7, 0.85, 1.0, 1.2,
    'Very wet (1.35)'
  ) # what user can select
  
  weather_index_scenario[5] = weather_options[which(weather_choices == inputyear1)]
  weather_index_scenario[6] = weather_options[which(weather_choices == inputyear2)]
  weather_index_scenario[7] = weather_options[which(weather_choices == inputyear3)]
  weather_index_scenario[8] = weather_options[which(weather_choices == inputyear4)]
  weather_index_scenario[9] = weather_options[which(weather_choices == inputyear5)]
  weather_index_scenario[10] = weather_options[which(weather_choices == inputyear6)]
  weather_index_scenario[11] = weather_options[which(weather_choices == inputyear7)]
  weather_index_scenario[12] = weather_options[which(weather_choices == inputyear8)]
  weather_index_scenario[13] = weather_options[which(weather_choices == inputyear9)]
  weather_index_scenario[14] = weather_options[which(weather_choices == inputyear10)]
  
  fitted_exp_baseline = fitted_stock_baseline  = fitted_revenue_baseline = rep(0, 10)
  fitted_exp_scenario = fitted_stock_scenario  = fitted_revenue_scenario  = rep(0, 10)
  
  for (y in 1:10) {
    # reverses vectors as data is saved where first column is current year, subsequent columns lags
    inputs_weather_baseline = rev(weather_index_baseline[y:(y + 3)]) 
    revenue_weather_baseline = rev(weather_index_baseline[(y + 1):(y + 4)])
    
    input_row = which(apply(opex_gams[, 2:5], 1, function(row) all(row == inputs_weather_baseline)))
    fitted_exp_baseline[y] = opex_gams$predicted[input_row]
    fitted_stock_baseline[y] = stock_gams$predicted[input_row]
    
    revenue_row = which(apply(revenue_gams[, 2:5], 1, function(row) all(row == revenue_weather_baseline)))
    fitted_revenue_baseline[y] = revenue_gams$predicted[revenue_row]
    
    inputs_weather_scenario = rev(weather_index_scenario[y:(y + 3)])
    revenue_weather_scenario = rev(weather_index_scenario[(y + 1):(y + 4)])
    
    input_row = which(apply(opex_gams[, 2:5], 1, function(row) all(row == inputs_weather_scenario)))
    fitted_exp_scenario[y] = opex_gams$predicted[input_row]
    fitted_stock_scenario[y] = stock_gams$predicted[input_row]
    
    revenue_row = which(apply(revenue_gams[, 2:5], 1, function(row) all(row == revenue_weather_scenario)))
    fitted_revenue_scenario[y] = revenue_gams$predicted[revenue_row]
  }
  
  return(list(weather_index_baseline, 
              weather_index_scenario, 
              fitted_exp_baseline,
              fitted_stock_baseline,
              fitted_revenue_baseline,
              fitted_exp_scenario,
              fitted_stock_scenario,
              fitted_revenue_scenario))
  
  
}
