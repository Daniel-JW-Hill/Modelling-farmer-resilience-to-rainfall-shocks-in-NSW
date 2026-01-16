 
# This script reads in and prepares the weather data for matching at a business level

weather_data_preparation = function(){

  # install.packages('gtools', type = 'source')
  library(gtools)
  
  load(file.path(root_wd, data_path, "ARC_NSW_AllLandUses.Rdata"))
  
  #Load in first data to initialise dataframe of sa2 codes
  weather_var = # read in weather data file here. 
  vars = c("sa2_code_2021",
            "year",
            "month",
            "daily_rain",
            "daily_rain_max",
            "daily_rain_min",
            "et_morton_actual",
            "et_morton_potential",
            "et_morton_wet",
            "et_short_crop",
            "et_tall_crop",
            "evap_morton_lake",
            "evap_pan",
            "evap_syn",
            "max_temp",
            "min_temp",
            "monthly_rain_max",
            "monthly_rain_min",
            "monthly_rain_mean",
            "mslp",
            "radiation",
            "rh_tmax",
            "rh_tmin",
            "vp",
            "vp_deficit")
  
  years = 2011:2020
  months = 1:12
  sa2_code_2021 = unique(weather_var$SA2_CODE21)
  weather_data = expand.grid(sa2_code_2021 = sa2_code_2021, year = years, month = months)
  for (v in vars){
    weather_data$newvar = NA
    names(weather_data)[names(weather_data) == "newvar"] = v
  }
  
  #Weather data path and files
  directory_path = file.path(root_wd, data_path, "weather_data")
  file_names = list.files(directory_path)
  setwd(directory_path)
  
  for (f in 1:length(file_names)){
    file = file_names[f]
    weather_var = read.csv(file)
    
    #Strip information from file name
    file_string = sub("zonal_stats_", "", file)
    yr = as.numeric(substr(file_string, 1,4))
    file_string = sub(paste(yr, "_", sep = ""), "", file_string)
    month = substr(file_string, 1,2)
    file_string = sub(paste(month, "_", sep = ""), "", file_string)
    var_name = sub(".csv", "", file_string)
    month = as.numeric(month)
    
    #Now add information to dataframe. 
    c = which(names(weather_data) == var_name)
    for (i in 1:nrow(weather_var)){
      sa2_code_idx = weather_var[i,1]
      r = which(weather_data$sa2_code_2021 == sa2_code_idx & weather_data$year == yr & weather_data$month == month)
      weather_data[r,c] = weather_var[i,2]
    }
  }
  
  #Save weather dataframe
  save(weather_data, file = file.path(root_wd, data_path, "weather_data_monthly_from2011.RData"))
  
  # Now add in weather data for 2001-2010
  weather_data_older = # read in older weather data here. 
  names(weather_data_older)[which(names(weather_data_older) == "sa2")] = "sa2_code_2021"
  names(weather_data_older)[which(names(weather_data_older) == "yr")] = "year"
  names(weather_data_older)[which(names(weather_data_older) == "mo")] = "month"
  
  #Drop missing info from longer weather panel. 
  cols_to_drop = c("monthly_rain_max", "monthly_rain_min", "daily_rain_max", "daily_rain_min")
  weather_data = weather_data[,!names(weather_data) %in% cols_to_drop]
  names(weather_data)[which(names(weather_data_older) == "monthly_rain_mean")] = "monthly_rain"
  
  #Drop repeated colnames 
  weather_data = weather_data[,-c(4:6)]
  
  weather_data = gtools::smartbind(weather_data, weather_data_older)
  save(weather_data, file = file.path(root_wd, data_path, "weather_data_monthly.RData"))
  setwd(file.path(root_wd))
}

# End of script



