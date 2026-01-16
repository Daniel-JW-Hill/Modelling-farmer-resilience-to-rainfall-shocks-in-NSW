
# This script prepares the annual data frames for analysis.
# Including final cleaning of data, new variables, merging in weather data
# Also performs Pesaran Cross Section Dependence tests. 

finalise_annual_dataframes = function() {

# install.packages('plm', type = 'source')
library(plm)

#Load farm business panel data 
load(file.path(root_wd, data_path, "data_fullpanel_20years.Rdata"))
data_20yr = data
rm(data)

#load weather data
load(file.path(root_wd, data_path, "weather_data_monthly.RData"))
weather_data$monthly_rain = ifelse(is.na(weather_data$monthly_rain), weather_data$monthly_rain_mean, weather_data$monthly_rain)
weather_data = weather_data[,-which(names(weather_data) == "monthly_rain_mean")]
weather_data = na.omit(weather_data)

# Strip out obs where no ESI information
# SA2 codes not in NSW as business locations "change"
bn_todrop = unique(data_20yr$bn[which(substr(data_20yr$sa2_code_2021,1,1) != "1")])
data_20yr = data_20yr[-which(data_20yr$bn %in% bn_todrop),]

#Add in ESI index (our chosen weather index)
source(file.path(root_wd, utilities_path, "get_annual_ESI.R"))
data_20yr = generate_annual_ESI(data_20yr, weather_data)

#Add in SPI index 
source(file.path(root_wd, utilities_path, "get_annual_SPI.R"))
data_20yr = generate_annual_SPI(data_20yr, weather_data)

# Drop NA series (last year in series without weather data)
data_20yr = data_20yr[-which(is.na(data_20yr$ESI_annual)),]

# Create key output metrics on a per ha basis. This repairs some existing metrics in the data
# after amendments are made to missing area observations, and to introduce the area data augmentations. 
 
data_20yr$bn = as.character(data_20yr$bn)
data_20yr$gross_returns_perha = data_20yr$gross_returns / data_20yr$area_holdings_derived
data_20yr$EBIT_perha = data_20yr$EBIT / data_20yr$area_holdings_derived
data_20yr$inc_total_perha = data_20yr$inc_total / data_20yr$area_holdings_derived
data_20yr$exp_total_perha = data_20yr$exp_total / data_20yr$area_holdings_derived
data_20yr$profit_net_operating_perha = data_20yr$profit_net_operating / data_20yr$area_holdings_derived
data_20yr$assets_less_stock_perha = data_20yr$assets_less_stock/ data_20yr$area_holdings_derived
data_20yr$liability_total_perha = data_20yr$liability_total / data_20yr$area_holdings_derived

# Save .csv data for remaining analysis (inc. Stata analysis)  
write.csv(data_20yr, file = "P:\\DanielH\\farm performance cointegration study\\Data\\data_20yr_forcointegration.csv")

}

# End of function. 


