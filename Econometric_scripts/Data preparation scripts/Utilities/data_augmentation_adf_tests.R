
# This script seeks to understand the autocorrelation of landholdings between years. 
# For landholdings. 
# And generate rules for jumps in assets less stock to understand
# whether we can backcast landholdings over longer panels. 

data_augmentation_adf_tests = function() {
  
  # install.packages('plm')
  # install.packages('urca')
  library(plm)
  library(urca)
  
  # Load in dataframe
  load(file.path(root_wd, data_path, "ARC_NSW_AllLandUses.Rdata"))
  data = data_master
  rm(data_master)
  rows_to_drop = c()
  
  # Strip data where income/profits are not available. This is an indicator that the business is no longer alive. 
  data = data[!is.na(data$inc_total),]
  
  #Focus on dataset with areas. 
  data = data[which(is.na(data$area_holdings_derived) == FALSE),]
  
  #Strip out all Greater Sydney SA4 regions
  data = data[data$SA4_region != 102,]
  data = data[which(data$SA4_region<116 | data$SA4_region>128),]
  
  #Get pdata frame
  data = pdata.frame(data, index = c("bn", "tsid"))
  
  #Factor variables for regression
  data$latest_anzsic_all = as.factor(data$latest_anzsic_all)
  data$SA3_region = as.factor(data$SA3_region)
  data$sa2_code_2021 = as.factor(data$sa2_code_2021)
  # Retrieve lags and assets less stock values for tests
  data$area_holdings_derived_lag = lag(data$area_holdings_derived)
  data$assets_total_total = rowSums(cbind(data$assets_total, data$stock_closing), na.rm = TRUE)
  data$assets_less_stock = data$assets_total_total - data$stock_closing
  data$assets_less_stock_lag = lag(data$assets_less_stock)
  
  #Drop cases with missing information on assets and areas.  
  data = data[complete.cases(data$area_holdings_derived),]
  data = data[complete.cases(data$assets_less_stock),]
  
  # Subset for properties with full 10 year panel. 
  # Also fill in gaps in data if they exist
  unique_bn = as.vector(unique(data$bn))
  omit = c()
  for (b in 1:length(unique_bn)){
    print(b/length(unique_bn))
    bn_idx = unique_bn[b]
    areas = data$area_holdings_derived[data$bn == bn_idx]
    assets = data$assets_less_stock[data$bn == bn_idx]
    
     if (any(is.na(areas))){
      omit = c(omit, bn_idx)
      next
     }
    
    if (all(areas == 0)){
      omit = c(omit, bn_idx)
      next
    }
    
    if (any(is.na(assets))){
      omit = c(omit, bn_idx)
      next
    }
    
    #Drop if not a full panel in 10 year obs
    if (length(areas)<10){
      omit = c(omit, bn_idx)
      next
    }
    
    #Drop if asset information is not reliable
    if (any(as.vector(assets) == 0)){
      omit = c(omit, bn_idx)
      next
    }
    
    # If gaps or random jumps for single years, interpolate data 
    for (a in 2:(length(areas)-1)){
      if(areas[a] < areas[a-1] & areas[a] < areas[a+1]){
        area_replace = which(rownames(data) == names(areas[a]))
        areas[a] = mean(areas[a-1], areas[a+1])
        data$area_holdings_derived[area_replace] = mean(areas[a-1], areas[a+1])
      }
    }
    
    #repair further missing values
    for (a in 1:(length(areas))){
      if(areas[a] == 0){
        area_replace = which(rownames(data) == names(areas[a]))
        areas[a] = areas[which(areas !=0)[1]]
        data$area_holdings_derived[area_replace] = areas[which(areas !=0)[1]]
      }
    }
    
  }
  
  #Omit those with short panel
  data = data[-which(data$bn %in% omit),]
  unique_bn = as.vector(unique(data$bn))
  
  #Test for stationary series in areas for each observation. 
  area_counter_true = 0
  area_counter_false = 0
  asset_counter_true = 0
  asset_counter_false = 0
  both_true = 0
  
  for (b in 1:length(unique_bn)){
    print(b/length(unique_bn))
    bn_idx = unique_bn[b]
    area_subset = data$area_holdings_derived[data$bn == bn_idx]
    asset_subset = data$assets_less_stock[data$bn == bn_idx]
    
    if (min(area_subset[2:(length(area_subset)-1)]) == max(area_subset[2:(length(area_subset)-1)])){
      area_counter_true = area_counter_true + 1
      area_outcome = TRUE
    } else {
      test_none = urca::ur.df(area_subset, type = "none", selectlags = "AIC")
        
      #Check if gamma is zero - i.e. we want scenarios where we do not reject the null
      #we do not expect/want drift or trend for areas, so do not test for them
        if (abs(test_none@teststat) < abs(test_none@cval[2])){
          area_counter_true = area_counter_true + 1
          area_true = TRUE
        } else {
          area_counter_false = area_counter_false + 1 
          area_true = FALSE
        }
    }
    
    if (min(asset_subset[2:(length(asset_subset)-1)]) == max(asset_subset[2:(length(asset_subset)-1)])){
      asset_counter_true = asset_counter_true + 1
      asset_outcome = TRUE
    } else {
      test_none = urca::ur.df(asset_subset, type = "none", selectlags = "AIC")
      test_drift = urca::ur.df(asset_subset, type = 'drift', selectlags = "AIC")
      test_trend = urca::ur.df(asset_subset, type = "trend", selectlags = "AIC")
      
      #Check if gamma is zero - i.e. we want scenarios where we do not reject the null
      if (abs(test_none@teststat) < abs(test_none@cval[2])){
        asset_counter_true = asset_counter_true + 1
        asset_true = TRUE
      } else if (abs(test_trend@teststat[1]) < abs(test_trend@cval[1,2])){ 
        asset_counter_true = asset_counter_true + 1
        asset_true = TRUE
       } else if (abs(test_drift@teststat[1]) < abs(test_drift@cval[1,2])){ 
          asset_counter_true = asset_counter_true + 1
          asset_true = TRUE
      } else {
        asset_counter_false = asset_counter_false + 1 
        asset_true = FALSE
      }
    }
    
    if (all(asset_true, area_true) == TRUE){
      both_true = both_true + 1
    }
  }
  
  print(paste("percent_stationary = ", round(both_true*100/b, 2), "%"))
  
  # Dickey fuller tests for whether assets and areas share unit roots. 
  # Confirm whether the vast majority of observations pass tests here. 
  
  # Assume those with stationary assets in the full panel 
  # Also are stationary in areas 

  low_entries = data$bn[which(data$area_holdings_derived <10)]
  data = data[-which(data$bn %in% low_entries),]
  save(data, file = file.path(root_wd, data_path, "data_fullpanel_10years.Rdata"))
  
  # Now repeat for full panel, backcast, and save as new dataframe. 
  # Reload the full data and repeat the cleaning steps
  load(file.path(root_wd, data_path, "ARC_NSW_AllLandUses.Rdata"))
  data = data_master
  rm(data_master)
  data = data[!is.na(data$inc_total),]
  data = data[!is.na(data$profit_net_operating),]
  data = data[data$SA4_region != 102,]
  data = data[which(data$SA4_region<116 | data$SA4_region>128),]
  
  # Retrieve lags and assets less stock values for tests
  data$assets_total_total = rowSums(cbind(data$assets_total, data$stock_closing), na.rm = TRUE)
  data$assets_less_stock = data$assets_total_total - data$stock_closing
  data$assets_less_stock_lag = lag(data$assets_less_stock)
  data = data[complete.cases(data$assets_less_stock),]
  
  # Subset for properties with full panel. 
  # Also fill in gaps in data if they exist
  unique_bn = as.vector(unique(data$bn))
  omit = c()
  for (b in 1:length(unique_bn)){
    print(b/length(unique_bn))
    bn_idx = unique_bn[b]
    areas = data$area_holdings_derived[data$bn == bn_idx]
    assets = data$assets_less_stock[data$bn == bn_idx]
    
    if (any(is.na(assets))){
      omit = c(omit, bn_idx)
      next
    }
    
    #Drop if not a full panel in 10 year obs
    if (length(assets)<19){
      omit = c(omit, bn_idx)
      next
    }
    
    
    #Drop if asset information is not reliable
    if (any(as.vector(assets) == 0)){
      omit = c(omit, bn_idx)
      next
    }
    
    areas = na.omit(areas)
    
    #drop if no area information at all
    if (all(areas == 0)){
      omit = c(omit, bn_idx)
      next
    }
    
    for (a in 1:(length(areas))){
      if(areas[a] == 0){
        area_replace = which(rownames(data) == names(areas[a]))
        areas[a] = areas[which(areas !=0)[1]]
        data$area_holdings_derived[area_replace] = areas[which(areas !=0)[1]]
      }
    }
    
    #skip if we do not have full area panel - we will backcast if appropriate
    if (length(areas) < 3) {
      next
    }
    
    # If gaps or random jumps for single years, fill in the data
    for (a in 2:(length(areas)-1)){
      if(areas[a] < areas[a-1] & areas[a] < areas[a+1]){
        area_replace = which(rownames(data) == names(areas[a]))
        data$area_holdings_derived[area_replace] = mean(areas[a-1], areas[a+1])
      }
    }
    
    
  }
  
  data = data[-which(data$bn %in% omit),]
  unique_bn = as.vector(unique(data$bn))
  
  # Now back-cast the areas where assets are stationary
  omit = c()
  for (b in 1:length(unique_bn)){
    print(b/length(unique_bn))
    bn_idx = unique_bn[b]
    
    #We only need it to be stationary until the first area observation. 
    asset_subset = data$assets_less_stock[data$bn == bn_idx & data$tsid < 11]
    area_subset = data$area_holdings_derived[data$bn == bn_idx]
    area_subset = area_subset[!is.na(area_subset)]
    
    if (min(asset_subset[2:(length(asset_subset)-1)]) == max(asset_subset[2:(length(asset_subset)-1)])){ # rare case of non changing assets. 
      data$area_holdings_derived[which(data$bn == bn_idx & is.na(data$area_holdings_derived))] = area_subset[1]
    } else {
      test_none = urca::ur.df(asset_subset, type = "none", selectlags = "AIC")
      test_drift = urca::ur.df(asset_subset, type = "drift", selectlags = "AIC")
      test_trend = urca::ur.df(asset_subset, type = "trend", selectlags = "AIC")
      
      if (abs(test_none@teststat) < abs(test_none@cval[2])){
        data$area_holdings_derived[which(data$bn == bn_idx & is.na(data$area_holdings_derived))] = as.numeric(area_subset[1])
      } else if (abs(test_drift@teststat[1]) > abs(test_drift@cval[1,2])){ 
        data$area_holdings_derived[which(data$bn == bn_idx & is.na(data$area_holdings_derived))] = as.numeric(area_subset[1])
      } else if (abs(test_trend@teststat[1]) > abs(test_trend@cval[1,2])){ 
        data$area_holdings_derived[which(data$bn == bn_idx & is.na(data$area_holdings_derived))] = as.numeric(area_subset[1])
      } else {
        omit = c(omit, bn_idx) #to drop as we cannot data augment areas back
      }
    }
  }
  
  #Omit final obs where we cannot establish stationary relationships for assets less closing stock. 
  data = data[-which(data$bn %in% omit),]
  low_entries = data$bn[which(data$area_holdings_derived <10)]
  data = data[-which(data$bn %in% low_entries),]
  
  save(data, file = file.path(root_wd, data_path, "data_fullpanel_20years.Rdata"))

}

# End of script


