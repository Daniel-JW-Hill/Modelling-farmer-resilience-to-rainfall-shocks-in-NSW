
# This script reads in the merged annual master file for further subsetting and cleaning

master_file_preparation_finalise = function() {
  
# Load in master compiled annual data
load(file.path(root_wd, data_path, "data_master.Rdata"))
  
# Limit to NSW farms only - no farms with cross border operations. 
data_master = data_master[data_master$state_operation == "100000000",]

# Some farms will be located over multiple locations
# For these, combine if in the same SA2, otherwise drop (too hard to attribute outcomes)
bn_tsid = paste(data_master$bn, data_master$tsid, sep = "_")
n_duplicates = data.frame(table(bn_tsid))
bn_duplicates = n_duplicates[which(n_duplicates[,2] > 1),]
bn_duplicates = unique(substr(bn_duplicates$bn_tsid,1,10))

rows_to_drop = c() #drops duplicates
bn_to_drop = c() #drops bn's with multiple locations. 

for (i in 1:length(bn_duplicates)){
  bn_idx = bn_duplicates[i]
  subset_data = subset(data_master, data_master$bn == bn_idx)
  year_counts = data.frame(table(subset_data$tsid))
  dup_years = which(year_counts$Freq>1)
  dup_years = as.vector(year_counts$Var1[dup_years])
  for (y in 1:length(dup_years)){
    year = dup_years[y]
    subset_year_data = subset(subset_data, subset_data$tsid == year)
    if (all(subset_year_data$sa2_code_2021 == subset_year_data$sa2_code_2021[1])) { # all properties face the same weather variable
      rows = which(data_master$bn == bn_idx & data_master$tsid == year)
      data_master$area_holdings_derived[rows] =  sum(subset_year_data$area_holdings_derived, na.rm = TRUE)
      data_master$EVAO_derived[rows] =  sum(subset_year_data$EVAO_derived, na.rm = TRUE) # note BIT data consistent across all duplicates
      rows = rows[2:length(rows)]
      rows_to_drop = c(rows_to_drop, rows)
    } else {
      bn_to_drop = c(bn_to_drop, bn_idx) # if split across locations we drop - allocation of stock etc endogenous
    }
  }
}

data_master = data_master[-rows_to_drop,] #drops duplicates
data_master = data_master[-which(data_master$bn %in% bn_to_drop),] #drops bn's with multiple locations that cannot be aggregated

# Statistical area regions derive from the Agricultural frame, which is only for the last 10 years. 
# We cast back these values where the bn exists in previous years, recognizing this is an imperfect measure. 
bn_unique = unique(data_master$bn[data_master$tsid<=11])
bn_regions = unique(data_master$bn[!is.na(data_master$sa2_code_2021)])
bn_unique = bn_unique[bn_unique %in% bn_regions]
bn_unique = na.omit(bn_unique)

for (i in 1:length(bn_unique)){
  bn_idx = bn_unique[i]
  bn_region = unique(data_master$sa2_code_2021[data_master$bn == bn_idx & !is.na(data_master$sa2_code_2021)])
  if (length(bn_region) == 0){ # where area observations do not exist
      next
  } else {
      bn_region = bn_region[1] # unique function retains order of sorted dataframe so pick the earlier region. 
  }
  data_master$sa2_code_2021[which(data_master$bn == bn_idx & is.na(data_master$sa2_code_2021))] = bn_region
}

# Add in SA3 and SA4 regions based on the SA2 region. 
data_master$SA3_region = as.numeric(substr(as.character(data_master$sa2_code_2021), 1,5))
data_master$SA4_region = as.numeric(substr(as.character(data_master$SA3_region), 1,3))

# Force latest_anzsic to be latest_anzsic_all (error in data construction but keep both for scripts already written)
data_master$latest_anzsic = data_master$latest_anzsic_all

# Add in gross return and other related variables. 
buildup_trading_stock = rowSums(cbind(data_master$stock_closing, - data_master$stock_opening), na.rm = TRUE)
data_master$gross_returns = rowSums(cbind(data_master$inc_total , buildup_trading_stock), na.rm = TRUE)
data_master$EBIT = rowSums(cbind(data_master$gross_returns, -data_master$exp_total), na.rm = TRUE)

# Save dataframe. 
save(data_master, file = file.path(root_wd, data_path, "ARC_NSW_AllLandUses.Rdata"))

}

# End of function
