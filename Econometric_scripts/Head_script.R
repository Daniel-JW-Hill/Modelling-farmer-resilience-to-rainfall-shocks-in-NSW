
# ARC linkage paper
# Heterogeoenity in farm performance in response to climatic shocks
# Daniel Hill, Daniel Gregg, Oscar Cacho, and Jeff Connor
# September 2025
 
rm(list = ls())

# Establish working directories
root_wd = #
data_path = #
utilities_path = "utilities"
results_path = "results"

setwd(file.path(root_wd))

# Read in final analysis data
# This dataframe derives from the BIT dataframe. 
load(file.path(data_path, "final_analysis_data.Rdata"))

# Confirm region definitions
source(file.path(utilities_path, "define_regions.R"))
data = define_regions(data)
unique_regions = c("Northern_Tablelands", "Central_West", "Far_West", "Murray_Riverina")

# Add in price indices
source(file.path(utilities_path, "merge_price_indices.R"))
data = merge_price_indices(data, data_path)

# Construct quantity index and EBIT measures. 
data$quantity_index = data$gross_returns_perha_log/data$output_price_index
data$EBIT_perha_log = data$EBIT_perha_log # already defined. 

##### Initial Analysis ####
# Here we perform some simple checks, including:
#  - summary stat tables
#  - Panel unit root tests of expenditure and stock (for AR 'input demand' models)

# Get summary stats by region and NSW
source(file.path(utilities_path, "getsummarystats.R"))
getsummarystats(data, unique_regions)

# Run unit root tests for each region and NSW
library(plm)
source(file.path(utilities_path, "run_unit_root_tests.R"))

unit_root_df = data.frame()
for (r in 1:length(unique_regions)){
  ridx = unique_regions[r]
  data_subset = data.frame(subset(data,data$analysis_regions == ridx))
  data_subset$bn = as.character(data_subset$bn)
  unit_root = run_unit_root_tests(data_subset, "exp_total_perha_log", paste(ridx,'opex', sep = "_"))
  unit_root_df = rbind(unit_root_df, unit_root)
  unit_root =  run_unit_root_tests(data_subset, "stock_opening_perha_log", paste(ridx,'stock', sep = "_"))
  unit_root = unit_root_df = rbind(unit_root_df, unit_root)
  unit_root = run_unit_root_tests(data_subset, "gross_returns_perha_log", paste(ridx,'revenue', sep = "_"))
  unit_root_df = rbind(unit_root_df, unit_root)
  unit_root =  run_unit_root_tests(data_subset, "assets_less_stock_perha_log", paste(ridx,'assets', sep = "_"))
  unit_root = unit_root_df = rbind(unit_root_df, unit_root)
}
unit_root = run_unit_root_tests(data, "exp_total_perha_log", paste("NSW",'opex', sep = "_"))
unit_root = unit_root_df = rbind(unit_root_df, unit_root)
unit_root = run_unit_root_tests(data, "stock_opening_perha_log", paste("NSW",'stock', sep = "_"))
unit_root = unit_root_df = rbind(unit_root_df, unit_root)
unit_root = run_unit_root_tests(data_subset, "gross_returns_perha_log", paste("NSW",'revenue', sep = "_"))
unit_root_df = rbind(unit_root_df, unit_root)
unit_root =  run_unit_root_tests(data_subset, "assets_less_stock_perha_log", paste("NSW",'assets', sep = "_"))
unit_root = unit_root_df = rbind(unit_root_df, unit_root)

write.csv(unit_root_df, file = file.path(root_wd, results_path, "unit_root_tests", "unit_root_tests.csv"))

#### GAMs models ###
# In this section we estimate the GAMs models
library(mgcv) #gam models
library(flextable) #saving GAMs results
library(officer) #for flextable landscape orientation
library(ggplot2)

source(file.path(utilities_path, "get_GAMs_data.R"))
data_GAMs = get_GAMs_data(data)
data_GAMs = subset(data_GAMs, data_GAMs$analysis_regions %in% unique_regions)

source(file.path(utilities_path, "estimate_GAMs_models.R"))
source(file.path(utilities_path, "save_GAMs_results.R"))
source(file.path(utilities_path, "simple_gams_lines.R"))
source(file.path(utilities_path, "white_noise_test.R"))
data_list = estimate_GAMs_models(data_GAMs)
data_GAMs = data_list[[1]]
data_subset_list = data_list[[2]]
rm(data_list)

#### Second stage models with weather index from GAMs models ####
library(pdynmc)
library(plm)
library(fixest)
source(file.path(utilities_path, "get_thirdstage_data.R"))
source(file.path(utilities_path, "define_pgmm_equations.R"))
source(file.path(utilities_path, "run_PGMM.R"))
pgmm_eq_list = define_pgmm_equations()

# Run region specific models as SUR. 
for (r in 1:length(unique_regions)){
  ridx = unique_regions[r]
  print(ridx)
  
  # Get relevant data for the region and first difference relevant variables. 
  data_subset = data_subset_list[[ridx]]
  data_subset = get_thirdstage_data(data_subset)
  
  # run model for region
  run_PGMM(pgmm_eq_list, data_subset, ridx)
}

# Run for NSW as well
data_GAMs = get_thirdstage_data(data_GAMs)
run_PGMM(pgmm_eq_list, data_GAMs, "NSW")

#### END OF SCRIPT ####

