
# ARC linkage paper
# Heterogeoenity in farm performance in respnse to climatic shocks

# This script prepares the dataframe for analysis
# from the raw abs data
# note this script takes some time to complete.
# but after it is run once, data frames are available for the analysis head script. 

rm(list = ls())

# Establish working directories
root_wd = # set root wd here
data_path =  # set data path here
# in this path make sure to have both the PPI and weather series available. 
utilities_path = # set path to all utility scripts. 

setwd(file.path(root_wd))

###########################################
#### STEP 1 - establish RAW dataframes ####
###########################################

# In this step we call the raw dataframes and create a dataframe for analysis
# We also call the imported weather data 
# And perform checks on missing data

source(file.path(utilities_path, "establish_dataframes.R"))
establish_dataframes()

################################################
#### STEP 2 - establish analysis dataframes ####
################################################

# In this step we take a raw dataframe and prepare it further for analysis
source(file.path(utilities_path, "establish_analysis_dataframes.R"))
data_NSW = establish_analysis_dataframe()

# analysis data is also saved in data folder for analysis. 

# End of script. 