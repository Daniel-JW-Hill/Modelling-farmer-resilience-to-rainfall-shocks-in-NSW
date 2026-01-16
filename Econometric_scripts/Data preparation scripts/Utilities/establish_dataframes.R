
# In this step we call the raw dataframes and create a dataframe for analysis
# We also call the imported weather data 
# And perform checks on missing data

establish_dataframes = function() {
  
  # First manually specify the file names required to merge in the BIT folder:
  # Saves the filename structures in the data folder.
  source(file.path(utilities_path, "get_annual_filenames.R"))
  get_BIT_annual_filenames()

  # Run master file preparation script for annual data
  # This script dynamically merges each year of data
  # Subsetting as each is loaded based on the ANZSIC codes.
  source(file.path(utilities_path, "Initial_data_preparation_annual.R"))
  Initial_data_preparation_annual()

  # Now run first cleaning script of the annual data
  source(file.path(utilities_path, "master_file_preparation_finalise.R"))
  master_file_preparation_finalise()

  # Generate monthly dataframe by SA2 region for weather data
  source(file.path(utilities_path, "weather_data_preparation.R"))
  weather_data_preparation()

  # For longer period lags, we need to backcast areas of operations
  # Where the agricultural frame is only reported from 2011
  # The following script performs autocorrelated dickey fuller tests
  # to generate rules of backcasting areas based on observed assets from BIT.
  source(file.path(utilities_path, "data_augmentation_adf_tests.R"))
  data_augmentation_adf_tests()

  # # Now merge in ESI annual,and final cleaning/checks
  source(file.path(utilities_path, "finalise_annual_dataframes.R"))
  finalise_annual_dataframes()
  

}

# End of function