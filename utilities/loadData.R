
# Loads data for relevant region

loadData = function(Region_select){
  
  
  

  
  
  regions = c("CentralWest",
              "FarWest",
              "MurrayRiverina",
              "NorthernTablelands")
  
  
  # Simulated GAMs data for retrieving the weather index
  # retrieve the right file path name to call dynamically depending on need. 
  file_prefix = switch(
    Region_select,
    "Central West" = "CentralWest",
    "Far West" = "arWest",
    "Murray-Riverina" = "MurrayRiverina",
    "Northern Tablelands" = "NorthernTablelands",
    stop("No region selected")
  )
  gams_path = file_prefix
  
  #Coefficients for arellano bond estimator

  file_prefix = switch(
    Region_select,
    "Central West" = "Central_West",
    "Far West" = "Far_West",
    "Murray-Riverina" = "Murray_Riverina",
    "Northern Tablelands" = "Northern_Tablelands",
    stop("No region selected")
  )
  
  exp_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_exp_model_results.xlsx")))
  stock_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_stock_model_results.xlsx")))
  revenue_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_revenue_model_results.xlsx")))
  
  
  return(list(gams_path, 
              exp_coefficients, 
              stock_coefficients,
              revenue_coefficients))
  
}