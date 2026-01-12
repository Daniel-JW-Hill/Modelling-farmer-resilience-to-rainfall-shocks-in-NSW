
# Loads data for relevant region

loadData = function(Region_select){
  
  
  
  # Retrieve data files
  file_prefix = switch(
    Region_select,
    "Central_West" = "Central West",
    "Far_West" = "Far West",
    "Murray_Riverina" = "Murray-Riverina",
    "Northern_Tablelands" = "Northern Tablelands",
    stop("No region selected")
  )
  
  # Simulated GAMs data for retrieving the weather index
  gams = read.csv(file.path("GAMs_Data", paste0(file_prefix, "_simulated_data.csv")))
  
  #Coefficients for arellano bond estimator
  exp_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_exp_model_results.xlsx")))
  stock_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_stock_model_results.xlsx")))
  revenue_coefficients = read_xlsx(file.path("Regression_parameters", paste0(file_prefix, "_PGMM_revenue_model_results.xlsx")))
  
  
  
  return(list(gams, 
              exp_coefficients, 
              stock_coefficients,
              revenue_coefficients))
  
}