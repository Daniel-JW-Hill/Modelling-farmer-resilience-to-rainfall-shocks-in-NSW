
# Loads data for relevant region

loadData = function(Region_select, nickel_adjustment){
  
  # Retrieve data files
  file_prefix = switch(
    Region_select,
    "Central_West" = "Central_West",
    "Far_West" = "Far_West",
    "Murray_Riverina" = "Murray_Riverina",
    "Northern_Tablelands" = "Northern_Tablelands",
    "Southern_Tablelands" = "Southern_Tablelands",
    stop("No region selected")
  )
  
  opex_gams = read.csv(paste0("GAMs_Data/opex_", file_prefix, "_simulated_data.csv"))
  revenue_gams = read.csv(paste0( "GAMs_Data/revenue_", file_prefix, "_simulated_data.csv"))
  stock_gams = read.csv(paste0("GAMs_Data/stock_", file_prefix, "_simulated_data.csv"))
  
  input_coefficients = read.csv(paste0("Regression_parameters/", file_prefix, "_opex_stock_model_results.csv" ))
  revenue_coefficients = read.csv( paste0( "Regression_parameters/",  file_prefix, "_revenue_model_results.csv" ) )
  
  # subset nickel bias for relevant region
  nickel_adjustment = unlist(nickel_adjustment[which(nickel_adjustment$Name == Region_select),])
  nickel_adjustment = nickel_adjustment[2:6]
  nickel_adjustment = as.numeric(nickel_adjustment)

  # Isolate the relevant coefficients from the input demand functions
  exp_coefficients = input_coefficients[grepl("^eq1_", input_coefficients$term), ]
  exp_coefficients$term = gsub("eq1_", "", exp_coefficients$term)
  exp_coefficients = exp_coefficients[, c("term", "estimate", "std.error")]
  
  exp_coefs = c(
    exp_coefficients$estimate[which(exp_coefficients$term == "gross_returns_perha_log_lag")],
    exp_coefficients$estimate[which(exp_coefficients$term == "exp_total_perha_log_lag")] ,
    exp_coefficients$estimate[which(exp_coefficients$term == "stock_opening_perha_log_lag")],
    exp_coefficients$estimate[which(exp_coefficients$term == "fitted_opex")]
  )
  exp_coefs[2] = exp_coefs[2] * nickel_adjustment[1] # adjusts AR(1) coefficient
  
  exp_sd = c(
    exp_coefficients$std.error[which(exp_coefficients$term == "gross_returns_perha_log_lag")],
    exp_coefficients$std.error[which(exp_coefficients$term == "exp_total_perha_log_lag")] ,
    exp_coefficients$std.error[which(exp_coefficients$term == "stock_opening_perha_log_lag")],
    exp_coefficients$std.error[which(exp_coefficients$term == "fitted_opex")]
  )
  exp_sd[2] = exp_sd[2] * nickel_adjustment[1] # adjusts AR(1) coefficient
  
  stock_coefficients = input_coefficients[grepl("^eq2_", input_coefficients$term), ]
  stock_coefficients$term = gsub("eq2_", "", stock_coefficients$term)
  stock_coefficients = stock_coefficients[, c("term", "estimate", "std.error")]
  
  stock_coefs = c(
    stock_coefficients$estimate[which(stock_coefficients$term == "gross_returns_perha_log_lag")],
    stock_coefficients$estimate[which(stock_coefficients$term == "gross_returns_perha_log_lag_2")],
    stock_coefficients$estimate[which(stock_coefficients$term == "exp_total_perha_log_lag")] ,
    stock_coefficients$estimate[which(stock_coefficients$term == "exp_total_perha_log_lag_2")],
    stock_coefficients$estimate[which(stock_coefficients$term == "stock_opening_perha_log_lag")],
    stock_coefficients$estimate[which(stock_coefficients$term == "stock_opening_perha_log_lag_2")],
    stock_coefficients$estimate[which(stock_coefficients$term == "fitted_stock")]
  )
  stock_coefs[5] = stock_coefs[5] * nickel_adjustment[2] # adjusts AR(1) 
  stock_coefs[6] = stock_coefs[6] * nickel_adjustment[3] # adjusts AR(1) 
  
  stock_sd = c(
    stock_coefficients$std.error[which(stock_coefficients$term == "gross_returns_perha_log_lag")],
    stock_coefficients$std.error[which(stock_coefficients$term == "gross_returns_perha_log_lag_2")],
    stock_coefficients$std.error[which(stock_coefficients$term == "exp_total_perha_log_lag")] ,
    stock_coefficients$std.error[which(stock_coefficients$term == "exp_total_perha_log_lag_2")],
    stock_coefficients$std.error[which(stock_coefficients$term == "stock_opening_perha_log_lag")],
    stock_coefficients$std.error[which(stock_coefficients$term == "stock_opening_perha_log_lag_2")],
    stock_coefficients$std.error[which(stock_coefficients$term == "fitted_stock")]
  )
  stock_sd[5] = stock_sd[5] * nickel_adjustment[2] # adjusts AR(1) 
  stock_sd[6] = stock_sd[6] * nickel_adjustment[3] # adjusts AR(1) 

  #simplify revenue coefficient table
  revenue_coefficients = revenue_coefficients[, c("term", "estimate", "std.error")]
  
  revenue_coefs = c(
    revenue_coefficients$estimate[which(revenue_coefficients$term == "gross_returns_perha_log_lag")],
    revenue_coefficients$estimate[which(revenue_coefficients$term == "I(gross_returns_perha_log_lag^2)")],
    revenue_coefficients$estimate[which(revenue_coefficients$term == "pred_opex_3")] ,
    revenue_coefficients$estimate[which(revenue_coefficients$term == "I(pred_opex_3^2)")],
    revenue_coefficients$estimate[which(revenue_coefficients$term == "pred_stock_3")],
    revenue_coefficients$estimate[which(revenue_coefficients$term == "I(pred_stock_3^2)")],
    revenue_coefficients$estimate[which(revenue_coefficients$term == "fitted_revenue")]
  )
  revenue_coefs[1] = revenue_coefs[1] * nickel_adjustment[4] # adjusts AR(1) 
  revenue_coefs[2] = revenue_coefs[2] * nickel_adjustment[5] # adjusts AR(1) 
  
  revenue_sd = c(
    revenue_coefficients$std.error[which(revenue_coefficients$term == "gross_returns_perha_log_lag")],
    revenue_coefficients$std.error[which(revenue_coefficients$term == "I(gross_returns_perha_log_lag^2)")],
    revenue_coefficients$std.error[which(revenue_coefficients$term == "pred_opex_3")] ,
    revenue_coefficients$std.error[which(revenue_coefficients$term == "I(pred_opex_3^2)")],
    revenue_coefficients$std.error[which(revenue_coefficients$term == "pred_stock_3")],
    revenue_coefficients$std.error[which(revenue_coefficients$term == "I(pred_stock_3^2)")],
    revenue_coefficients$std.error[which(revenue_coefficients$term == "fitted_revenue")]
  )
  revenue_sd[1] = revenue_sd[1] * nickel_adjustment[4] # adjusts AR(1) 
  revenue_sd[2] = revenue_sd[2] * nickel_adjustment[5] # adjusts AR(1) 
  
  return(list(opex_gams, 
              revenue_gams, 
              stock_gams, 
              exp_coefs, 
              exp_sd,
              stock_coefs,
              stock_sd, 
              revenue_coefs, 
              revenue_sd))
  
}