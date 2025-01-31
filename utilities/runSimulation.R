

# Runs simulation and saves input and output outcomes (as change from mean)

runSimulation = function(fitted_exp_baseline,
                         fitted_stock_baseline,
                         fitted_revenue_baseline,
                         fitted_exp_scenario,
                         fitted_stock_scenario,
                         fitted_revenue_scenario,
                         exp_coefs,
                         stock_coefs, 
                         revenue_coefs){
  
  exp_frame_baseline = initialise_frame(4, 12, c(0, 0, 0, fitted_exp_baseline[1]))
  exp_frame_scenario = initialise_frame(4, 12, c(0, 0, 0, fitted_exp_scenario[1]))
  stock_frame_baseline = initialise_frame(7, 12, c(0, 0, 0, 0, 0, 0, fitted_stock_baseline[1]))
  stock_frame_scenario = initialise_frame(7, 12, c(0, 0, 0, 0, 0, 0, fitted_stock_scenario[1]))
  revenue_frame_baseline = initialise_frame(7, 12, c(0, 0, 0, 0, 0, 0, fitted_revenue_baseline[1]))
  revenue_frame_scenario = initialise_frame(7, 12, c(0, 0, 0, 0, 0, 0, fitted_revenue_scenario[1]))
  
  exp_outcomes_baseline = stock_outcomes_baseline = revenue_outcomes_baseline = rep(0, 10)
  exp_outcomes_scenario = stock_outcomes_scenario = rep(0, 10)
  revenue_outcomes_direct_baseline = revenue_outcomes_exp_baseline = revenue_outcomes_stock_baseline = revenue_outcomes_baseline = rep(0, 10)
  revenue_outcomes_direct_scenario = revenue_outcomes_exp_scenario = revenue_outcomes_stock_scenario = revenue_outcomes_scenario = rep(0, 10)
  
  
  for (y in 1:10) {
    
    #### BASELINE ####
    exp_outcomes_baseline[y] = t(exp_frame_baseline[, y]) %*% as.matrix((exp_coefs))
    stock_outcomes_baseline[y] = t(stock_frame_baseline[, y]) %*% as.matrix((stock_coefs))
    
    #populate input decisions for gross revenue outcomes
    revenue_frame_baseline[3, y] = exp_outcomes_baseline[y]
    revenue_frame_baseline[4, y] = exp_outcomes_baseline[y] ^ 2
    revenue_frame_baseline[5, y] = stock_outcomes_baseline[y]
    revenue_frame_baseline[6, y] = stock_outcomes_baseline[y] ^ 2
    
    revenue_outcomes_direct_baseline[y] = revenue_frame_baseline[7, y] %*% as.matrix((revenue_coefs[7]))
    revenue_outcomes_exp_baseline[y] = revenue_frame_baseline[c(3, 4), y] %*% as.matrix((revenue_coefs[c(3, 4)]))
    revenue_outcomes_stock_baseline[y] = revenue_frame_baseline[c(5, 6), y] %*% as.matrix((revenue_coefs[c(5, 6)]))
    revenue_outcomes_baseline[y] = t(revenue_frame_baseline[, y]) %*% as.matrix((revenue_coefs))
    
    #populate matrices for next periods decisions/outcomes
    exp_frame_baseline[1, y + 1] = revenue_outcomes_baseline[y]
    exp_frame_baseline[2, y + 1] = exp_outcomes_baseline[y]
    exp_frame_baseline[3, y + 1] = stock_outcomes_baseline[y]
    exp_frame_baseline[4, y + 1] = fitted_exp_baseline[y + 1]
    
    stock_frame_baseline[1, y + 1] = revenue_outcomes_baseline[y]
    stock_frame_baseline[2, y + 2] = revenue_outcomes_baseline[y]
    stock_frame_baseline[3, y + 1] = exp_outcomes_baseline[y]
    stock_frame_baseline[4, y + 2] = exp_outcomes_baseline[y]
    stock_frame_baseline[5, y + 1] = stock_outcomes_baseline[y]
    stock_frame_baseline[6, y + 2] = stock_outcomes_baseline[y]
    stock_frame_baseline[7, y + 1] = fitted_stock_baseline[y + 1]
    
    revenue_frame_baseline[1, y + 1] = revenue_outcomes_baseline[y]
    revenue_frame_baseline[2, y + 1] = revenue_outcomes_baseline[y] ^2
    revenue_frame_baseline[7, y + 1] = fitted_revenue_baseline[y + 1]
    
    #### SCENARIO ####
    exp_outcomes_scenario[y] = t(exp_frame_scenario[, y]) %*% as.matrix((exp_coefs))
    stock_outcomes_scenario[y] = t(stock_frame_scenario[, y]) %*% as.matrix((stock_coefs))
    
    #populate input decisions for gross revenue outcomes
    revenue_frame_scenario[3, y] = exp_outcomes_scenario[y]
    revenue_frame_scenario[4, y] = exp_outcomes_scenario[y] ^ 2
    revenue_frame_scenario[5, y] = stock_outcomes_scenario[y]
    revenue_frame_scenario[6, y] = stock_outcomes_scenario[y] ^ 2
    
    # retrieve decomposition and total outcomes
    revenue_outcomes_direct_scenario[y] = revenue_frame_scenario[7, y] %*% as.matrix((revenue_coefs[7]))
    revenue_outcomes_exp_scenario[y] = revenue_frame_scenario[c(3, 4), y] %*% as.matrix((revenue_coefs[c(3, 4)]))
    revenue_outcomes_stock_scenario[y] = revenue_frame_scenario[c(5, 6), y] %*% as.matrix((revenue_coefs[c(5, 6)]))
    revenue_outcomes_scenario[y] = t(revenue_frame_scenario[, y]) %*% as.matrix((revenue_coefs))
    
    #populate matrices for next periods decisions/outcomes
    exp_frame_scenario[1, y + 1] = revenue_outcomes_scenario[y]
    exp_frame_scenario[2, y + 1] = exp_outcomes_scenario[y]
    exp_frame_scenario[3, y + 1] = stock_outcomes_scenario[y]
    exp_frame_scenario[4, y + 1] = fitted_exp_scenario[y + 1]
    
    stock_frame_scenario[1, y + 1] = revenue_outcomes_scenario[y]
    stock_frame_scenario[2, y + 2] = revenue_outcomes_scenario[y]
    stock_frame_scenario[3, y + 1] = exp_outcomes_scenario[y]
    stock_frame_scenario[4, y + 2] = exp_outcomes_scenario[y]
    stock_frame_scenario[5, y + 1] = stock_outcomes_scenario[y]
    stock_frame_scenario[6, y + 2] = stock_outcomes_scenario[y]
    stock_frame_scenario[7, y + 1] = fitted_stock_scenario[y + 1]
    
    revenue_frame_scenario[1, y + 1] = revenue_outcomes_scenario[y]
    revenue_frame_scenario[2, y + 1] = revenue_outcomes_scenario[y] ^2
    revenue_frame_scenario[7, y + 1] = fitted_revenue_scenario[y + 1]
  }
  
  return(list(exp_outcomes_baseline = exp_outcomes_baseline,
              exp_outcomes_scenario = exp_outcomes_scenario, 
              stock_outcomes_baseline = stock_outcomes_baseline, 
              stock_outcomes_scenario = stock_outcomes_scenario, 
              revenue_outcomes_direct_baseline = revenue_outcomes_direct_baseline, 
              revenue_outcomes_direct_scenario = revenue_outcomes_direct_scenario, 
              revenue_outcomes_exp_baseline = revenue_outcomes_exp_baseline, 
              revenue_outcomes_exp_scenario = revenue_outcomes_exp_scenario, 
              revenue_outcomes_stock_baseline = revenue_outcomes_stock_baseline,
              revenue_outcomes_stock_scenario = revenue_outcomes_stock_scenario,
              revenue_outcomes_baseline = revenue_outcomes_baseline, 
              revenue_outcomes_scenario = revenue_outcomes_scenario))
  
}