

# Runs simulation and saves input and output outcomes (as change from mean)

runSimulation = function(yrs,
                         index_baseline,
                         index_scenario,
                         exp_coefs,
                         stock_coefs, 
                         revenue_coefs){
  
  #drop years in rainfall index no longer needed
  index_baseline = na.omit(index_baseline)
  index_scenario = na.omit(index_scenario)
  
  # Save coefs as a vector
  exp_coefs_vec = exp_coefs[,2]
  stock_coefs_vec = stock_coefs[,2]
  revenue_coefs_vec = revenue_coefs[,2]
  
  #Initialise matrices to save results. 
  exp_frame_baseline =  matrix(0, nrow = 7, ncol = yrs+1) # first year is the lag year for revenues so we add an extra year.  
  exp_frame_scenario = matrix(0, nrow = 7, ncol = yrs+1)
  stock_frame_baseline = matrix(0, nrow = 7, ncol = yrs+1)
  stock_frame_scenario = matrix(0, nrow = 7, ncol = yrs+1)
  revenue_frame_baseline = matrix(0, nrow = 7, ncol = yrs+1)
  revenue_frame_scenario = matrix(0, nrow = 7, ncol = yrs+1)
  
  # Pre-fill values into the frames
  stock_frame_baseline[1,1] = stock_frame_scenario[1,1] = 0  
  stock_frame_baseline[2,] = stock_frame_scenario[2] = 0 # constant all years
  stock_frame_baseline[3,1] = stock_frame_scenario[3,1] = 0
  stock_frame_baseline[4,1]   = stock_frame_scenario[4,1]   = 0
  stock_frame_baseline[5,1] = stock_frame_scenario[5,1] = index_baseline[1] # we are using lags of weather for stock here, so the scenario uses the same as the baseline for the first year. 
  stock_frame_baseline[6,]   = stock_frame_scenario[6]   = 0 # constant all years - set to median year of sample. 
  stock_frame_baseline[7,]   = stock_frame_scenario[7]   = 0 # constant all years
  
  exp_frame_baseline[1,1] = exp_frame_scenario[1,1] = 0 
  exp_frame_baseline[2,] = exp_frame_scenario[2,] = 0 # will enter in after stock is calculated. 
  exp_frame_baseline[3,1] = exp_frame_scenario[3,1] = 0
  exp_frame_baseline[4,]   = exp_frame_scenario[4]   = 0# constant all years
  exp_frame_baseline[5,1] = exp_frame_scenario[5,1] = index_baseline[1] # we are using lags of weather for exp here, so the scenario uses the same as the baseline for the first year. 
  exp_frame_baseline[6,]   = exp_frame_scenario[6]   = 0 # constant all years - set to median year of sample. 
  exp_frame_baseline[7,]   = exp_frame_scenario[7]   = 0 # constant all years
  
  revenue_frame_baseline[1,1] = revenue_frame_scenario[1,1] = 0 
  revenue_frame_baseline[2,] = revenue_frame_scenario[2,] = 0 # will enter in after exp is calculated. 
  revenue_frame_baseline[3,] = revenue_frame_scenario[3,] = 0 # will enter in after stock is calculated. 
  revenue_frame_baseline[4,]   = revenue_frame_scenario[4]   = 0 # constant all years
  revenue_frame_baseline[5,1] = index_baseline[1]
  revenue_frame_scenario[5,1] = index_scenario[1] # revenues react to current, not lagged, rainfall (different to inputs) 
  revenue_frame_baseline[6,]   = revenue_frame_scenario[6]   = 0 # constant all years - set to median year of sample. 
  revenue_frame_baseline[7,]   = revenue_frame_scenario[7]   = 0 # constant all years
  
  # initialise vvectors to track outcomes, rather than full model matrices. 
  exp_outcomes_baseline = stock_outcomes_baseline = revenue_outcomes_baseline = rep(0, yrs+1)
  exp_outcomes_scenario = stock_outcomes_scenario = rep(0, yrs+1)
  revenue_outcomes_direct_baseline = revenue_outcomes_exp_baseline = revenue_outcomes_stock_baseline = revenue_outcomes_baseline = rep(0, yrs+1)
  revenue_outcomes_direct_scenario = revenue_outcomes_exp_scenario = revenue_outcomes_stock_scenario = revenue_outcomes_scenario = rep(0, yrs+1)
  
  for (y in 1:yrs) {
    
    #### BASELINE ####
    stock_outcomes_baseline[y] = t(stock_frame_baseline[, y]) %*% as.matrix((stock_coefs_vec))
    exp_frame_baseline[2,y] = stock_outcomes_baseline[y]
    exp_outcomes_baseline[y] = t(exp_frame_baseline[, y]) %*% as.matrix((exp_coefs_vec))
    revenue_frame_baseline[2,y] = exp_outcomes_baseline[y]
    revenue_frame_baseline[3,y] = stock_outcomes_baseline[y]
    
    revenue_outcomes_direct_baseline[y] = revenue_frame_baseline[5, y] %*% as.matrix((revenue_coefs_vec[5,1]))
    revenue_outcomes_exp_baseline[y] = revenue_frame_baseline[2, y] %*% as.matrix((revenue_coefs_vec[2, 1]))
    revenue_outcomes_stock_baseline[y] = revenue_frame_baseline[3, y] %*% as.matrix((revenue_coefs_vec[3,1]))
    revenue_outcomes_baseline[y] = t(revenue_frame_baseline[, y]) %*% as.matrix((revenue_coefs_vec))
    
    #populate matrices for next periods decisions/outcomes
    if (y < yrs) {
      stock_frame_baseline[1, y+1] = stock_outcomes_baseline[y] 
      stock_frame_baseline[3, y+1] = revenue_outcomes_baseline[y]
      stock_frame_baseline[4, y+1] = exp_outcomes_baseline[y]
      stock_frame_baseline[5, y+1] = index_baseline[y] 
      
      exp_frame_baseline[1, y+1] =  exp_outcomes_baseline[y]
      exp_frame_baseline[3, y+1] =  revenue_outcomes_baseline[y]
      exp_frame_baseline[5, y+1] = index_baseline[y]  
      
      revenue_frame_baseline[1,y+1] = revenue_outcomes_baseline[y]
      revenue_frame_baseline[5,y+1] = index_baseline[y+1]
    }
    
    #### SCENARIO ####
    stock_outcomes_scenario[y] = t(stock_frame_scenario[, y]) %*% as.matrix((stock_coefs_vec))
    exp_frame_scenario[2,y] = stock_outcomes_scenario[y]
    exp_outcomes_scenario[y] = t(exp_frame_scenario[, y]) %*% as.matrix((exp_coefs_vec))
    revenue_frame_scenario[2,y] = exp_outcomes_scenario[y]
    revenue_frame_scenario[3,y] = stock_outcomes_scenario[y]
    
    revenue_outcomes_direct_scenario[y] = revenue_frame_scenario[5, y] %*% as.matrix((revenue_coefs_vec[5,1]))
    revenue_outcomes_exp_scenario[y] = revenue_frame_scenario[2, y] %*% as.matrix((revenue_coefs_vec[2, 1]))
    revenue_outcomes_stock_scenario[y] = revenue_frame_scenario[3, y] %*% as.matrix((revenue_coefs_vec[3,1]))
    revenue_outcomes_scenario[y] = t(revenue_frame_scenario[, y]) %*% as.matrix((revenue_coefs_vec))
    
    #populate matrices for next periods decisions/outcomes
    if (y < yrs) {
      stock_frame_scenario[1, y+1] = stock_outcomes_scenario[y] 
      stock_frame_scenario[3, y+1] = revenue_outcomes_scenario[y]
      stock_frame_scenario[4, y+1] = exp_outcomes_scenario[y]
      stock_frame_scenario[5, y+1] = index_scenario[y] 
      
      exp_frame_scenario[1, y+1] =  exp_outcomes_scenario[y]
      exp_frame_scenario[3, y+1] =  revenue_outcomes_scenario[y]
      exp_frame_scenario[5, y+1] = index_scenario[y]  
      
      revenue_frame_scenario[1,y+1] = revenue_outcomes_scenario[y]
      revenue_frame_scenario[5,y+1] = index_scenario[y+1] 
    }
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