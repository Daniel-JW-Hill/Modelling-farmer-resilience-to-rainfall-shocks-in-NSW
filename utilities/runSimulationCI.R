
#runs simulations for confidence intervals (bootstrapping)

runSimulationCI = function(exp_coefs, 
                           stock_coefs,
                           revenue_coefs,
                           exp_sd,
                           stock_sd, 
                           revenue_sd,
                           fitted_exp_baseline,
                           fitted_stock_baseline,
                           fitted_revenue_baseline,
                           fitted_exp_scenario,
                           fitted_stock_scenario,
                           fitted_revenue_scenario,
                           Region_select, 
                           levels_grossReturns, 
                           levels_exp, 
                           levels_stock,
                           min_b,
                           max_b){
  
  bb = 500 # number of draws.
  exp_outcomes_scenario_diff_ls = exp_outcomes_scenario_ls = list()
  stock_outcomes_scenario_diff_ls = stock_outcomes_scenario_ls = list()
  revenue_outcomes_direct_scenario_diff_ls = revenue_outcomes_direct_scenario_ls = list()
  revenue_outcomes_exp_scenario_diff_ls = revenue_outcomes_exp_scenario_ls = list()
  revenue_outcomes_stock_scenario_diff_ls = revenue_outcomes_stock_scenario_ls = list()
  revenue_outcomes_scenario_diff_ls = revenue_outcomes_scenario_ls = list()
  EBITDA_outcomes_scenario_diff_ls  = list()
  
  exp_coefs_b = exp_coefs
  stock_coefs_b = stock_coefs
  revenue_coefs_b = revenue_coefs
  
  for (b in 1:bb) {
    
    #redraw coefficients
    exp_coefs_b = mapply(rnorm,
                         n = 1,
                         mean = exp_coefs,
                         sd = exp_sd)
    stock_coefs_b = mapply(rnorm,
                           n = 1,
                           mean = stock_coefs,
                           sd = stock_sd)
    revenue_coefs_b = mapply(rnorm,
                             n = 1,
                             mean = revenue_coefs,
                             sd = revenue_sd)
    
    results = runSimulation(fitted_exp_baseline,
                            fitted_stock_baseline,
                            fitted_revenue_baseline,
                            fitted_exp_scenario,
                            fitted_stock_scenario,
                            fitted_revenue_scenario,
                            exp_coefs_b,
                            stock_coefs_b, 
                            revenue_coefs_b)
    
    exp_outcomes_baseline = results$exp_outcomes_baseline
    exp_outcomes_scenario =  results$exp_outcomes_scenario 
    stock_outcomes_baseline =  results$stock_outcomes_baseline
    stock_outcomes_scenario =  results$stock_outcomes_scenario
    revenue_outcomes_direct_baseline =  results$revenue_outcomes_direct_baseline 
    revenue_outcomes_direct_scenario =  results$revenue_outcomes_direct_scenario
    revenue_outcomes_exp_baseline = results$revenue_outcomes_exp_baseline
    revenue_outcomes_exp_scenario = results$revenue_outcomes_exp_scenario
    revenue_outcomes_stock_baseline = results$revenue_outcomes_stock_baseline
    revenue_outcomes_stock_scenario = results$revenue_outcomes_stock_scenario
    revenue_outcomes_baseline =  results$revenue_outcomes_baseline
    revenue_outcomes_scenario =  results$revenue_outcomes_scenario
    
    
    # Add levels back into estimates
    
    exp_outcomes_baseline = exp_outcomes_baseline + log(levels_exp$levels[levels_exp$regions == Region_select])
    stock_outcomes_baseline = stock_outcomes_baseline + log(levels_stock$levels[levels_stock$regions == Region_select])
    revenue_outcomes_direct_baseline = revenue_outcomes_direct_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_exp_baseline = revenue_outcomes_exp_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_stock_baseline = revenue_outcomes_stock_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_baseline = revenue_outcomes_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    
    exp_outcomes_scenario = exp_outcomes_scenario + log(levels_exp$levels[levels_exp$regions == Region_select])
    stock_outcomes_scenario = stock_outcomes_scenario + log(levels_stock$levels[levels_stock$regions == Region_select])
    revenue_outcomes_direct_scenario = revenue_outcomes_direct_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_exp_scenario = revenue_outcomes_exp_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_stock_scenario = revenue_outcomes_stock_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
    revenue_outcomes_scenario = revenue_outcomes_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
   
    exp_outcomes_baseline = exp(exp_outcomes_baseline)
    stock_outcomes_baseline = exp(stock_outcomes_baseline)
    revenue_outcomes_direct_baseline = exp(revenue_outcomes_direct_baseline)
    revenue_outcomes_exp_baseline = exp(revenue_outcomes_exp_baseline)
    revenue_outcomes_stock_baseline = exp(revenue_outcomes_stock_baseline)
    revenue_outcomes_baseline = exp(revenue_outcomes_baseline)
    
    exp_outcomes_scenario = exp(exp_outcomes_scenario)
    stock_outcomes_scenario = exp(stock_outcomes_scenario)
    revenue_outcomes_direct_scenario = exp(revenue_outcomes_direct_scenario)
    revenue_outcomes_exp_scenario = exp(revenue_outcomes_exp_scenario)
    revenue_outcomes_stock_scenario = exp(revenue_outcomes_stock_scenario)
    revenue_outcomes_scenario = exp(revenue_outcomes_scenario)
    
    # Find differences and relative differences, then plot.
    exp_outcomes_scenario_diff_ls[[b]] =  exp_outcomes_scenario - exp_outcomes_baseline
    stock_outcomes_scenario_diff_ls[[b]]  =  stock_outcomes_scenario - stock_outcomes_baseline
    revenue_outcomes_direct_scenario_diff_ls[[b]] = revenue_outcomes_direct_scenario - revenue_outcomes_direct_baseline
    revenue_outcomes_exp_scenario_diff_ls[[b]] = revenue_outcomes_exp_scenario - revenue_outcomes_exp_baseline
    revenue_outcomes_stock_scenario_diff_ls[[b]] = revenue_outcomes_stock_scenario - revenue_outcomes_stock_baseline
    revenue_outcomes_scenario_diff_ls[[b]]  =  revenue_outcomes_scenario - revenue_outcomes_baseline
    
    EBITDA_baseline = revenue_outcomes_baseline - exp_outcomes_baseline
    EBITDA_scenario = revenue_outcomes_scenario - exp_outcomes_scenario
    EBITDA_outcomes_scenario_diff_ls[[b]] =  EBITDA_scenario - EBITDA_baseline
    
    exp_outcomes_scenario_ls[[b]] = (exp_outcomes_scenario_diff_ls[[b]] / exp_outcomes_baseline) * 100 + 100
    stock_outcomes_scenario_ls[[b]] = (stock_outcomes_scenario_diff_ls[[b]] / stock_outcomes_baseline) * 100 + 100
    revenue_outcomes_direct_scenario_ls[[b]] = (revenue_outcomes_direct_scenario_diff_ls[[b]] / revenue_outcomes_direct_baseline) * 100 + 100
    revenue_outcomes_exp_scenario_ls[[b]] = (revenue_outcomes_exp_scenario_diff_ls[[b]] / revenue_outcomes_exp_baseline) * 100 + 100
    revenue_outcomes_stock_scenario_ls[[b]] = ( revenue_outcomes_stock_scenario_diff_ls[[b]] / revenue_outcomes_stock_baseline) * 100 + 100
    revenue_outcomes_scenario_ls[[b]] = (revenue_outcomes_scenario_diff_ls[[b]] / revenue_outcomes_baseline) * 100 + 100
  }
  

  # Find the min and max for geom ribbon
  exp_outcomes_scenario_ls = do.call(rbind, exp_outcomes_scenario_ls)
  stock_outcomes_scenario_ls = do.call(rbind, stock_outcomes_scenario_ls)
  revenue_outcomes_direct_scenario_ls = do.call(rbind, revenue_outcomes_direct_scenario_ls)
  revenue_outcomes_exp_scenario_ls = do.call(rbind, revenue_outcomes_exp_scenario_ls)
  revenue_outcomes_stock_scenario_ls = do.call(rbind, revenue_outcomes_stock_scenario_ls)
  revenue_outcomes_scenario_ls = do.call(rbind, revenue_outcomes_scenario_ls)
  
  exp_outcomes_scenario_diff_ls = do.call(rbind, exp_outcomes_scenario_diff_ls)
  stock_outcomes_scenario_diff_ls = do.call(rbind, stock_outcomes_scenario_diff_ls)
  revenue_outcomes_direct_scenario_diff_ls = do.call(rbind, revenue_outcomes_direct_scenario_diff_ls)
  revenue_outcomes_exp_scenario_diff_ls = do.call(rbind, revenue_outcomes_exp_scenario_diff_ls)
  revenue_outcomes_stock_scenario_diff_ls = do.call(rbind, revenue_outcomes_stock_scenario_diff_ls)
  revenue_outcomes_scenario_diff_ls = do.call(rbind, revenue_outcomes_scenario_diff_ls)
  EBITDA_outcomes_scenario_diff_ls = do.call(rbind, EBITDA_outcomes_scenario_diff_ls )

  exp_outcomes_scenario_ls = apply(exp_outcomes_scenario_ls, 2, quantile, probs = c(min_b, max_b))
  stock_outcomes_scenario_ls = apply(stock_outcomes_scenario_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_direct_scenario_ls = apply(revenue_outcomes_direct_scenario_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_exp_scenario_ls = apply(revenue_outcomes_exp_scenario_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_stock_scenario_ls = apply(revenue_outcomes_stock_scenario_ls, 2 , quantile, probs = c(min_b, max_b))
  revenue_outcomes_scenario_ls = apply(revenue_outcomes_scenario_ls, 2 , quantile, probs = c(min_b, max_b))
  
  exp_outcomes_scenario_diff_ls = apply(exp_outcomes_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  stock_outcomes_scenario_diff_ls = apply(stock_outcomes_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_direct_scenario_diff_ls = apply(revenue_outcomes_direct_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_exp_scenario_diff_ls = apply(revenue_outcomes_exp_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_stock_scenario_diff_ls = apply(revenue_outcomes_stock_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  revenue_outcomes_scenario_diff_ls = apply(revenue_outcomes_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  EBITDA_outcomes_scenario_diff_ls = apply(EBITDA_outcomes_scenario_diff_ls, 2, quantile, probs = c(min_b, max_b))
  
  # get summary dataframes. 
  summary_lower = data.frame(exp_outcomes_scenario_diff  = exp_outcomes_scenario_diff_ls[1,] ,
                             stock_outcomes_scenario_diff  =  stock_outcomes_scenario_diff_ls[1,] ,
                             revenue_outcomes_direct_scenario_diff =  revenue_outcomes_direct_scenario_diff_ls[1,],
                             revenue_outcomes_exp_scenario_diff = revenue_outcomes_exp_scenario_diff_ls[1,],
                             revenue_outcomes_stock_scenario_diff = revenue_outcomes_stock_scenario_diff_ls[1,],
                             revenue_outcomes_scenario_diff  =  revenue_outcomes_scenario_diff_ls[1,],
                             EBITDA_diff = EBITDA_outcomes_scenario_diff_ls[1,],
                             exp_outcomes_scenario_percent = exp_outcomes_scenario_ls[1,],
                             stock_outcomes_scenario_percent = stock_outcomes_scenario_ls[1,],
                             revenue_outcomes_direct_scenario_percent = revenue_outcomes_direct_scenario_ls[1,],
                             revenue_outcomes_exp_scenario_percent = revenue_outcomes_exp_scenario_ls[1,],
                             revenue_outcomes_stock_scenario_percent = revenue_outcomes_stock_scenario_ls[1,],
                             revenue_outcomes_scenario_percent = revenue_outcomes_scenario_ls[1,]
  )
  
  summary_upper = data.frame(exp_outcomes_scenario_diff  = exp_outcomes_scenario_diff_ls[2,] ,
                             stock_outcomes_scenario_diff  =  stock_outcomes_scenario_diff_ls[2,] ,
                             revenue_outcomes_direct_scenario_diff =  revenue_outcomes_direct_scenario_diff_ls[2,],
                             revenue_outcomes_exp_scenario_diff = revenue_outcomes_exp_scenario_diff_ls[2,],
                             revenue_outcomes_stock_scenario_diff = revenue_outcomes_stock_scenario_diff_ls[2,],
                             revenue_outcomes_scenario_diff  =  revenue_outcomes_scenario_diff_ls[2,],
                             EBITDA_diff = EBITDA_outcomes_scenario_diff_ls[2,],
                             exp_outcomes_scenario_percent = exp_outcomes_scenario_ls[2,],
                             stock_outcomes_scenario_percent = stock_outcomes_scenario_ls[2,],
                             revenue_outcomes_direct_scenario_percent = revenue_outcomes_direct_scenario_ls[2,],
                             revenue_outcomes_exp_scenario_percent = revenue_outcomes_exp_scenario_ls[2,],
                             revenue_outcomes_stock_scenario_percent = revenue_outcomes_stock_scenario_ls[2,],
                             revenue_outcomes_scenario_percent = revenue_outcomes_scenario_ls[2,])
  
  return(list(summary_lower = summary_lower, 
         summary_upper = summary_upper))
  
}


