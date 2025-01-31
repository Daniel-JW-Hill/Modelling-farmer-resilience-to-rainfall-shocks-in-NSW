
# Saves summary data frame. 

getSummaryCentral = function(central_simulation, 
                             weather_index_scenario, 
                             weather_index_baseline,
                             Region_select, 
                             levels_grossReturns, 
                             levels_exp, 
                             levels_stock) {
  
  exp_outcomes_baseline = central_simulation$exp_outcomes_baseline
  exp_outcomes_scenario = central_simulation$exp_outcomes_scenario
  stock_outcomes_baseline = central_simulation$stock_outcomes_baseline
  stock_outcomes_scenario = central_simulation$stock_outcomes_scenario
  revenue_outcomes_direct_baseline = central_simulation$revenue_outcomes_direct_baseline 
  revenue_outcomes_direct_scenario = central_simulation$revenue_outcomes_direct_scenario
  revenue_outcomes_exp_baseline = central_simulation$revenue_outcomes_exp_baseline
  revenue_outcomes_exp_scenario = central_simulation$revenue_outcomes_exp_scenario
  revenue_outcomes_stock_baseline = central_simulation$revenue_outcomes_stock_baseline
  revenue_outcomes_stock_scenario = central_simulation$revenue_outcomes_stock_scenario
  revenue_outcomes_baseline = central_simulation$revenue_outcomes_baseline 
  revenue_outcomes_scenario = central_simulation$revenue_outcomes_scenario
  
  
  # Add levels back into estimates
  exp_outcomes_baseline = exp_outcomes_baseline + log(levels_exp$levels[levels_exp$regions == Region_select])
  stock_outcomes_baseline = stock_outcomes_baseline + log(levels_stock$levels[levels_stock$regions == Region_select])
  revenue_outcomes_direct_baseline = revenue_outcomes_direct_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_exp_baseline = revenue_outcomes_exp_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_stock_baseline = revenue_outcomes_stock_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_baseline = revenue_outcomes_baseline + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  exp_outcomes_baseline = exp(exp_outcomes_baseline)
  stock_outcomes_baseline = exp(stock_outcomes_baseline)
  revenue_outcomes_direct_baseline = exp(revenue_outcomes_direct_baseline)
  revenue_outcomes_exp_baseline = exp(revenue_outcomes_exp_baseline)
  revenue_outcomes_stock_baseline = exp(revenue_outcomes_stock_baseline)
  revenue_outcomes_baseline = exp(revenue_outcomes_baseline)
  
  exp_outcomes_scenario = exp_outcomes_scenario + log(levels_exp$levels[levels_exp$regions == Region_select])
  stock_outcomes_scenario = stock_outcomes_scenario + log(levels_stock$levels[levels_stock$regions == Region_select])
  revenue_outcomes_direct_scenario = revenue_outcomes_direct_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_exp_scenario = revenue_outcomes_exp_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_stock_scenario = revenue_outcomes_stock_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  revenue_outcomes_scenario = revenue_outcomes_scenario + log(levels_grossReturns$levels[levels_grossReturns$regions == Region_select])
  exp_outcomes_scenario = exp(exp_outcomes_scenario)
  stock_outcomes_scenario = exp(stock_outcomes_scenario)
  revenue_outcomes_direct_scenario = exp(revenue_outcomes_direct_scenario)
  revenue_outcomes_exp_scenario = exp(revenue_outcomes_exp_scenario)
  revenue_outcomes_stock_scenario = exp(revenue_outcomes_stock_scenario)
  revenue_outcomes_scenario = exp(revenue_outcomes_scenario)
  
  # Find differences and relative differences, then plot.
  exp_outcomes_scenario_diff  =  exp_outcomes_scenario - exp_outcomes_baseline
  stock_outcomes_scenario_diff  =  stock_outcomes_scenario - stock_outcomes_baseline
  revenue_outcomes_direct_scenario_diff = revenue_outcomes_direct_scenario - revenue_outcomes_direct_baseline
  revenue_outcomes_exp_scenario_diff = revenue_outcomes_exp_scenario - revenue_outcomes_exp_baseline
  revenue_outcomes_stock_scenario_diff = revenue_outcomes_stock_scenario - revenue_outcomes_stock_baseline
  revenue_outcomes_scenario_diff  =  revenue_outcomes_scenario - revenue_outcomes_baseline
  
  #Save vectors as percentages for ggplot
  exp_outcomes_scenario_percent = (exp_outcomes_scenario_diff / exp_outcomes_baseline) * 100 + 100
  stock_outcomes_scenario_percent = (stock_outcomes_scenario_diff / stock_outcomes_baseline) * 100 + 100
  revenue_outcomes_direct_scenario_percent = (revenue_outcomes_direct_scenario_diff / revenue_outcomes_direct_baseline) * 100 + 100
  revenue_outcomes_exp_scenario_percent = (revenue_outcomes_exp_scenario_diff / revenue_outcomes_exp_baseline) * 100 + 100
  revenue_outcomes_stock_scenario_percent = (revenue_outcomes_stock_scenario_diff / revenue_outcomes_stock_baseline) * 100 + 100
  revenue_outcomes_scenario_percent = (revenue_outcomes_scenario_diff / revenue_outcomes_baseline) *100 + 100
  
  # calculate EBITDA outcomes
  EBITDA_baseline = revenue_outcomes_baseline - exp_outcomes_baseline
  EBITDA_scenario = revenue_outcomes_scenario - exp_outcomes_scenario
  EBITDA_diff = EBITDA_scenario - EBITDA_baseline
  
  # Update weather index (better for plot as normal conditions not precisely zero)
  for (w in 5:(length(weather_index_scenario)-1)) {
    if (weather_index_scenario[w] == weather_index_baseline[1]){# equal to normal conditions
      weather_index_scenario[w] = 0
    } 
  }
  
  # save central vectors
  summary_central = data.frame(weather_indices = weather_index_scenario[5:14],
                               exp_baseline = exp_outcomes_baseline,
                               stock_baseline = stock_outcomes_baseline,
                               revenue_direct_baseline = revenue_outcomes_direct_baseline,
                               revenue_exp_baseline = revenue_outcomes_exp_baseline,
                               revenue_stock_baseline = revenue_outcomes_stock_baseline,
                               revenue_baseline = revenue_outcomes_baseline,
                               EBITDA_baseline = EBITDA_baseline,
                               exp_outcomes_scenario_diff  = exp_outcomes_scenario_diff ,
                               stock_outcomes_scenario_diff  =  stock_outcomes_scenario_diff ,
                               revenue_outcomes_direct_scenario_diff =  revenue_outcomes_direct_scenario_diff,
                               revenue_outcomes_exp_scenario_diff = revenue_outcomes_exp_scenario_diff,
                               revenue_outcomes_stock_scenario_diff = revenue_outcomes_stock_scenario_diff,
                               revenue_outcomes_scenario_diff  =  revenue_outcomes_scenario_diff,
                               exp_outcomes_scenario_percent = exp_outcomes_scenario_percent,
                               stock_outcomes_scenario_percent = stock_outcomes_scenario_percent,
                               revenue_outcomes_direct_scenario_percent = revenue_outcomes_direct_scenario_percent ,
                               revenue_outcomes_exp_scenario_percent = revenue_outcomes_exp_scenario_percent,
                               revenue_outcomes_stock_scenario_percent = revenue_outcomes_stock_scenario_percent,
                               revenue_outcomes_scenario_percent = revenue_outcomes_scenario_percent,
                               EBITDA_diff = EBITDA_diff
  )
  
  return(summary_central)
}

