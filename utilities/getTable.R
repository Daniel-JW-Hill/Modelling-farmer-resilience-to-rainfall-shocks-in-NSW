
# prints summary results
getTable = function(plot_data, Discount_rate){
  
  data_summary = plot_data$data_summary
  data_summary_lower = plot_data$data_summary_lower
  data_summary_upper = plot_data$data_summary_upper
  
  years_length = length(data_summary$revenue_outcomes_scenario_diff)
  
  discount_factors = 1 / (1 + Discount_rate)^(0:(years_length-5))
  
  summary_table = data.frame(
    Outcome = c(
      "Change in stock value per ha in year t due to historical rainfall outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in expenditure per ha in year t due to historical rainfall outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in gross returns per ha in year t-1 due to historical rainfall outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in gross returns per ha in year t due to prevailing rainfall outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in stock value per ha in year t+1 in response to historical rainfall and revenue outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in expenditure per ha in year t+1 in response to historical rainfall and revenue outcomes, and current period stocking decisions ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in gross returns per ha in year t+1 in response to historical rainfall outcomes ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in gross returns per ha in year t+1 deriving from stock value decisions in period t+1 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Change in gross returns per ha in year t+1 deriving from expenditure decisions in period t+1 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Net change in revenue per ha in year t+1 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Net change in revenue per ha in year t+2 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Net change in revenue per ha in year t+3 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Net change in revenue per ha in year t+4 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Net change in revenue per ha in year t+5 ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Average net change in revenue per ha in remaining years ($AUD/ha)",
      "Percent change relative to normal rainfall conditions for all years.",
      "",
      
      "Aggregate net change in revenue per ha from year t to end ($AUD/ha)",
      "",
      
      "Net change in revenue per ha from year t to end ($AUD/ha, discounted)",
      ""
    ),
    
    
    Central_Value = c(
      round(data_summary$stock_outcomes_scenario_diff[5], 2), #Change in stock value per ha in year t
      round(data_summary$stock_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary$exp_outcomes_scenario_diff[5], 2), #Change in exp value per ha in year t
      round(data_summary$exp_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[4], 2), #Change in revenue value per ha in year t-1
      round(data_summary$revenue_outcomes_scenario_percent[4] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[5], 2), #Change in revenue value per ha in year t direct
      round(data_summary$revenue_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary$stock_outcomes_scenario_diff[6], 2), #Change in stock value per ha in year t+1
      round(data_summary$stock_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$exp_outcomes_scenario_diff[6], 2), #Change in expenditure per ha in year t+1
      round(data_summary$exp_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_direct_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 direct
      round(data_summary$revenue_outcomes_direct_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_stock_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from stock
      round(data_summary$revenue_outcomes_stock_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_exp_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from exp
      round(data_summary$revenue_outcomes_exp_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 net
      round(data_summary$revenue_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[7], 2), #Change in revenue value per ha in year t+2 net
      round(data_summary$revenue_outcomes_scenario_percent[7] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[8], 2), #Change in revenue value per ha in year t+3 net
      round(data_summary$revenue_outcomes_scenario_percent[8] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[9], 2), #Change in revenue value per ha in year t+4 net
      round(data_summary$revenue_outcomes_scenario_percent[9] - 100, 2),
      "---",
      
      round(data_summary$revenue_outcomes_scenario_diff[10], 2), #Change in revenue value per ha in year t+5 net
      round(data_summary$revenue_outcomes_scenario_percent[10] - 100, 2),
      "---",
      
      round(mean(data_summary$revenue_outcomes_scenario_diff[11:years_length], na.rm = TRUE), 2), #Change in revenue value per ha for remaining years
      round(mean(data_summary$revenue_outcomes_scenario_percent[11:years_length] - 100, na.rm = TRUE), 2),
      "---",
      
      round(sum(data_summary$revenue_outcomes_scenario_diff[5:years_length], na.rm = TRUE), 2), #Aggregate change in revenue value per ha 
      "---",
      
      round(sum(data_summary$revenue_outcomes_scenario_diff[5:years_length] * discount_factors, na.rm = TRUE) , 2), #discounted aggregate change in revenue value per ha 
      "---"
      
    ),
    
    Lower_CI_Value = c(
      round(data_summary_lower$stock_outcomes_scenario_diff[5], 2), #Change in stock value per ha in year t
      round(data_summary_lower$stock_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_lower$exp_outcomes_scenario_diff[5], 2), #Change in exp value per ha in year t
      round(data_summary_lower$exp_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[4], 2), #Change in revenue value per ha in year t-1
      round(data_summary_lower$revenue_outcomes_scenario_percent[4] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[5], 2), #Change in revenue value per ha in year t direct
      round(data_summary_lower$revenue_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_lower$stock_outcomes_scenario_diff[6], 2), #Change in stock value per ha in year t+1
      round(data_summary_lower$stock_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$exp_outcomes_scenario_diff[6], 2), #Change in expenditure per ha in year t+1
      round(data_summary_lower$exp_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_direct_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 direct
      round(data_summary_lower$revenue_outcomes_direct_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_stock_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from stock
      round(data_summary_lower$revenue_outcomes_stock_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_exp_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from exp
      round(data_summary_lower$revenue_outcomes_exp_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 net
      round(data_summary_lower$revenue_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[7], 2), #Change in revenue value per ha in year t+2 net
      round(data_summary_lower$revenue_outcomes_scenario_percent[7] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[8], 2), #Change in revenue value per ha in year t+3 net
      round(data_summary_lower$revenue_outcomes_scenario_percent[8] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[9], 2), #Change in revenue value per ha in year t+4 net
      round(data_summary_lower$revenue_outcomes_scenario_percent[9] - 100, 2),
      "---",
      
      round(data_summary_lower$revenue_outcomes_scenario_diff[10], 2), #Change in revenue value per ha in year t+5 net
      round(data_summary_lower$revenue_outcomes_scenario_percent[10] - 100, 2),
      "---",
      
      round(mean(data_summary_lower$revenue_outcomes_scenario_diff[11:years_length], na.rm = TRUE), 2), #Change in revenue value per ha for remaining years
      round(mean(data_summary_lower$revenue_outcomes_scenario_percent[11:years_length] - 100, na.rm = TRUE), 2),
      "---",
      
      round(sum(data_summary_lower$revenue_outcomes_scenario_diff[5:years_length], na.rm = TRUE), 2), #Aggregate change in revenue value per ha 
       "---",
      
      round(sum(data_summary_lower$revenue_outcomes_scenario_diff[5:years_length] * discount_factors, na.rm = TRUE) , 2), #discounted aggregate change in revenue value per ha 
      "---"
      
    ),
    
    Upper_CI_Value = c(
      round(data_summary_upper$stock_outcomes_scenario_diff[5], 2), #Change in stock value per ha in year t
      round(data_summary_upper$stock_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_upper$exp_outcomes_scenario_diff[5], 2), #Change in exp value per ha in year t
      round(data_summary_upper$exp_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[4], 2), #Change in revenue value per ha in year t-1
      round(data_summary_upper$revenue_outcomes_scenario_percent[4] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[5], 2), #Change in revenue value per ha in year t direct
      round(data_summary_upper$revenue_outcomes_scenario_percent[5] - 100, 2),
      "---",
      
      round(data_summary_upper$stock_outcomes_scenario_diff[6], 2), #Change in stock value per ha in year t+1
      round(data_summary_upper$stock_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$exp_outcomes_scenario_diff[6], 2), #Change in expenditure per ha in year t+1
      round(data_summary_upper$exp_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_direct_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 direct
      round(data_summary_upper$revenue_outcomes_direct_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_stock_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from stock
      round(data_summary_upper$revenue_outcomes_stock_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_exp_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 from exp
      round(data_summary_upper$revenue_outcomes_exp_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[6], 2), #Change in revenue value per ha in year t+1 net
      round(data_summary_upper$revenue_outcomes_scenario_percent[6] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[7], 2), #Change in revenue value per ha in year t+2 net
      round(data_summary_upper$revenue_outcomes_scenario_percent[7] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[8], 2), #Change in revenue value per ha in year t+3 net
      round(data_summary_upper$revenue_outcomes_scenario_percent[8] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[9], 2), #Change in revenue value per ha in year t+4 net
      round(data_summary_upper$revenue_outcomes_scenario_percent[9] - 100, 2),
      "---",
      
      round(data_summary_upper$revenue_outcomes_scenario_diff[10], 2), #Change in revenue value per ha in year t+5 net
      round(data_summary_upper$revenue_outcomes_scenario_percent[10] - 100, 2),
      "---",
      
      round(mean(data_summary_upper$revenue_outcomes_scenario_diff[11:years_length], na.rm = TRUE), 2), #Change in revenue value per ha for remaining years
      round(mean(data_summary_upper$revenue_outcomes_scenario_percent[11:years_length] - 100, na.rm = TRUE), 2),
      "---",
      
      round(sum(data_summary_upper$revenue_outcomes_scenario_diff[5:years_length], na.rm = TRUE), 2), #Aggregate change in revenue value per ha 
      "---",
      
      round(sum(data_summary_upper$revenue_outcomes_scenario_diff[5:years_length] * discount_factors, na.rm = TRUE) , 2), #discounted aggregate change in revenue value per ha 
      "---"
      
    )
    
  )
  
  return(summary_table)
  
}