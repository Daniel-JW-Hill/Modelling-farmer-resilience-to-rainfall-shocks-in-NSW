
# prints summary results
getTable = function(plot_data, Discount_rate){
  
  data_summary = plot_data$data_summary
  data_summary_lower = plot_data$data_summary_lower
  data_summary_upper = plot_data$data_summary_upper
  
  discount_factors = 1 / (1 + Discount_rate)^(0:9)
  
  summary_table = data.frame(
    Outcome = c(
      "Average change in annual expenditure ($AUD/ha)",
      "Average % change in annual expenditure per ha",
      "Change in NPV expenditure ($AUD/ha)",
      "% change in NPV expenditure per ha",
      "",
      "Average change in opening stock value ($AUD/ha)",
      "Average % change in opening stock value per ha",
      "",
      "Average change in annual gross returns directly derived from rainfall ($AUD/ha)",
      "Average % change in annual gross returns directly derived from rainfall per ha",
      "Change in NPV gross returns directly derived from rainfall ($AUD/ha)",
      "% change in NPV gross returns directly derived from rainfall per ha",
      "",
      "Average change in annual gross returns directly derived from expenditure ($AUD/ha)",
      "Average % change in annual gross returns directly derived from expenditure per ha",
      "Change in NPV gross returns directly derived from expenditure ($AUD/ha)",
      "% change in NPV gross returns directly derived from expenditure per ha",
      "",
      "Average change in annual gross returns directly derived from stock value ($AUD/ha)",
      "Average % change in annual gross returns directly derived from stock value per ha",
      "Change in NPV gross returns directly derived from stock value ($AUD/ha)",
      "% change in NPV gross returns directly derived from stock value per ha",
      "",
      "Average change in annual gross returns ($AUD/ha)",
      "Average % change in annual gross returns per ha",
      "Change in NPV gross returns ($AUD/ha)",
      "% change in NPV gross returns per ha"
      
    ),
    Central_Value = c(
      round(mean(data_summary$exp_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$exp_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary$exp_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary$exp_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary$stock_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$stock_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      "",
      
      round(mean(data_summary$revenue_outcomes_direct_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$revenue_outcomes_direct_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary$revenue_outcomes_direct_scenario_diff * discount_factors), 2),
      round((sum(data_summary$revenue_outcomes_direct_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_direct_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary$revenue_outcomes_exp_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$revenue_outcomes_exp_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary$revenue_outcomes_exp_scenario_diff * discount_factors), 2),
      round((sum(data_summary$revenue_outcomes_exp_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary$revenue_outcomes_stock_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$revenue_outcomes_stock_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary$revenue_outcomes_stock_scenario_diff * discount_factors), 2),
      round((sum(data_summary$revenue_outcomes_stock_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_stock_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary$revenue_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary$revenue_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary$revenue_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary$revenue_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_baseline * discount_factors)) * 100, 2)
    ),
    
    Lower_CI_Value = c(
      round(mean(data_summary_lower$exp_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$exp_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_lower$exp_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary_lower$exp_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_lower$stock_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$stock_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      "",
      
      round(mean(data_summary_lower$revenue_outcomes_direct_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$revenue_outcomes_direct_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_lower$revenue_outcomes_direct_scenario_diff * discount_factors), 2),
      round((sum(data_summary_lower$revenue_outcomes_direct_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_direct_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_lower$revenue_outcomes_exp_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$revenue_outcomes_exp_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_lower$revenue_outcomes_exp_scenario_diff * discount_factors), 2),
      round((sum(data_summary_lower$revenue_outcomes_exp_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_lower$revenue_outcomes_stock_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$revenue_outcomes_stock_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_lower$revenue_outcomes_stock_scenario_diff * discount_factors), 2),
      round((sum(data_summary_lower$revenue_outcomes_stock_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_stock_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_lower$revenue_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_lower$revenue_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_lower$revenue_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary_lower$revenue_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_baseline * discount_factors)) * 100, 2)
    ),
    
    Upper_CI_Value = c(
      round(mean(data_summary_upper$exp_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$exp_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_upper$exp_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary_upper$exp_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_upper$stock_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$stock_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      "",
      
      round(mean(data_summary_upper$revenue_outcomes_direct_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$revenue_outcomes_direct_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_upper$revenue_outcomes_direct_scenario_diff * discount_factors), 2),
      round((sum(data_summary_upper$revenue_outcomes_direct_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_direct_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_upper$revenue_outcomes_exp_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$revenue_outcomes_exp_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_upper$revenue_outcomes_exp_scenario_diff * discount_factors), 2),
      round((sum(data_summary_upper$revenue_outcomes_exp_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_exp_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_upper$revenue_outcomes_stock_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$revenue_outcomes_stock_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_upper$revenue_outcomes_stock_scenario_diff * discount_factors), 2),
      round((sum(data_summary_upper$revenue_outcomes_stock_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_stock_baseline * discount_factors)) * 100, 2),
      "",
      
      round(mean(data_summary_upper$revenue_outcomes_scenario_diff, na.rm = TRUE), 2),
      round(mean(data_summary_upper$revenue_outcomes_scenario_percent - 100, na.rm = TRUE), 2),
      round(sum(data_summary_upper$revenue_outcomes_scenario_diff * discount_factors), 2),
      round((sum(data_summary_upper$revenue_outcomes_scenario_diff * discount_factors) / 
               sum(data_summary$revenue_baseline * discount_factors)) * 100, 2)
    )
    
  )
  
  return(summary_table)
  
}