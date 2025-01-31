
getFullResults = function(plot_data){
      data_Central = plot_data$data_summary
      data_Lower =  plot_data$data_summary_lower
      data_Upper =  plot_data$data_summary_upper
      
      data_Central$YEAR = 1:10
      data_Central = data_Central[,c(ncol(data_Central),1:(ncol(data_Central)-1))]
      
      colnames(data_Central) = c("Year", 
                                 "Weather_index", 
                                 "Exp_baseline", 
                                 "Stock_baseline", 
                                 "revenue_direct_baseline", 
                                 "revenue_exp_baseline", 
                                 "revenue_stock_baseline", 
                                 "revenue_net_baseline",
                                 "EBITDA_baseline",
                                 "exp_change_scenario",
                                 'stock_change_scenario', 
                                 'revenue_direct_change_scenario',
                                 'revenue_exp_change_scenario',
                                 'revenue_stock_change_scenario',
                                 "revenue_net_change_scenario", 
                                 "EBITDA_change_scenario",
                                 "exp_change_percent_scenario",
                                 'stock_change_percent_scenario', 
                                 'revenue_direct_change_percent_scenario',
                                 'revenue_exp_change_percent_scenario',
                                 'revenue_stock_change_percent_scenario',
                                 "revenue_net_change_percent_scenario")
      
      colnames(data_Lower) = c( "exp_change_scenario_lower",
                                'stock_change_scenario_lower', 
                                'revenue_direct_change_scenario_lower',
                                'revenue_exp_change_scenario_lower',
                                'revenue_stock_change_scenario_lower',
                                "revenue_net_change_scenario_lower", 
                                "EBITDA_change_scenario_lower",
                                "exp_change_percent_scenario_lower",
                                'stock_change_percent_scenario_lower', 
                                'revenue_direct_change_percent_scenario_lower',
                                'revenue_exp_change_percent_scenario_lower',
                                'revenue_stock_change_percent_scenario_lower',
                                "revenue_net_change_percent_scenario_lower")
      
      colnames(data_Upper) = c("exp_change_scenario_upper",
                               'stock_change_scenario_upper', 
                               'revenue_direct_change_scenario_upper',
                               'revenue_exp_change_scenario_upper',
                               'revenue_stock_change_scenario_upper',
                               "revenue_net_change_scenario_upper", 
                               "EBITDA_change_scenario_upper",
                               "exp_change_percent_scenario_upper",
                               'stock_change_percent_scenario_upper', 
                               'revenue_direct_change_percent_scenario_upper',
                               'revenue_exp_change_percent_scenario_upper',
                               'revenue_stock_change_percent_scenario_upper',
                               "revenue_net_change_percent_scenario_upper")
      
      data_full = cbind(data_Central, data_Lower, data_Upper)
      
      return(data_full)
}