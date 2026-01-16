
# Creates lag and first difference variables from plm data
# Also creates SA3 and 4 regions for 20 year panel. 
# note - much of this script is legacy when analysis was also considering quarterly data

get_lagged_variables = function(data,freq = "annual"){
  
  if(freq == "annual"){
    
    data$exp_total_perha_log_lag = plm::lag(data$exp_total_perha_log)
    data$stock_opening_perha_log_lag = plm::lag(data$stock_opening_perha_log)
    data$stock_closing_perha_log_lag = plm::lag(data$stock_closing_perha_log)
    data$profit_net_operating_perha_log_lag = plm::lag(data$profit_net_operating_perha_log)
    data$gross_returns_perha_log_lag = plm::lag(data$gross_returns_perha_log)
    data$assets_less_stock_perha_log_lag = plm::lag(data$assets_less_stock_perha_log)
    data$liability_total_perha_log_lag = plm::lag(data$liability_total_perha_log)
    data$EBIT_perha_log_lag = plm::lag(data$EBIT_perha_log)
    data$ESI_annual_lag = plm::lag(data$ESI_annual)
    data$ESI_annual_NSW_lag = plm::lag(data$ESI_annual_NSW)
    data$ESI_annual_SUMMER_lag = plm::lag(data$ESI_annual_SUMMER)
    data$ESI_annual_WINTER_lag = plm::lag(data$ESI_annual_WINTER)
    data$ESI_annual_SPRING_lag = plm::lag(data$ESI_annual_SPRING)

    data$SA3_region = as.numeric(substr(as.character(data$sa2_code_2021), 1,5))
    data$SA4_region = as.numeric(substr(as.character(data$SA3_region), 1,3))
    data$SA4_region = as.factor(data$SA4_region)
    data$latest_anzsic_all = as.factor(data$latest_anzsic_all)
    
  } else {
    
    ihs = function(x){
      y = log(x + sqrt(x^2 + 1))
      return(y)
    }
    
    data$buildup_trading_stock_perha = rowSums(cbind(data$stock_interpolate__perha, - lag(data$stock_interpolate_perha,1)))
    data$gross_returns_perha = rowSums(cbind(data$tot_sales_seasadj_perha, data$buildup_trading_stock_perha))
    data$EBIT_perha = rowSums(cbind(data$gross_returns_perha , data$tot_expenses_seasadj_perha))
    data$gross_returns_perha_log = ihs(data$gross_returns_perha)
    data$EBIT_perha_log = ihs(data$EBIT_perha)
    
    data$tot_expenses_seasadj_perha_log_lag = plm::lag(data$tot_expenses_seasadj_perha_log)
    data$tot_sales_seasadj_perha_log_lag = plm::lag(data$tot_sales_seasadj_perha_log)
    data$profits_seasadj_perha_log_lag = plm::lag(data$profits_seasadj_perha_log)
    data$opex_seasadj_perha_log_lag = plm::lag(data$opex_seasadj_perha_log)
    data$equity_perha_log_lag = plm::lag(data$equity_perha_log)
    data$ESI_quarterly_lag = plm::lag(data$ESI_quarterly)
    data$stock_interpolate_perha_log_lag = plm::lag(data$stock_interpolate_perha_log)
    data$gross_returns_perha_log_lag = plm::lag(data$gross_returns_perha_log)
    data$EBIT_perha_log_lag = plm::lag(data$EBIT_perha_log)
    
    data$tot_expenses_seasadj_perha_log_fdiff = data$tot_expenses_seasadj_perha_log - data$tot_expenses_seasadj_perha_log_lag
    data$tot_sales_seasadj_perha_log_fdiff = data$tot_sales_seasadj_perha_log - data$tot_sales_seasadj_perha_log_lag
    data$profits_seasadj_perha_log_fdiff = data$profits_seasadj_perha_log - data$profits_seasadj_perha_log_lag
    data$opex_seasadj_perha_log_fdiff = data$opex_seasadj_perha_log - data$opex_seasadj_perha_log_lag
    data$equity_perha_log_fdiff = data$equity_perha_log - data$equity_perha_log_lag
    data$ESI_quarterly_fdiff =data$ESI_quarterly - data$ESI_quarterly_lag
    data$gross_returns_perha_log_fdiff = data$gross_returns_perha_log - data$gross_returns_perha_log_lag
    data$EBIT_perha_log_fdiff = data$EBIT_perha_log - data$EBIT_perha_log_lag
    data$stock_interpolate_perha_log_fdiff = data$stock_interpolate_perha_log - data$stock_interpolate_perha_log_lag
  }
  
  return(data)
}

# End of function