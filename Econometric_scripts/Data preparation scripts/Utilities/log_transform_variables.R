
#Log transforms variables and generates new variables where needed. 


log_transform_variables = function(data, 
                                   freq = "annual"){
  
  #IHS transformation to map zero values to log transformation
  ihs = function(x){
    y = log(x + sqrt(x^2 + 1)) 
    return(y)
  }
  
  if (freq == "annual"){
    
    data$exp_total_perha = data$exp_total / data$area_holdings_derived
    data$stock_opening_perha = data$stock_opening / data$area_holdings_derived
    data$stock_closing_perha = data$stock_closing / data$area_holdings_derived
    
    data$exp_total_perha_log = ihs(data$exp_total_perha)
    data$stock_opening_perha_log = ihs(data$stock_opening_perha)
    data$stock_closing_perha_log = ihs(data$stock_closing_perha)
    data$assets_less_stock_perha_log = ihs(data$assets_less_stock_perha)
    data$liability_total_perha_log = ihs(data$liability_total_perha)
    data$profit_net_operating_perha_log = ihs(data$profit_net_operating_perha)
    data$gross_returns_perha_log = ihs(data$gross_returns_perha)
    data$EBIT_perha_log = ihs(data$EBIT_perha)
 
    
  } else {
    
    data$profits_seasadj = data$tot_sales_seasadj  -  data$tot_expenses_seasadj
    data$equity =  data$assets_total - data$liabilities_total
    
    data$tot_expenses_seasadj_perha = data$tot_expenses_seasadj/ data$area_holdings_derived
    data$tot_sales_seasadj_perha = data$tot_expenses_seasadj/ data$area_holdings_derived
    data$profits_seasadj_perha = data$profits_seasadj/ data$area_holdings_derived
    data$opex_seasadj_perha = data$opex_seasadj/ data$area_holdings_derived
    data$equity_perha =  data$equity / data$area_holdings_derived
    data$stock_interpolate_perha = data$stock_interpolate / data$area_holdings_derived
    
    data$tot_expenses_seasadj_perha_log = ihs(data$tot_expenses_seasadj_perha)
    data$tot_sales_seasadj_perha_log = ihs(data$tot_sales_seasadj_perha )
    data$profits_seasadj_perha_log = ihs(data$profits_seasadj_perha )
    data$opex_seasadj_perha_log = ihs(data$opex_seasadj_perha)
    data$equity_perha_log = ihs(data$equity_perha)
    data$stock_interpolate_perha_log = ihs(data$stock_interpolate_perha)
    data$ESI_quarterly_log = ihs(data$ESI_quarterly)
    
    }
  
  return(data)
  
}