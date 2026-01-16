
# Estimates GAMs models and saves results in designated file-paths. 

estimate_GAMs_models = function(data_GAMs, new_data_list) {
  
  data_GAMs = as.data.frame(data_GAMs)
  data_GAMs = data_GAMs[which(data_GAMs$analysis_regions %in% c("Southern_Tablelands", "Northern_Tablelands", "Central_West", "Far_West", "Murray_Riverina")),]
  unique_regions = unique(data_GAMs$analysis_regions)
  range_SPI = quantile(data_GAMs$SPI_index, probs = c(0.05,0.95), na.rm = TRUE) # for line charts. 
  
  # First estimate models for each region separately. 
  eq_list = list()
  eq_list[[1]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year + demeaned_termsoftrade_index + s(SPI_index) ')
  eq_list[[2]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year + demeaned_termsoftrade_index + s(SPI_index) + s(SPI_index_Lag1)')
  eq_list[[3]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year + demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2)')
  eq_list[[4]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year + demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2) + s(SPI_index_Lag3) ')
  eq_list[[5]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year + demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2) + s(SPI_index_Lag3) + s(SPI_index_Lag4) ')

  data_subset_list = list()
  model_list = list()
  AIC_list = list()
  BIC_list = list()
  
  for (rr in 1:length(unique_regions)){
    r = unique_regions[rr]
    print(r)
    data_subset = subset(data_GAMs, data_GAMs$analysis_regions == r)
    AIC_vec = data.frame("AIC" = rep(NA, length(eq_list)+1),
                         "BIC"= rep(NA, length(eq_list)+1),
                         "obs" = rep(NA, length(eq_list)+1),
                         "degrees_freedom" = rep(NA, length(eq_list)+1),
                         "r2" = rep(NA, length(eq_list)+1),
                         "deviance_explained" = rep(NA, length(eq_list)+1))
    gam_models = list()
    for (eq in 1:length(eq_list)) {
      gam_models[[eq]] = gam(eq_list[[eq]], data = data_subset, family = "scat", method = "REML", na.action = na.exclude)
      AIC_vec$AIC[eq] = AIC(gam_models[[eq]])
      AIC_vec$BIC[eq] = BIC(gam_models[[eq]])
      AIC_vec$obs[eq] = summary(gam_models[[eq]])$n
      AIC_vec$degrees_freedom[eq] = summary(gam_models[[eq]])$n - summary(gam_models[[eq]])$np
      AIC_vec$r2[eq] = summary(gam_models[[eq]])$r.sq
      AIC_vec$deviance_explained[eq] = summary(gam_models[[eq]])$dev.expl
    }

    best_model = which(AIC_vec$BIC == min(AIC_vec$BIC, na.rm = TRUE))[1]
    
    # test if lead SPI value improves the model. 
    eq_lead = update(eq_list[[best_model]], reformulate(c(".", "s(SPI_index_Lead1)")))
    gam_model_lead = gam(eq_lead, data = data_subset, family = "scat", method = "REML")
    AIC_vec$AIC[eq+1] = AIC(gam_model_lead)
    AIC_vec$BIC[eq+1] = BIC(gam_model_lead)
    AIC_vec$obs[eq+1] = summary(gam_model_lead)$n
    AIC_vec$degrees_freedom[eq+1] = summary(gam_model_lead)$n - summary(gam_model_lead)$np
    AIC_vec$r2[eq+1] = summary(gam_model_lead)$r.sq
    AIC_vec$deviance_explained[eq+1] = summary(gam_model_lead)$dev.expl
    
    
    if (AIC_vec$BIC[eq+1] < AIC_vec$BIC[best_model]){
      model_list[[r]] = gam_model_lead
      eq_best = eq_lead
    } else {
      model_list[[r]] = gam_models[[best_model]]
      eq_best = eq_list[[best_model]]
    }
    
    # Run white noise tests
    white_noise_tests(model_list[[r]],  r)
    
    # Retrieve fitted values with new data.
    vars_in_model = all.vars(eq_best)
    SPI_vars = vars_in_model[startsWith(vars_in_model,"SPI")]
    newdata_list <- lapply(SPI_vars, function(SPI) {
      out <- data_subset
      out[base::setdiff(names(data_subset), SPI)] <- 0
      out
    })
    names(newdata_list) = SPI_vars
    data_SPI = data_subset
    data_SPI[base::setdiff(names(data_subset), SPI_vars)] <- 0
    newdata_list[["all_SPI"]] =  data_SPI
    
    SPI_vars = c(SPI_vars, "ALL_SPI")
    for (SPI_var in 1:length(newdata_list)){
      pred_index = predict.gam(model_list[[r]], newdata = newdata_list[[SPI_var]], , type = 'response')
      var_name = paste("pred", SPI_vars[SPI_var])
      data_subset[,var_name] = pred_index 
    }
    
    # Save data, AIC, and BIC vecs. 
    data_subset_list[[r]] = data_subset
    vars_in_model = all.vars(update(eq_list[[length(eq_list)]], reformulate(c(".", "s(SPI_index_Lead1)"))))
    SPI_vars = vars_in_model[startsWith(vars_in_model,"SPI")]
    rownames(AIC_vec) = SPI_vars
    AIC_list[[r]] = AIC_vec 
    
    # generate line charts for best model. 
    simple_gams_lines(model_list[[r]], data_GAMs, r, range_SPI)
    print(summary(model_list[[r]]))
  }
  
  # Now estimate NSW modell with region specific trends 
  print("NSW")
  eq_list = list()
  eq_list[[1]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year +  NTablelands_trend + CWest_trend + Murray_trend + demeaned_termsoftrade_index + s(SPI_index)  ')
  eq_list[[2]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year +  NTablelands_trend + CWest_trend + Murray_trend +  demeaned_termsoftrade_index + s(SPI_index) + s(SPI_index_Lag1)')
  eq_list[[3]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year +  NTablelands_trend + CWest_trend + Murray_trend +  demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2)')
  eq_list[[4]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year +  NTablelands_trend + CWest_trend + Murray_trend +  demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2) + s(SPI_index_Lag3) ')
  eq_list[[5]] = as.formula('demeaned_revenue ~  demeaned_revenue_lag + demeaned_exp + demeaned_assets + demeaned_stock +  year +  NTablelands_trend + CWest_trend + Murray_trend +  demeaned_termsoftrade_index + s(SPI_index) +  s(SPI_index_Lag1) + s(SPI_index_Lag2) + s(SPI_index_Lag3) + s(SPI_index_Lag4) ')
  
   AIC_vec = data.frame("AIC" = rep(NA, length(eq_list)+1),
                       "BIC"= rep(NA, length(eq_list)+1),
                       "obs" = rep(NA, length(eq_list)+1),
                       "degrees_freedom" = rep(NA, length(eq_list)+1),
                       "r2" = rep(NA, length(eq_list)+1),
                       "deviance_explained" = rep(NA, length(eq_list)+1))
  gam_models = list()
  
  for (eq in 1:length(eq_list)) {
    vars_in_model = all.vars(eq_list[[eq]])
    data_GAMs_clean = data_GAMs[complete.cases(data_GAMs[,vars_in_model]),]
    gam_models[[eq]] = gam(eq_list[[eq]], data = data_GAMs_clean , family = "scat", ,method = "REML", na.action = na.exclude) 
    AIC_vec$AIC[eq] = AIC(gam_models[[eq]])
    AIC_vec$BIC[eq] = BIC(gam_models[[eq]])
    AIC_vec$obs[eq] = summary(gam_models[[eq]])$n
    AIC_vec$degrees_freedom[eq] = summary(gam_models[[eq]])$n - summary(gam_models[[eq]])$np
    AIC_vec$r2[eq] = summary(gam_models[[eq]])$r.sq
    AIC_vec$deviance_explained[eq] = summary(gam_models[[eq]])$dev.expl
  }
  
  best_model = which(AIC_vec$BIC == min(AIC_vec$BIC, na.rm = TRUE))[1]
  
  # test if lead SPI value improves the model. 
  eq_lead = update(eq_list[[best_model]], reformulate(c(".", "s(SPI_index_Lead1)")))
  vars_in_model = all.vars(eq_lead)
  data_GAMs_clean = data_GAMs[complete.cases(data_GAMs[,vars_in_model]),]
  gam_model_lead = gam(eq_lead, data = data_GAMs_clean, family = "scat", method = "REML", na.action = na.exclude)
  AIC_vec$AIC[eq+1] = AIC(gam_model_lead)
  AIC_vec$BIC[eq+1] = BIC(gam_model_lead)
  AIC_vec$obs[eq+1] = summary(gam_model_lead)$n
  AIC_vec$degrees_freedom[eq+1] = summary(gam_model_lead)$n - summary(gam_model_lead)$np
  AIC_vec$r2[eq+1] = summary(gam_model_lead)$r.sq
  AIC_vec$deviance_explained[eq+1] = summary(gam_model_lead)$dev.expl
  
  if (AIC_vec$BIC[eq+1] < AIC_vec$BIC[best_model]){
    model_list[["NSW"]] = gam_model_lead
    eq_best = eq_lead
  } else {
    model_list[["NSW"]] = gam_models[[best_model]]
    eq_best = eq_list[[best_model]]
  }
  
  # Run white noise tests
  white_noise_tests(model_list[["NSW"]], "NSW")
  
  # Retrieve fitted values with new data.
  # Index for each SPI, and one combined 
  vars_in_model = all.vars(eq_best)
  SPI_vars = vars_in_model[startsWith(vars_in_model,"SPI")]
  newdata_list <- lapply(SPI_vars, function(SPI) {
    out <- data_GAMs
    out[base::setdiff(names(data_subset), SPI)] <- 0
    out
  })
  names(newdata_list) = SPI_vars
  data_SPI = data_GAMs
  data_SPI[base::setdiff(names(data_subset), SPI_vars)] <- 0
  newdata_list[["all_SPI"]] =  data_SPI
  
  SPI_vars = c(SPI_vars, "ALL_SPI")
  for (SPI_var in 1:length(newdata_list)){
    pred_index = predict.gam(model_list[["NSW"]], newdata = newdata_list[[SPI_var]], , type = 'response')
    var_name = paste("pred", SPI_vars[SPI_var])
    data_GAMs[,var_name] = pred_index 
  }
  
  vars_in_model = all.vars(update(eq_list[[length(eq_list)]], reformulate(c(".", "s(SPI_index_Lead1)"))))
  SPI_vars = vars_in_model[startsWith(vars_in_model,"SPI")]
  rownames(AIC_vec) = SPI_vars
  AIC_list[["NSW"]] = AIC_vec
  
  # generate line charts for best NSW models. 
  simple_gams_lines(model_list[["NSW"]], data_GAMs,  "NSW", range_SPI)
  print(summary(model_list[["NSW"]]))
  
  # Save all model results
  for (model in names(model_list)){
    save_GAMs_results(model_list[[model]], paste(model, "gam_model_results", sep = "_"))
  }
  
  # Save AIC and BIC results, with model export checks
  for (AIC_results in names(AIC_list)){
    write.csv(AIC_list[[AIC_results]], file = file.path("Results", "GAMmodels", paste(AIC_results, "_AIC_results", ".csv")))
  }
  
  return(list(data_GAMs, data_subset_list))
  
}

# End of function. 

