
# rusn arellano bond estimators with fitted weather indices. 

run_PGMM = function(pgmm_eq_list, data_subset,  ridx) {
  
  # Run Arellano bond estimators using pdynmc
  print("EXP")
  model_exp = pdynmc::pdynmc(dat = data_subset, 
                             varname.i = "bn",
                             varname.t = "year",
                             use.mc.lev = FALSE,
                             use.mc.diff = TRUE,
                             use.mc.nonlin = FALSE,
                             
                             include.y = TRUE,
                             varname.y = pgmm_eq_list$exp[[ridx]]$varname.y,
                             lagTerms.y =  pgmm_eq_list$exp[[ridx]]$lagTerms.y,
                             maxLags.y = pgmm_eq_list$exp[[ridx]]$maxLags.y,
                             
                             include.x = TRUE,
                             varname.reg.end = pgmm_eq_list$exp[[ridx]]$varname.reg.end,
                             lagTerms.reg.end = pgmm_eq_list$exp[[ridx]]$lagTerms.reg.end,
                             maxLags.reg.end = pgmm_eq_list$exp[[ridx]]$maxLags.reg.end,
                             
                             varname.reg.pre =  pgmm_eq_list$exp[[ridx]]$varname.reg.pre,
                             lagTerms.reg.pre = pgmm_eq_list$exp[[ridx]]$lagTerms.reg.pre,
                             maxLags.reg.pre = pgmm_eq_list$exp[[ridx]]$maxLags.reg.pre,
                             
                             varname.reg.ex = pgmm_eq_list$exp[[ridx]]$varname.reg.ex,
                             lagTerms.reg.ex = pgmm_eq_list$exp[[ridx]]$lagTerms.reg.ex,
                             maxLags.reg.ex = pgmm_eq_list$exp[[ridx]]$maxLags.reg.ex,
                             
                             estimation = pgmm_eq_list$exp[[ridx]]$estimation,
                             std.err = pgmm_eq_list$exp[[ridx]]$std.err,
                             w.mat = pgmm_eq_list$exp[[ridx]]$w.mat,
                             inst.collaps = pgmm_eq_list$exp[[ridx]]$inst.collaps)

  print("Stock")
  model_stock = pdynmc::pdynmc(dat = data_subset, 
                               varname.i = "bn",
                               varname.t = "year",
                               use.mc.lev = FALSE,
                               use.mc.diff = TRUE,
                               use.mc.nonlin = FALSE,
                               
                               include.y = TRUE,
                               varname.y = pgmm_eq_list$stock[[ridx]]$varname.y,
                               lagTerms.y =  pgmm_eq_list$stock[[ridx]]$lagTerms.y,
                               maxLags.y = pgmm_eq_list$stock[[ridx]]$maxLags.y,
                               
                               include.x = TRUE,
                               varname.reg.end = pgmm_eq_list$stock[[ridx]]$varname.reg.end,
                               lagTerms.reg.end = pgmm_eq_list$stock[[ridx]]$lagTerms.reg.end,
                               maxLags.reg.end = pgmm_eq_list$stock[[ridx]]$maxLags.reg.end,

                               varname.reg.pre =  pgmm_eq_list$stock[[ridx]]$varname.reg.pre,
                               lagTerms.reg.pre = pgmm_eq_list$stock[[ridx]]$lagTerms.reg.pre,
                               maxLags.reg.pre = pgmm_eq_list$stock[[ridx]]$maxLags.reg.pre,

                               varname.reg.ex = pgmm_eq_list$stock[[ridx]]$varname.reg.ex,
                               lagTerms.reg.ex = pgmm_eq_list$stock[[ridx]]$lagTerms.reg.ex,
                               maxLags.reg.ex = pgmm_eq_list$stock[[ridx]]$maxLags.reg.ex,

                               estimation = pgmm_eq_list$stock[[ridx]]$estimation,
                               std.err = pgmm_eq_list$stock[[ridx]]$std.err,
                               w.mat = pgmm_eq_list$stock[[ridx]]$w.mat,
                               inst.collaps = pgmm_eq_list$stock[[ridx]]$inst.collaps)
  
  print("Revenue")
  model_revenue = pdynmc::pdynmc(dat = data_subset, 
                                 varname.i = "bn",
                                 varname.t = "year",
                                 use.mc.lev = FALSE,
                                 use.mc.diff = TRUE,
                                 use.mc.nonlin = FALSE,
                                 
                                 include.y = TRUE,
                                 varname.y = pgmm_eq_list$revenue[[ridx]]$varname.y,
                                 lagTerms.y =  pgmm_eq_list$revenue[[ridx]]$lagTerms.y,
                                 maxLags.y = 2,
                                 
                                 include.x = TRUE,
                                 varname.reg.end = pgmm_eq_list$revenue[[ridx]]$varname.reg.end,
                                 lagTerms.reg.end = pgmm_eq_list$revenue[[ridx]]$lagTerms.reg.end,
                                 
                                 varname.reg.pre =  pgmm_eq_list$revenue[[ridx]]$varname.reg.pre,
                                 lagTerms.reg.pre = pgmm_eq_list$revenue[[ridx]]$lagTerms.reg.pre,
                                 
                                 varname.reg.ex = pgmm_eq_list$revenue[[ridx]]$varname.reg.ex,
                                 lagTerms.reg.ex = pgmm_eq_list$revenue[[ridx]]$lagTerms.reg.ex,
                                 
                                 estimation = pgmm_eq_list$revenue[[ridx]]$estimation,
                                 std.err = pgmm_eq_list$revenue[[ridx]]$std.err,
                                 w.mat = pgmm_eq_list$revenue[[ridx]]$w.mat,
                                 inst.collaps = pgmm_eq_list$revenue[[ridx]]$inst.collaps)
  
  
  # Retrieve R2-like measure for output clearance
  # Overall r2 measure using residual sum of square and total sum of square
  model_summary_exp = summary(model_exp)
  model_summary_stock = summary(model_stock)
  model_summary_revenue = summary(model_revenue)
  
  #EXP model fitted and residuals.
  exp_fitted = unlist(
    lapply(model_exp$fitted.values$step2, function (x) {
     x = c(NA,x)
    })
  )
  exp_fitted = cbind(model_exp$data$dat.na$bn, model_exp$data$dat.na$year, model_exp$data$dat.na$revenue, exp_fitted)
  exp_fitted = pdata.frame(exp_fitted, index = c("V1", "V2"))
  exp_fitted$exp_actual_fdiff = diff(exp_fitted$V3)
  exp_fitted$exp_fitted_fdiff = diff(exp_fitted$exp_fitted)
  r2_exp = cor(exp_fitted$exp_fitted_fdiff, exp_fitted$exp_actual_fdiff, use = "complete.obs")^2
 
  #Stock model fitted and residuals.
  stock_fitted = unlist(
    lapply(model_stock$fitted.values$step2, function (x) {
      x = c(NA,x)
    })
  )
  stock_fitted = cbind(model_stock$data$dat.na$bn, model_stock$data$dat.na$year, model_stock$data$dat.na$revenue, stock_fitted)
  stock_fitted = pdata.frame(stock_fitted, index = c("V1", "V2"))
  stock_fitted$stock_actual_fdiff = diff(stock_fitted$V3)
  stock_fitted$stock_fitted_fdiff = diff(stock_fitted$stock_fitted)
  r2_stock = cor(stock_fitted$stock_fitted_fdiff, stock_fitted$stock_actual_fdiff, use = "complete.obs")^2
  
  #Revenue model fitted and residuals.
  revenue_fitted = unlist(
    lapply(model_revenue$fitted.values$step2, function (x) {
      x = c(NA,x)
    })
  )
  revenue_fitted = cbind(model_revenue$data$dat.na$bn, model_revenue$data$dat.na$year, model_revenue$data$dat.na$revenue, revenue_fitted)
  revenue_fitted = pdata.frame(revenue_fitted, index = c("V1", "V2"))
  revenue_fitted$revenue_actual_fdiff = diff(revenue_fitted$V3)
  revenue_fitted$revenue_fitted_fdiff = diff(revenue_fitted$revenue_fitted)
  r2_revenue = cor(revenue_fitted$revenue_fitted_fdiff, revenue_fitted$revenue_actual_fdiff, use = "complete.obs")^2
  

  # retrieve degrees of freedom for model (note this assumes collapse = TRUE)
  # exp
  obs_exp = model_summary_exp$data$Time * model_summary_exp$data$n
  n_inst_exp = model_summary_exp$data$n.inst
  coefs = nrow(model_summary_exp$coefficients)+1
  model_df_exp = obs_exp - n_inst_exp - coefs
  
  # stock
  obs_stock = model_summary_stock$data$Time * model_summary_stock$data$n
  n_inst_stock = model_summary_stock$data$n.inst
  coefs = nrow(model_summary_stock$coefficients)+1
  model_df_stock = obs_stock - n_inst_stock - coefs
  
  # revenue
  obs_revenue = model_summary_revenue$data$Time * model_summary_revenue$data$n
  n_inst_revenue = model_summary_revenue$data$n.inst
  coefs = nrow(model_summary_revenue$coefficients)+1
  model_df_revenue = obs_revenue - n_inst_revenue - coefs
  
  #Save results
  # Exp
  model_summary_exp_output = as.data.frame(model_summary_exp$coefficients)
  model_summary_exp_output$obs = obs_exp
  model_summary_exp_output$df = model_df_exp 
  model_summary_exp_output$pseudor2 = r2_exp
  model_summary_exp_output$ar1stat = mtest.fct(model_exp, order=1)$statistic[1]
  model_summary_exp_output$ar1pval = mtest.fct(model_exp, order=1)$p.value[1]
  model_summary_exp_output$ar2stat = mtest.fct(model_exp, order=2)$statistic[1]
  model_summary_exp_output$ar2pval = mtest.fct(model_exp, order=2)$p.value[1]
  model_summary_exp_output$waldstat = model_summary_exp$slopef$statistic[1]
  model_summary_exp_output$waldpval = model_summary_exp$slopef$p.value[1]
  model_summary_exp_output$sarganstat = model_summary_exp$hansenj$statistic[1]
  model_summary_exp_output$sarganpval = model_summary_exp$hansenj$p.value[1]
  write.csv(model_summary_exp_output, file = file.path(results_path,"secondstage", paste(ridx, "PGMM_exp_model_results.csv", sep = "_")))
  
  #Stock
  model_summary_stock_output = as.data.frame(model_summary_stock$coefficients)
  model_summary_stock_output$obs = obs_stock
  model_summary_stock_output$df = model_df_stock 
  model_summary_stock_output$pseudor2 = r2_stock
  model_summary_stock_output$ar1stat = mtest.fct(model_stock, order=1)$statistic[1]
  model_summary_stock_output$ar1pval = mtest.fct(model_stock, order=1)$p.value[1]
  model_summary_stock_output$ar2stat = mtest.fct(model_stock, order=2)$statistic[1]
  model_summary_stock_output$ar2pval = mtest.fct(model_stock, order=2)$p.value[1]
  model_summary_stock_output$waldstat = model_summary_stock$slopef$statistic[1]
  model_summary_stock_output$waldpval = model_summary_stock$slopef$p.value[1]
  model_summary_stock_output$sarganstat = model_summary_stock$hansenj$statistic[1]
  model_summary_stock_output$sarganpval = model_summary_stock$hansenj$p.value[1]
  write.csv(model_summary_stock_output, file = file.path(results_path,"secondstage", paste(ridx, "PGMM_stock_model_results.csv", sep = "_")))
  
  #revenue
  model_summary_revenue_output = as.data.frame(model_summary_revenue$coefficients)
  model_summary_revenue_output$obs = obs_revenue
  model_summary_revenue_output$df = model_df_revenue 
  model_summary_revenue_output$pseudor2 = r2_revenue
  model_summary_revenue_output$ar1stat = mtest.fct(model_revenue, order=1)$statistic[1]
  model_summary_revenue_output$ar1pval = mtest.fct(model_revenue, order=1)$p.value[1]
  model_summary_revenue_output$ar2stat = mtest.fct(model_revenue, order=2)$statistic[1]
  model_summary_revenue_output$ar2pval = mtest.fct(model_revenue, order=2)$p.value[1]
  model_summary_revenue_output$waldstat = model_summary_revenue$slopef$statistic[1]
  model_summary_revenue_output$waldpval = model_summary_revenue$slopef$p.value[1]
  model_summary_revenue_output$sarganstat = model_summary_revenue$hansenj$statistic[1]
  model_summary_revenue_output$sarganpval = model_summary_revenue$hansenj$p.value[1]
  write.csv(model_summary_revenue_output, file = file.path(results_path,"secondstage", paste(ridx, "PGMM_revenue_model_results.csv", sep = "_")))
  
}