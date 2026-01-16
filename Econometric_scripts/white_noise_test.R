
# White noise test on GAMs residuals

white_noise_tests = function(model, r){
  
  test = car::durbinWatsonTest(model$residuals)
  
  # save results and clearance checks. 
  results = data.frame("stat" = test,
                       "method" = "Durbin Watson Test",
                       "alternative" = "Two-sided",
                       "observations" = length(model$residuals),
                       "degrees_freedom" = model$df.residual,
                       "r2" = summary(model)$r.sq)
  write.csv(results, file = file.path(results_path, "GAMmodels", paste(r,"white_noise_test.csv",sep = "_")))
  
}