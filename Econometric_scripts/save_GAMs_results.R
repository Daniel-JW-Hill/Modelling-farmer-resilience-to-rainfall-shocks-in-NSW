
#Saves simple GAMs model results

save_GAMs_results = function(model, name){
  results = flextable::as_flextable(model)
  results = flextable::autofit(results)

  flextable::save_as_docx(results, pr_section = prop_section(page_size = page_size(orient = "landscape")), path = file.path("Results", 'GAMmodels', paste(name, "docx", sep = ".")))
  
  #save other measures in df
  kcheck = mgcv::k.check(model)
  result_measures = data.frame("k_allowance" = kcheck[1],
                               "ecdf" = kcheck[2],
                               "residual_degreesFreedom" = summary(model)$residual.df)
  write.csv(result_measures, file = file.path("Results", "GAMmodels", paste(name, "csv", sep = ".")))
  
}

# End of function
