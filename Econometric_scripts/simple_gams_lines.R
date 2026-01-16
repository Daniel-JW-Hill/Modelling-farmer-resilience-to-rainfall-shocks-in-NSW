
simple_gams_lines = function(model, data_GAMs, r, range_SPI) {

  all_vars = all.vars(model$formula)
  SPI_vars =  all_vars[startsWith( all_vars,"SPI")]

  for (SPI in 1:length(SPI_vars)){
    SPI_idx = SPI_vars[SPI]
    plot_data  = data.frame()
    new_data = data.frame(id = 1:1000)
    
    for (v in 1:length(all_vars)){
      if (all_vars[v] == SPI_idx){
        var = seq(from = range_SPI[1], to = range_SPI[2], length.out = 1000)
      } else {
        var = rep(mean(model$model[,all_vars[v]]), 1000)
      }
      new_data[,all_vars[v]] = var
    }
    
    # set it to the omitted region if NSW
    new_data$year = 11 
    if ("STablelands" %in% all_vars){
      new_data$STablelands = 0
    } 
    if ("NTablelands" %in% all_vars){
      new_data$NTablelands = 0
    } 
    if ("CWest" %in% all_vars){
      new_data$CWest = 0
    } 
    if ("FarWest" %in% all_vars){
      new_data$FarWest = 0
    }
    if ("Murray" %in% all_vars){
      new_data$Murray = 0
    }
  
    predictions = predict(model, newdata = new_data, type = 'response', se.fit = TRUE)
    new_data$predictions = predictions$fit
    new_data$se = predictions$se.fit
    conf_int = 0.95
    z = qnorm(1-(1-conf_int)/2)
    new_data$lower = new_data$predictions  - new_data$se
    new_data$upper = new_data$predictions  + new_data$se
    
    ymin = min(new_data$lower)-0.01
    ymax = max(new_data$upper)+0.01
    ymin = ifelse(ymin >= 0, -0.01 , ymin)
    ymax = ifelse(ymax <= 0, +0.01, ymax)
    
    plot = ggplot(new_data, aes(x = .data[[SPI_idx]], y = predictions))+
      geom_hline(yintercept = 0, color = 'black') +
      geom_vline(xintercept = 0, color = 'black') +
      geom_line(linewidth = 1) + 
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha =  0.2, color = NA)+
      scale_y_continuous(breaks = seq(floor(ymin/0.05)*0.05, ceiling(ymax/0.05)*0.05, by = 0.05)) +
      theme_minimal() +
      labs(title = paste("Predicted values", r, sep = " - "), y = "Predicted Values (log)") +
      theme(axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            axis.text = element_text(size = 14))
   
    ggsave(file.path(results_path, "GAMmodels", "Figures", r,  paste(r, SPI_idx, "simplelinechart.png", sep = "_")), plot = plot, dpi = 300, height = 6, width = 8, units = "in")
    write.csv(new_data, file = file.path(results_path, "GAMmodels", "Figures", r,  paste(r, SPI_idx, "simplelinechart_data.csv",sep = "_")))
  }
  

}


