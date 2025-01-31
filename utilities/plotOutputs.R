


plotOutputs = function(data_outputs,
                       output_display,
                       ymin_Output,
                       ymax_Output, 
                       show_confidence_intervals_output ){
  
  # check which options are selected for plotting
  # and subset accordingly
  labels_plot = c(
    "Change directly attributable to rainfall",
    "Change mediated by expenditure changes",
    "Change mediated by stock value changes",
    "Net Change"
  )
  colour_values = c("steelblue", "darkorange", "forestgreen", "#DC143C")
  
  if (!("Change directly attributable to rainfall" %in% output_display)) {
    data_outputs = data_outputs[-which(data_outputs$Type == "Direct"), ]
    labels_plot = labels_plot[-which(labels_plot == "Change directly attributable to rainfall")]
    colour_values = colour_values[-1]
  }
  
  if (!("Change mediated by expenditure changes" %in% output_display)) {
    data_outputs = data_outputs[-which(data_outputs$Type == "Exp"), ]
    labels_plot = labels_plot[-which(labels_plot == "Change mediated by expenditure changes")]
    colour_values = colour_values[-2]
  }
  
  if (!("Change mediated by stock value changes" %in% output_display)) {
    data_outputs = data_outputs[-which(data_outputs$Type == "Stock"), ]
    labels_plot = labels_plot[-which(labels_plot == "Change mediated by stock value changes")]
    colour_values = colour_values[-3]
  }
  
  if (!("Net Change" %in% output_display)) {
    data_outputs = data_outputs[-which(data_outputs$Type == "Total"), ]
    labels_plot = labels_plot[-which(labels_plot == "Net Change")]
    colour_values = colour_values[-4]
  }
  
  
  # determine y min and ymax
  limits = c(0, 200)
  if (nzchar(ymin_Output)) {
    limits[1] = as.numeric(ymin_Output)
  }
  if (nzchar(ymin_Output)) {
    limits[2] = as.numeric(ymax_Output)
  }
  
  # remove confidence intervals if turned off
  if (show_confidence_intervals_output == FALSE) {
    data_outputs$Lower = data_outputs$Central
    data_outputs$Upper = data_outputs$Central
  }
  
  #Plot chart
  plot = ggplot(data_outputs,
           aes(
             x = Year,
             y = Central,
             fill = Type,
             color = Type
           )) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper),
                  alpha = 0.2,
                  color = NA) +
      geom_line() +
      ggtitle("Change in gross returns ($AUD/ha)", subtitle = "Relative to 'normal' rainfall conditions") +
      xlab("Year") +
      ylab("Change\n (relative to 'normal' rainfall conditions indexed at 100)") +
      scale_fill_manual(labels = labels_plot, values = colour_values) +
      scale_color_manual(labels = labels_plot, values = colour_values) +
      scale_y_continuous(limits = limits) +
      scale_x_continuous(breaks = seq(1, 10, 1)) +
      geom_hline(yintercept = 100, linetype = 'dashed') +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "bottom"
      ) +
      guides(fill = guide_legend(ncol = 1), 
             color = guide_legend(ncol = 1)) 
  
  return(plot)
  
  
}