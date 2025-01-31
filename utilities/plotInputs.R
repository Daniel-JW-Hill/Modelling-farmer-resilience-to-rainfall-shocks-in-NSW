
# plots and returns change in inputs

plotInputs = function(data_inputs, 
                      input_display, 
                      ymin_Input, 
                      ymax_Input,
                      show_confidence_intervals_input){
  
  # check which options are selected for plotting
  # and subset accordingly
  labels_plot = c("Change in expenditure", "Change in stock value")
  colour_values = c("steelblue", "darkorange")
  
  if (!("Changes in expenditure ($AUD/ha)" %in% input_display)) {
    data_inputs = data_inputs[-which(data_inputs$Type == "ExpOutcomes"), ]
    labels_plot = labels_plot[-which(labels_plot == "Change in expenditure")]
    colour_values = colour_values[-1]
  }
  
  if (!("Changes in stock value ($AUD/ha)" %in% input_display)) {
    data_inputs = data_inputs[-which(data_inputs$Type == "StockOutcomes"), ]
    labels_plot = labels_plot[-which(labels_plot == "Change in stock value")]
    colour_values = colour_values[-2]
  }
  
  # determine y min and ymax
  limits = c(0, 200)
  if (nzchar(ymin_Input)) {
    limits[1] = as.numeric(ymin_Input)
  }
  
  if (nzchar(ymin_Input)) {
    limits[2] = as.numeric(ymax_Input)
  }
  
  # remove confidence intervals if turned off
  if (show_confidence_intervals_input == FALSE) {
    data_inputs$Lower = data_inputs$Central
    data_inputs$Upper = data_inputs$Central
  }
  
  #Plot chart
  plot = ggplot(data_inputs, aes(
                  x = Year,
                  y = Central,
                  fill = Type,
                  color = Type
                )) +
                  geom_ribbon(aes(ymin = Lower, ymax = Upper),
                              alpha = 0.2,
                              color = NA) +
                  geom_line() +
                  ggtitle("Change in input value ($AUD/ha)", subtitle = "Relative to 'normal' rainfall conditions") +
                  xlab("Year") +
                  ylab("Change in input levels\n (relative to 'normal' rainfall conditions indexed at 100)") +
                  scale_fill_manual(labels = labels_plot, values = colour_values, ) +
                  scale_color_manual(labels = labels_plot, values = colour_values, ) +
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
                    legend.position = "bottom")
  
  return(plot)
  
}