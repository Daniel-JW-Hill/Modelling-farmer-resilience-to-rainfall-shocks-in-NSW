
plotRainfall = function(data_summary) {
  
  x_vals = seq(-4, 1, 1)  # keep just the years chosen
  yrs = length(x_vals) - 1
  spi_values = data_summary[1:(yrs+1), 1]
  
  # Create full data frame
  data_outputs = data.frame(
    Year = x_vals,
    SPI_index = spi_values
  )
  
  data_obs = data_outputs[data_outputs$Year <= 0, ]
  data_forecast = data_outputs[data_outputs$Year >= 0, ]  # expectations are now a dotted line
  
  # Plot chart
  plot = ggplot() +
    geom_line(data = data_obs, aes(x = Year, y = SPI_index), size = 1.2, color = "steelblue", linetype = "solid") +
    geom_line(data = data_forecast, aes(x = Year, y = SPI_index), size = 1.2, color = "steelblue", linetype = "dotted") +
    ggtitle("Rainfall index (SPI) scenario over time", 
            subtitle = "Normalised to mean rainfall conditions") +
    labs(caption = "Rainfall in years prior and after are assumed to be at their long-run annual mean (SPI = 0)") +
    xlab("Year relative to current") +
    ylab("Standardised Precipitation Index for annual rainfall (SPI)") +
    scale_y_continuous(limits = c(-1.2, 1.4)) +
    scale_x_continuous(breaks = x_vals) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.position = "none"
    )
  
  
  return(plot)
  
}

