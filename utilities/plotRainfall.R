
plotRainfall = function(data_summary) {
  
  data_outputs = data.frame(Year = seq(1, 10, 1),
                            SPI_index = data_summary[,1], 
                            group = rep("SPI_index", 10))
  
  labels_plot = c("Rainfall Index (SPI)")
  colour_values = c("steelblue")
  limits = c(-1.2, 1.4)
  
  #Plot chart
  plot = ggplot(data_outputs, aes(x = Year, y = SPI_index, color = group)) +
          geom_line() +
          ggtitle("Rainfall index (SPI) scenario over time ", subtitle = "Normalised to mean rainfall conditions") +
          xlab("Year") +
          ylab("Normalised rainfall index (SPI)") +
          scale_color_manual(labels = labels_plot, values = colour_values) +
          scale_y_continuous(limits = limits) +
          scale_x_continuous(breaks = seq(1, 10, 1)) +
          geom_hline(yintercept = 0, linetype = 'dashed') +
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

