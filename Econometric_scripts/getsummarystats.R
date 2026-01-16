
# Prints summary stats by region and tests for export. 

getsummarystats = function(data, unique_regions){
  
  vars = c("exp_total_perha", 
           "gross_returns_perha", 
           "assets_less_stock_perha", 
           "stock_opening_perha", 
           "SPI_annual")
  
  data = data[which(data$analysis_regions %in% unique_regions),]
  
  for(v in vars){
    
    # Tables of mean, standard error, median, and sample sizes. 
    means = data.frame(matrix(NA, nrow = length(unique(data$analysis_regions)) + 1, ncol = length(unique(data$year))))
    colnames(means) = sort(unique(data$year))
    rownames(means) = c(unique(data$analysis_regions), "NSW")
    sample_sizes = standard_error = median = median_deviation = dominance_test_largest = dominance_test_2largest = means
    
    # Loop through years and analysis regions and save information
    for (y in 1:ncol(means)){
      year = colnames(means)[y]
      for (r in 1:nrow(means)-1){
        region = rownames(means)[r]
        data_subset = data[which(data$year == year & data$analysis_regions == region),]
        sample_sizes[r,y] = nrow(data_subset)
        means[r,y] = mean(data_subset[,v], na.rm = TRUE)
        median[r,y] = median(data_subset[,v], na.rm = TRUE)
        standard_error[r,y] = sd(data_subset[,v], na.rm = TRUE)/ sqrt(sample_sizes[r,y])
        median_deviation[r,y] = mad(data_subset[,v], na.rm = TRUE)
        sorted = as.vector(sort(data_subset[,v],decreasing = TRUE))
        dominance_test_largest[r,y] = abs(sorted[1])/sum(abs(sorted), na.rm = TRUE)
        dominance_test_2largest[r,y] = abs(sorted[1]+ sorted[2])/sum(abs(sorted), na.rm = TRUE)
      }
      
      #Repeat for NSW
      data_subset = data[which(data$year == year),]
      sample_sizes[nrow(sample_sizes),y] = nrow(data_subset)
      means[nrow(means),y] = mean(data_subset[,v], na.rm = TRUE)
      median[nrow(median),y] = median(data_subset[,v], na.rm = TRUE)
      standard_error[nrow(standard_error),y] = sd(data_subset[,v], na.rm = TRUE)/ sqrt(sample_sizes[nrow(sample_sizes),y])
      median_deviation[nrow(median_deviation),y] = mad(data_subset[,v], na.rm = TRUE)
      sorted = as.vector(sort(data_subset[,v],decreasing = TRUE))
      dominance_test_largest[nrow(dominance_test_largest),y] = abs(sorted[1])/sum(abs(sorted), na.rm = TRUE)
      dominance_test_2largest[nrow(dominance_test_2largest),y] = abs(sorted[1]+ sorted[2])/sum(abs(sorted), na.rm = TRUE)
    }
    
    # Save versions
    write.csv(means, file = file.path(results_path, "summary_stats",  v, "means.csv"))
    write.csv(median, file = file.path(results_path, "summary_stats",  v, "median.csv"))
    write.csv(standard_error, file = file.path(results_path, "summary_stats",  v, "standard_error.csv"))
    write.csv(median_deviation, file = file.path(results_path, "summary_stats",  v, "median_deviation.csv"))
    write.csv(dominance_test_largest, file = file.path(results_path, "summary_stats",  v, "dominance_test_largest.csv"))
    write.csv(dominance_test_2largest, file = file.path(results_path, "summary_stats",  v, "dominance_test_2largest.csv"))
    write.csv(sample_sizes, file = file.path(results_path, "summary_stats",  v, "sample_sizes,.csv"))
    
  }
}
