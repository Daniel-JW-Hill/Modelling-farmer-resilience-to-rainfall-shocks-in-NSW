
#This function deflates all series by PPI to remove exogenous trend in input prices. 

deflate_series = function(data,
                          freq = 'annual',
                          vars = NULL){
  
  PPI = read.csv(file.path(root_wd, data_path, "PPI_series.csv"))
  
  if (freq == "quarterly"){
    unique_years = unique(data$tsid) 
    unique_quarters = unique(data$quarter)
    
    for (y in 1:length(unique_years)){
      y_idx = unique_years[y]+2000
      for (q in 1:length(unique_quarters)){
        q_idx = unique_quarters[q]
        deflation_index = PPI$PPI[which(PPI$Year == y_idx & PPI$Quarter == q_idx)]/100
        row_idx = which(data$tsid == (y_idx-2000) & data$quarter == q_idx)
        data[row_idx,vars] = data[row_idx,vars] / deflation_index
      }
    }
  } else {
    unique_years = unique(data$tsid)
    
    for (y in 1:length(unique_years)){
      y_idx = unique_years[y]+2000
      deflation_index = mean(PPI$PPI[which(PPI$Year == y_idx)]/100, na.rm = TRUE)
      row_idx = which(data$tsid == (y_idx-2000))
      data[row_idx,vars] = data[row_idx,vars] / deflation_index
    }
  }
  return(data)
}

# End of function


