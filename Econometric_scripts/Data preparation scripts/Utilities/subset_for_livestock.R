#subsets for livestock anzsic codes

subset_for_livestock = function(data,
                                save_name = "no name"){
  
  bn_unique = unique(data$bn)
  bn_to_omit = c()
  
  for (bn in 1:length(unique(bn_unique))){
    subset_anzsic = data$latest_anzsic[which(data$bn == bn_unique[bn])]
    if (!(all(subset_anzsic %in% c(0141, 0142, 0143, 0144)))) {
      bn_to_omit = c(bn_to_omit, bn_unique[bn]) 
    }
    
  }
    
  data = data[!(data$bn %in% bn_to_omit),]
  
  return(data)
      
}

  
