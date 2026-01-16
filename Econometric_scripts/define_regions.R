 
# This function confirms the region definition for each farm business. 
# Note SA4 regions are adapted to exclude coastal regions which are outside the scope of this analysis. 

define_regions = function(data){
  
  data$analysis_regions = NA
  
  # Murray and riverina regions. 
  data$analysis_regions[which(data$SA4_region == 109)] = "Murray_Riverina" # Murray region
  data$analysis_regions[which(data$SA4_region == 113)] = "Murray_Riverina" # Riverina
  data$analysis_regions[which(data$SA3_region == 10106)] = "Murray_Riverina" # Young-Yass
  
  # Central west SA4 region, minus Bathurst
  data$analysis_regions[which(data$SA4_region == 103)] = "Central_West" # Central West SA4 region
  
  # North coast =Richmond, Tweed, Coffs and Grafton regions sa4 regions
  data$analysis_regions[which(data$SA4_region == 112)] = "North_coast" # Richmond Tweed
  data$analysis_regions[which(data$SA4_region == 104)] = "North_coast" # Coffs-Grafton
  
  # Mid north coast sa4 region. 
  data$analysis_regions[which(data$SA4_region == 108)] = "Mid_North_coast" # Mid North coast
  
  #Northern Tablelands, including Upper Hunter region from Hunter Valley SA4. Minus Moree which goes in Far West
  data$analysis_regions[which(data$SA4_region == 110)] = "Northern_Tablelands" # New England
  data$analysis_regions[which(data$SA3_region == 10604)] = "Northern_Tablelands" # Upper Hunter

  # Far west SA4 region
  data$analysis_regions[which(data$SA4_region == 105)] = "Far_West" # Far west SA4 region
  data$analysis_regions[which(data$SA3_region == 11003)] = "Far_West" #Moree
  
  # Capital and Southern tablelands - composite of non-coastal sa3 regions from capital SA4 region
  # and southern highlands and Bathhurst
  data$analysis_regions[which(data$SA3_region == 10102)] = "Southern_Tablelands" # Queanbeyan
  data$analysis_regions[which(data$SA3_region == 10103)] = "Southern_Tablelands" # Snowy mountains
  data$analysis_regions[which(data$SA3_region == 10105)] = "Southern_Tablelands" # Goulburn
  data$analysis_regions[which(data$SA3_region == 11402)] = "Southern_Tablelands" # Southern highlands
  data$analysis_regions[which(data$SA3_region == 10301)] = "Southern_Tablelands" # Bathurst
  
  # Coastal - non metro coastal SA3 regions. 
  data$analysis_regions[which(data$SA3_region == 11401)] = "Coastal" # Shoalhaven
  data$analysis_regions[which(data$SA3_region == 10104)] = "Coastal" # South Coast
  data$analysis_regions[which(data$SA4_region == 111)] = "Coastal"  # Newcastle and Lake Macquarie 
  data$analysis_regions[which(data$SA4_region == 107)] = "Coastal" # Illawarra
  data$analysis_regions[which(data$SA4_region == 115)] = "Coastal" # Hawksebury
  data$analysis_regions[which(data$SA3_region == 10601)] = "Coastal" # Lower Hunter
  data$analysis_regions[which(data$SA3_region == 10602)] = "Coastal" # Maitland
  data$analysis_regions[which(data$SA3_region == 10603)] = "Coastal" #Port Stephens
  
  # Drop all bn which change between these regions. 
  unique_bn = unique(data$bn)
  bn_to_drop = c()
  for (bn_idx in unique_bn){
    sa3_regions = unique(data$analysis_regions[which(data$bn == bn_idx)])
    if(length(sa3_regions)>1){
      bn_to_drop = c(bn_to_drop, bn_idx)
    }
  }
  data = data[-which(data$bn %in% bn_to_drop),]

  return(data)
}