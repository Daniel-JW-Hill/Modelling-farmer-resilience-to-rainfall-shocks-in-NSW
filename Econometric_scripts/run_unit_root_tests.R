
# Runs panel unit root tests and saves outputs
# with tests to check outputs

run_unit_root_tests = function(data, var, name){
  
  data = pdata.frame(data, index = c("bn", "tsid"))
  
  unit_root = purtest(data[,var], test = "madwu" , exo = "trend", lags = 1)
  
  # retrieve stats, ignoring INF values for short panels. 
  # follows methods from purtest Git for Madwu test. 
  
  stat = p_values = c() 
  for (i in 1:length(unit_root$idres)){
    stat = c(stat, as.numeric(unit_root$idres[[i]]$trho))
    p_values = c(p_values, unit_root$idres[[i]]$p.trho)
  }
  
  not_inf = which(is.finite(stat))
  stat = stat[not_inf]
  p_values = p_values[not_inf]
  p_values  = p_values[p_values !=  0]
  
  mean_stat = -2*sum(log(p_values))
  sample_size = length(stat)
  pval = pchisq(mean_stat, df = sample_size*2, lower.tail = FALSE) # one sided test
  
  #dominance tests. 
  stat = sort(abs(stat), decreasing = TRUE)
  max_unit_root = abs(stat[1])
  second_max_unit_root = abs(stat[2])
  sum_unit_root = sum(stat, na.rm = TRUE)
  
  unit_root_df = data.frame(  "name" = name, 
                              "alternative" = unit_root$statistic$alternative,
                              "method" = unit_root$statistic$method,
                              "statistic" = mean_stat ,
                              "p.val" = pval,
                              "sample_size" = sample_size,
                              "dominance_max" =  max_unit_root / sum_unit_root,
                              "dominance_2ndmax" =  sum(max_unit_root, second_max_unit_root) / sum_unit_root 
  )

  return(unit_root_df)
  
}
