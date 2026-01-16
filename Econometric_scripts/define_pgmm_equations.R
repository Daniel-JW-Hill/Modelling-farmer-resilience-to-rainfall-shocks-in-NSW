
# Defines individual Arellano Bond GMM equations for each region and NSW
# The final versions of these are judged best based on Sargan and autocorrelation tests. 

define_pgmm_equations = function(){
  
  eqs_exp =  eqs_stock =  eqs_revenue  = list()
  
  # NSW
  eqs_revenue[["NSW"]] = list(varname.y = "revenue",
                              lagTerms.y = 1,
                              maxLags.y = 3,
                              
                              varname.reg.end = c("exp", "stock","assets"),
                              lagTerms.reg.end = c(0,0,0),
                              maxLags.reg.end = NULL,
                              
                              varname.reg.pre =  NULL,
                              lagTerms.reg.pre = NULL,
                              maxLags.reg.pre = NULL,
                              
                              varname.reg.ex = c("weather_index", "year", "termsoftrade_index"),
                              lagTerms.reg.ex = c(0,0,0),
                              maxLags.reg.ex = NULL,
                              
                              estimation = "twostep",
                              std.err = "corrected",
                              w.mat = "iid.err",
                              inst.collaps = TRUE)
  
  eqs_exp[["NSW"]] = list(varname.y = "exp",
                          lagTerms.y = 1,
                          maxLags.y = 3,
                          
                          varname.reg.end = c("stock"),
                          lagTerms.reg.end = c(0),
                          maxLags.reg.end = NULL,
                          
                          varname.reg.pre =  c("revenue_lag",  "assets"),
                          lagTerms.reg.pre = c(0,0),
                          maxLags.reg.pre = NULL,
                          
                          varname.reg.ex = c("weather_index_lag", "year", "termsoftrade_index"),
                          lagTerms.reg.ex = c(0,0,0),
                          maxLags.reg.ex = NULL,
                          
                          estimation = "twostep",
                          std.err = "corrected",
                          w.mat = "iid.err",
                          inst.collaps = TRUE)
  
  
  eqs_stock[["NSW"]] = list(varname.y = "stock",
                            lagTerms.y = 1,
                            maxLags.y = 3,
                            
                            varname.reg.end = c("assets"),
                            lagTerms.reg.end = c(0),
                            maxLags.reg.end = NULL,
                            
                            varname.reg.pre =  c("revenue_lag", "exp_lag"),
                            lagTerms.reg.pre = c(0,0),
                            maxLags.reg.pre = NULL,
                            
                            varname.reg.ex = c("weather_index_lag", "year", "termsoftrade_index"),
                            lagTerms.reg.ex = c(0,0,0),
                            maxLags.reg.ex =  NULL,
                            
                            estimation = "twostep",
                            std.err = "corrected",
                            w.mat = "iid.err",
                            inst.collaps = TRUE)
  
  
  
  # Northern Tablelands
  eqs_revenue[["Northern_Tablelands"]] = list(varname.y = "revenue",
                                              lagTerms.y = 1,
                                              maxLags.y = 3,
                                              
                                              varname.reg.end = c("exp", "stock","assets"),
                                              lagTerms.reg.end = c(0,0,0),
                                              maxLags.reg.end = NULL,
                                              
                                              varname.reg.pre =  NULL,
                                              lagTerms.reg.pre = NULL,
                                              maxLags.reg.pre = NULL,
                                              
                                              varname.reg.ex = c("weather_index", "year", "termsoftrade_index"),
                                              lagTerms.reg.ex = c(0,0,0),
                                              maxLags.reg.ex = NULL,
                                              
                                              estimation = "twostep",
                                              std.err = "corrected",
                                              w.mat = "iid.err",
                                              inst.collaps = TRUE)
  
  
  eqs_exp[["Northern_Tablelands"]] = list(varname.y = "exp",
                                          lagTerms.y = 1,
                                          maxLags.y = 5,
                                          
                                          varname.reg.end = c("stock"),
                                          lagTerms.reg.end = c(0),
                                          maxLags.reg.end = NULL,
                                          
                                          varname.reg.pre =  c("revenue_lag", "assets"),
                                          lagTerms.reg.pre = c(0,0),
                                          maxLags.reg.pre = NULL,
                                          
                                          varname.reg.ex = c("weather_index_lag", "year", "termsoftrade_index"),
                                          lagTerms.reg.ex = c(0,0,0),
                                          maxLags.reg.ex = NULL,
                                          
                                          estimation = "twostep",
                                          std.err = "corrected",
                                          w.mat = "identity",
                                          inst.collaps = TRUE)
  
  
  eqs_stock[["Northern_Tablelands"]] = list(varname.y = "stock",
                                            lagTerms.y = 1,
                                            maxLags.y = 3,
                                            
                                            varname.reg.end = c("assets"),
                                            lagTerms.reg.end = c(0),
                                            maxLags.reg.end = c(5),
                                            
                                            varname.reg.pre =  c("revenue_lag", "exp_lag"),
                                            lagTerms.reg.pre = c(0,0),
                                            maxLags.reg.pre = NULL,
                                            
                                            varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                                            lagTerms.reg.ex = c(0,0,0),
                                            maxLags.reg.ex = NULL,
                                            
                                            estimation = "twostep",
                                            std.err = "corrected",
                                            w.mat = "identity",
                                            inst.collaps = TRUE)
  
  
  
  # Central West
  eqs_revenue[["Central_West"]] = list(varname.y = "revenue",
                                      lagTerms.y = 1,
                                      maxLags.y = 3,
                                      
                                      varname.reg.end = c("exp", "stock", "assets"),
                                      lagTerms.reg.end = c(0,0,0),
                                      maxLags.reg.end = NULL,
                                      
                                      varname.reg.pre =  NULL,
                                      lagTerms.reg.pre = NULL,
                                      maxLags.reg.pre = NULL,
                                      
                                      varname.reg.ex = c("weather_index", "year", "termsoftrade_index"),
                                      lagTerms.reg.ex = c(0,0,0),
                                      maxLags.reg.ex = NULL,
                                     
                                       estimation = "twostep",
                                       std.err = "corrected",
                                       w.mat = "iid.err",
                                       inst.collaps = TRUE)
  
  eqs_exp[["Central_West"]] = list(varname.y = "exp",
                                    lagTerms.y = 1,
                                    maxLags.y = 3,
                                    
                                    varname.reg.end = c("stock"),
                                    lagTerms.reg.end = c(0),
                                    maxLags.reg.end = NULL,
                                    
                                    varname.reg.pre =  c("revenue_lag",  "assets"),
                                    lagTerms.reg.pre = c(0,0),
                                    maxLags.reg.pre = NULL,
                                    
                                    varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                                    lagTerms.reg.ex = c(0,0,0),
                                    maxLags.reg.ex = NULL,
                             
                                   estimation = "twostep",
                                   std.err = "corrected",
                                   w.mat = "iid.err",
                                   inst.collaps = TRUE)
  
  
  eqs_stock[["Central_West"]] = list(varname.y = "stock",
                                    lagTerms.y = 1,
                                    maxLags.y = 3,
                                    
                                    varname.reg.end = c("assets"),
                                    lagTerms.reg.end = c(0),
                                    maxLags.reg.end = NULL,
                                    
                                    varname.reg.pre =  c("revenue_lag", "exp_lag"),
                                    lagTerms.reg.pre = c(0,0),
                                    maxLags.reg.pre = NULL,
                                    
                                    varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                                    lagTerms.reg.ex = c(0,0,0),
                                    maxLags.reg.ex = NULL,
                                    
                                    estimation = "twostep",
                                    std.err = "corrected",
                                    w.mat = "iid.err",
                                    inst.collaps = TRUE)
  
  
  # Far West
  eqs_revenue[["Far_West"]] = list(varname.y = "revenue",
                                   lagTerms.y = 1,
                                   maxLags.y = 3,
                                   
                                   varname.reg.end = c("exp", "stock", "assets"),
                                   lagTerms.reg.end = c(0,0,0),
                                   maxLags.reg.end = NULL,
                                   
                                   varname.reg.pre =  NULL,
                                   lagTerms.reg.pre = NULL,
                                   maxLags.reg.pre = NULL,
                                   
                                   varname.reg.ex = c("weather_index", "year", "termsoftrade_index"),
                                   lagTerms.reg.ex = c(0,0,0),
                                   maxLags.reg.ex = NULL,
                                   
                                   estimation = "twostep",
                                   std.err = "corrected",
                                   w.mat = "iid.err",
                                   inst.collaps = TRUE)
  
  eqs_exp[["Far_West"]] = list(varname.y = "exp",
                               lagTerms.y = 1,
                               maxLags.y = 3,
                               
                               varname.reg.end = c("stock"),
                               lagTerms.reg.end = c(0),
                               maxLags.reg.end = NULL,
                               
                               varname.reg.pre =  c("revenue_lag",  "assets"),
                               lagTerms.reg.pre = c(0,0),
                               maxLags.reg.pre = NULL,
                               
                               varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                               lagTerms.reg.ex = c(0,0,0),
                               maxLags.reg.ex = NULL,
                           
                               estimation = "twostep",
                               std.err = "corrected",
                               w.mat = "identity",
                               inst.collaps = TRUE)
  
  
  eqs_stock[["Far_West"]] = list(varname.y = "stock",
                                 lagTerms.y = 1,
                                 maxLags.y = 3,
                                 
                                 varname.reg.end = c("assets"),
                                 lagTerms.reg.end = c(0),
                                 maxLags.reg.end = NULL,
                                 
                                 varname.reg.pre =  c("revenue_lag", "exp_lag"),
                                 lagTerms.reg.pre = c(0,0),
                                 maxLags.reg.pre = NULL,
                                 
                                 varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                                 lagTerms.reg.ex = c(0,0,0),
                                 maxLags.reg.ex = NULL,
                                 
                                 estimation = "twostep",
                                 std.err = "corrected",
                                 w.mat = "iid.err",
                                 inst.collaps = TRUE)
  
  
  # Murray Riverina
  eqs_revenue[["Murray_Riverina"]] = list(varname.y = "revenue",
                                           lagTerms.y = 1,
                                           maxLags.y = 3,
                                           
                                           varname.reg.end = c("exp", "stock", "assets"),
                                           lagTerms.reg.end = c(0,0,0),
                                           maxLags.reg.end = NULL,
                                           
                                           varname.reg.pre =  NULL,
                                           lagTerms.reg.pre = NULL,
                                           maxLags.reg.pre = NULL,
                                           
                                           varname.reg.ex = c("weather_index", "year", "termsoftrade_index"),
                                           lagTerms.reg.ex = c(0,0,0),
                                           maxLags.reg.ex = NULL,
                                           
                                           estimation = "twostep",
                                           std.err = "corrected",
                                           w.mat = "iid.err",
                                           inst.collaps = TRUE)
  
  eqs_exp[["Murray_Riverina"]] = list(varname.y = "exp",
                               lagTerms.y = 1,
                               maxLags.y = 3,
                               
                               varname.reg.end = c("stock"),
                               lagTerms.reg.end = c(0),
                               maxLags.reg.end = NULL,
                               
                               varname.reg.pre =  c("revenue_lag",  "assets"),
                               lagTerms.reg.pre = c(0,0),
                               maxLags.reg.pre = NULL,
                               
                               varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                               lagTerms.reg.ex = c(0,0,0),
                               maxLags.reg.ex = NULL,
                               
                               estimation = "twostep",
                               std.err = "corrected",
                               w.mat = "iid.err",
                               inst.collaps = TRUE)
  
  
  eqs_stock[["Murray_Riverina"]] = list(varname.y = "stock",
                                 lagTerms.y = 1,
                                 maxLags.y = 3,
                                 
                                 varname.reg.end = c("assets"),
                                 lagTerms.reg.end = c(0),
                                 maxLags.reg.end = 5,
                                 
                                 varname.reg.pre =  c("revenue_lag", "exp_lag"),
                                 lagTerms.reg.pre = c(0,0),
                                 maxLags.reg.pre = NULL,
                                 
                                 varname.reg.ex = c("weather_index_lag", "year","termsoftrade_index"),
                                 lagTerms.reg.ex = c(0,0,0),
                                 maxLags.reg.ex = NULL,
                                 
                                 estimation = "twostep",
                                 std.err = "corrected",
                                 w.mat = "identity",
                                 inst.collaps = TRUE)
  
  # Store and return
  pgmm_eq_list = list('revenue' =eqs_revenue,
                      'stock' = eqs_stock,
                      'exp' = eqs_exp)
  
  return(pgmm_eq_list)
}

# End of Function