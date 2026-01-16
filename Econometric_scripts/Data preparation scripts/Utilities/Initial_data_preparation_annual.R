
# This function dynamically merges and subsets the raw BLADE dataframes for analysis
# Output includes all Ag businesses in Australia with a defined ANZSIC code, with selected variables. 
# Note the size of the files means the run time of this script is long, recommended to run overnight. 

Initial_data_preparation_annual = function() {
  
  # install.packages('gtools')
  library(gtools)

  # Retrieve initial Frames for initialising the dataframe
  frame_path =  # insert file path to retrieve indicative frame names here. 
  frame_names = read.csv(file.path(root_wd, data_path, "indicative_frame_names.csv"))
  
  # Define anzsic codes to keep
  keep_anzsic = c(0141:0149, 0152, 0160)
  
  #initialise dataframe with first year
  data_temp = read.csv(file.path(frame_path, frame_names[1,2]))
  
  data_temp = subset(data_temp, data_temp$d_anzsic06 %in% keep_anzsic)
  names(data_temp)[which(names(data_temp) == "x_st_op")] = "state_operation"
  names(data_temp)[which(names(data_temp) == "x_pcode")] = "postcode"
  names(data_temp)[which(names(data_temp) == "x_al_st")] = "business_status"
  names(data_temp)[which(names(data_temp) == "x_state")] = "state"
  names(data_temp)[which(names(data_temp) == "d_div06")] = "div06_code"
  names(data_temp)[which(names(data_temp) =="d_anzsic06")] = "latest_anzsic_all" 
  
  keep_names = c("bn", "tsid", "state_operation", "postcode", "business_status", 
                 "state", "latest_anzsic_all", "div06_code")
  
  data_master = data_temp[, keep_names]
  
  # Now loop through all remaining frame csv files and merge in
  for (file in 2:nrow(frame_names)){
    print(file)
    data_temp = read.csv(file.path(frame_path, frame_names[file,2]))
    if (max(data_temp$tsid > 5)){
      data_temp = subset(data_temp, data_temp$x_anzsic06 %in% keep_anzsic)
      names(data_temp)[which(names(data_temp) == "x_st_op")] = "state_operation"
      names(data_temp)[which(names(data_temp) == "x_pcode")] = "postcode"
      names(data_temp)[which(names(data_temp) == "x_al_st")] = "business_status"
      names(data_temp)[which(names(data_temp) == "x_state")] = "state"
      names(data_temp)[which(names(data_temp) == "d_div06")] = "div06_code"
      names(data_temp)[which(names(data_temp) =="x_anzsic06")] = "latest_anzsic_all"
    } else {
      data_temp = subset(data_temp, data_temp$d_anzsic06 %in% keep_anzsic)
      names(data_temp)[which(names(data_temp) == "x_st_op")] = "state_operation"
      names(data_temp)[which(names(data_temp) == "x_pcode")] = "postcode"
      names(data_temp)[which(names(data_temp) == "x_al_st")] = "business_status"
      names(data_temp)[which(names(data_temp) == "x_state")] = "state"
      names(data_temp)[which(names(data_temp) == "d_div06")] = "div06_code"
      names(data_temp)[which(names(data_temp) =="d_anzsic06")] = "latest_anzsic_all"
    }
    data_temp = data_temp[, which(names(data_temp) %in% keep_names)]
    data_master = rbind(data_master, data_temp)
  }
  
  #### Now introduce BIT dataframes #### 
  
  # Retrieve file names
  filenames_BIT_company = read.csv(file.path(root_wd, data_path, "BIT_files_annual_company.csv"))
  filenames_BIT_indi = read.csv(file.path(root_wd, data_path, "BIT_files_annual_individual.csv"))
  filenames_BIT_trust = read.csv(file.path(root_wd, data_path, "BIT_files_annual_trust.csv"))
  filenames_BIT_partnership = read.csv(file.path(root_wd, data_path, "BIT_files_annual_partnership.csv"))
  
  # Read in first BIT company file to initialise dataframe.
  BIT_path = # enter BIT filepath here. 
  
  ## COMPANY BIT ##
  keep_vars = c("bn", "tsid", "c_totlasst", "c_currasst", "c_currliab", "c_totlliab", "c_totl_debt_amt", 
  "c_opnstock" ,"c_clostock", "c_ncg_amt", "c_costsale", "c_extnlabr", "c_totlwage", "c_totwagcd" ,
  "c_superann", "c_leaexpau", "c_leaexpos", "c_rentexps" ,"c_motrexps" ,"c_repmaint" ,
  "c_royexpauc_deprexps", "c_ula", "c_intexpau", "c_intexpos" ,"c_expnss_fincl_arngmts_tofa_amt" ,
  "c_othrexps" ,"c_totlexps", "c_frrswinc", "c_gpnabn", "c_sales", "c_govind", "c_grosrent", "c_grosintr" ,
  "c_uga", "c_inclosp", "c_inclost", "c_dstst_cd", "c_frstry_mngd_invmtschm_incm_amt" ,
  "c_incm_fincl_arngmts_tofa_amt", "c_totlinc", "c_toprolos", "c_oproloss", "c_baddebts") 
  
  keep_new_vars = c("bn", "tsid" ,"assets_total" ,"assets_current", "liability_current" ,"liability_total" ,
                    "debt_total", "stock_opening","stock_closing", "exp_sales" ,"exp_contractors",
                    "exp_wages" ,"exp_wages_code", "exp_super", "exp_lease", "exp_motorVehicles",
                    "exp_repairsMaintenance", "exp_royalties", "exp_badDebts", "exp_depreciation", "exp_revaluation" ,
                    "exp_interest", "exp_other", "exp_total", "inc_grossPayments", "inc_govPayments", "inc_lease", 
                    "inc_interest", "inc_revaluation", "inc_other", "inc_total", "profit_total", "profit_net_operating")
  
  data_master_c = data.frame()
  
  # Now loop through BIT company and merge in with master. 
  for (file in 1:nrow(filenames_BIT_company)){
    print(file)
    data_temp = read.csv(file.path(BIT_path, filenames_BIT_company[file,2]))
    data_temp = data_temp[data_temp$bn %in% data_master$bn,] #subset out by anzsic codes
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_vars)]
    
    if (nrow(data_temp) == 0){
      next
    }
    
    names(data_temp)[which(names(data_temp) == "c_totlasst")] = "assets_total"
    names(data_temp)[which(names(data_temp) == "c_currasst")] = "assets_current"
    names(data_temp)[which(names(data_temp) == "c_currliab")] = "liability_current"
    names(data_temp)[which(names(data_temp) == "c_totlliab")] = "liability_total"
    names(data_temp)[which(names(data_temp) == "c_totl_debt_amt")] = "debt_total"
    names(data_temp)[which(names(data_temp) == "c_opnstock")] =  "stock_opening"
    names(data_temp)[which(names(data_temp) == "c_clostock")] =  "stock_closing"
    names(data_temp)[which(names(data_temp) == "c_costsale")] =  "exp_sales"
    names(data_temp)[which(names(data_temp) == "c_extnlabr")] =  "exp_contractors"
    names(data_temp)[which(names(data_temp) == "c_baddebts")] =  "exp_badDebts"
    names(data_temp)[which(names(data_temp) == "c_totlwage")] =  "exp_wages"
    names(data_temp)[which(names(data_temp) == "c_totwagcd")] =  "exp_wages_code"
    names(data_temp)[which(names(data_temp) == "c_superann")] =  "exp_super"
    data_temp$exp_lease = rowSums(cbind(data_temp$c_leaexpau, data_temp$c_leaexpos, data_temp$c_rentexps), na.rm = TRUE)
    names(data_temp)[which(names(data_temp) == "c_motrexps")] =  "exp_motorVehicles"
    names(data_temp)[which(names(data_temp) == "c_repmaint")] =  "exp_repairsMaintenance"
    names(data_temp)[which(names(data_temp) == "c_royexpau")] =  "exp_royalties"
    names(data_temp)[which(names(data_temp) == "c_deprexps")] =  "exp_depreciation"
    names(data_temp)[which(names(data_temp) == "c_ula")] =  "exp_revaluation"
    data_temp$exp_revaluation = as.numeric(data_temp$exp_revaluation)
    data_temp$exp_interest = rowSums(cbind(data_temp$c_intexpau, data_temp$c_intexpos), na.rm = TRUE)
    data_temp$exp_other= rowSums(cbind(data_temp$c_expnss_fincl_arngmts_tofa_amt, data_temp$c_othrexps), na.rm = TRUE)
    names(data_temp)[which(names(data_temp) == "c_totlexps")] =  "exp_total"
    data_temp$inc_grossPayments= rowSums(cbind(data_temp$c_frrswinc , data_temp$c_gpnabn, data_temp$c_sales), na.rm = TRUE)
    names(data_temp)[which(names(data_temp) == "c_govind")] =  "inc_govPayments"
    names(data_temp)[which(names(data_temp) == "c_grosrent")] =  "inc_lease"
    names(data_temp)[which(names(data_temp) == "c_grosintr")] =  "inc_interest"
    names(data_temp)[which(names(data_temp) == "c_uga")] =  "inc_revaluation"
    names(data_temp)[which(names(data_temp) == "c_totlinc")] =  "inc_total"
    names(data_temp)[which(names(data_temp) == "c_oproloss")] =  "profit_net_operating"
    names(data_temp)[which(names(data_temp) == "c_toprolos")] =  "profit_total"
    data_temp$inc_other = rowSums(cbind(data_temp$c_inclosp, data_temp$c_frstry_mngd_invmtschm_incm_amt, data_temp$c_incm_fincl_arngmts_tofa_amt), na.rm = TRUE)
  
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_new_vars)]
    data_master_c = smartbind(data_master_c, data_temp, fill = NA)
  }
  data_master_c$profit_net_operating[is.na(data_master_c$profit_net_operating)] = data_master_c$profit_total[is.na(data_master_c$profit_net_operating)] 
  
  ## Individual BIT ##
  keep_vars = c("bn", "tsid", "i_debtors", "i_clostock", "i_creditrs", "i_opnstock", "i_costsale", "i_totlwage", "i_totlwgcd", "i_motrexps",
                "i_repmaint", "i_superann", "i_leaextot", "i_intexpau", "i_intexpos", "i_deprexps", "i_extnlabr",
                "i_baddebts" ,"i_othrexps", "i_totlexps", "i_noabnpp" ,"i_noabnnpp", "i_gpsfrwpp", "i_gpsfrwnp", "i_grpvagpp", "i_grpvanpp", "i_asgipnpp",
                "i_asgidppp", "i_grpylhpp" ,"i_grplhnpp", "i_othbicpp", "i_othbinpp" ,"i_netincls", "i_ntinnpp", "i_totbusin")
  
  data_master_i = data.frame()
  
  for (file in 1:nrow(filenames_BIT_indi)){
    print(file)
    data_temp = read.csv(file.path(BIT_path, filenames_BIT_indi[file,2]))
    data_temp = data_temp[data_temp$bn %in% data_master$bn,] #subset out by anzsic codes
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_vars)]
    
    if (nrow(data_temp) == 0){
      next
    }
    
    data_temp$assets_total = rowSums(cbind(data_temp$i_debtors, data_temp$i_clostock), na.rm = TRUE)
    data_temp$assets_current = data_temp$assets_total
    names(data_temp)[which(names(data_temp) == "i_creditrs")] = "liability_current"
    data_temp$liability_total = data_temp$liability_current
    data_temp$debt_total = data_temp$liability_total
    names(data_temp)[which(names(data_temp) == "i_opnstock")] =  "stock_opening"
    names(data_temp)[which(names(data_temp) == "i_clostock")] =  "stock_closing"
    names(data_temp)[which(names(data_temp) == "i_costsale")] =  "exp_sales"
    names(data_temp)[which(names(data_temp) == "i_extnlabr")] =  "exp_contractors"
    names(data_temp)[which(names(data_temp) == "i_baddebts")] =  "exp_badDebts"
    names(data_temp)[which(names(data_temp) == "i_totlwage")] =  "exp_wages"
    names(data_temp)[which(names(data_temp) == "i_totlwgcd")] =  "exp_wages_code"
    names(data_temp)[which(names(data_temp) == "i_superann")] =  "exp_super"
    names(data_temp)[which(names(data_temp) == "i_leaextot")] =  "exp_lease"
    names(data_temp)[which(names(data_temp) == "i_motrexps")] =  "exp_motorVehicles"
    names(data_temp)[which(names(data_temp) == "i_repmaint")] =  "exp_repairsMaintenance"
    data_temp$exp_royalties = 0
    names(data_temp)[which(names(data_temp) == "i_deprexps")] =  "exp_depreciation"
    data_temp$exp_revaluation = 0
    data_temp$exp_interest = rowSums(cbind(data_temp$i_intexpau, data_temp$i_intexpos), na.rm = TRUE)
    names(data_temp)[which(names(data_temp) == "i_othrexps")] =  "exp_other"
    names(data_temp)[which(names(data_temp) == "i_totlexps")] =  "exp_total"
    data_temp$inc_grossPayments = rowSums(cbind(data_temp$i_noabnpp , data_temp$i_noabnnpp, data_temp$i_gpsfrwpp, data_temp$i_gpsfrwnp, data_temp$i_grpvagpp, data_temp$prpvanpp), na.rm = TRUE)
    data_temp$inc_govPayments = rowSums(cbind(data_temp$i_asgipnpp, data_temp$i_asgidppp), na.rm = TRUE)
    data_temp$inc_lease = 0
    data_temp$inc_interest = 0
    data_temp$inc_revaluation = 0
    data_temp$inc_other = rowSums(cbind(data_temp$i_grpylhpp, data_temp$i_grplhnpp, data_temp$i_othbicpp, data_temp$i_othbinpp), na.rm = TRUE)
    names(data_temp)[which(names(data_temp) == "i_totbusin")] =  "inc_total"
    data_temp$profit_total = rowSums(cbind(data_temp$i_netincls, data_temp$i_netincnpp), na.rm = TRUE)
    data_temp$profit_net_operating = data_temp$profit_total
    
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_new_vars)]
    data_master_i = smartbind(data_master_i, data_temp, fill = NA)
  }
  
  ## TRUST BIT ##
  
  keep_vars = c("bn", "tsid", "t_totlasst", "t_totlliab", "t_currasst", "t_currliab", "t_opnstock", "t_clostock", "t_costsale",
                "t_totlwage", "t_salwgexp", "t_superann", "t_motrexps", "t_repmaint", "t_deprexps", "t_intextot", "t_baddebts", "t_royextot",
                "t_leaextot", "t_extnlabr", "t_othrexps", "t_totlexps", "t_gpabnpp", "t_gpabnnpp", "t_gpsfrwpp", "t_gpsfrwnp", "t_asginppp",
                "t_asgipnpp", "t_netrentt", "t_grssintr", "t_othbicpp", "t_othbinpp", "t_frstry_mis_incm_amt", "t_totbuspp", "t_totbusnp", "t_netilbus")
  
  data_master_t = data.frame()
  
  for (file in 1:nrow(filenames_BIT_trust)){
    print(file)
    data_temp = read.csv(file.path(BIT_path, filenames_BIT_trust[file,2]))
    data_temp = data_temp[data_temp$bn %in% data_master$bn,] #subset out by anzsic codes
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_vars)]
    
    if (nrow(data_temp) == 0){
      next
    }
    
    data_temp$assets_total = data_temp$t_totlasst
    data_temp$assets_current = data_temp$t_currasst
    names(data_temp)[which(names(data_temp) == "t_currliab")] = "liability_current"
    names(data_temp)[which(names(data_temp) == "t_totlliab")] = "liability_total"
    data_temp$debt_total =  data_temp$liability_total
    names(data_temp)[which(names(data_temp) == "t_opnstock")] =  "stock_opening"
    names(data_temp)[which(names(data_temp) == "t_clostock")] =  "stock_closing"
    names(data_temp)[which(names(data_temp) == "t_costsale")] =  "exp_sales"
    names(data_temp)[which(names(data_temp) == "t_extnlabr")] =  "exp_contractors"
    names(data_temp)[which(names(data_temp) == "t_baddebts")] =  "exp_badDebts"
    names(data_temp)[which(names(data_temp) == "t_totlwage")] =  "exp_wages"
    names(data_temp)[which(names(data_temp) == "t_salwgexp")] =  "exp_wages_code"
    names(data_temp)[which(names(data_temp) == "t_superann")] =  "exp_super"
    names(data_temp)[which(names(data_temp) == "t_leaextot")] =  "exp_lease"
    names(data_temp)[which(names(data_temp) == "t_motrexps")] =  "exp_motorVehicles"
    names(data_temp)[which(names(data_temp) == "t_repmaint")] =  "exp_repairsMaintenance"
    data_temp$exp_royalties = data_temp$t_royextot
    names(data_temp)[which(names(data_temp) == "t_deprexps")] =  "exp_depreciation"
    data_temp$exp_revaluation = 0
    data_temp$exp_interest = data_temp$t_intextot
    names(data_temp)[which(names(data_temp) == "t_othrexps")] =  "exp_other"
    names(data_temp)[which(names(data_temp) == "t_totlexps")] =  "exp_total"
    data_temp$inc_grossPayments = rowSums(cbind(data_temp$t_gpabnpp , data_temp$t_gpabnnpp, data_temp$t_gpsfrwpp, data_temp$t_gpsfrwnp), na.rm = TRUE)
    data_temp$inc_govPayments = rowSums(cbind(data_temp$t_asginppp, data_temp$t_asginppp), na.rm = TRUE)
    data_temp$inc_lease = data_temp$t_netrentt
    data_temp$inc_interest = data_temp$t_grssintr
    data_temp$inc_revaluation = 0
    data_temp$inc_total = rowSums(cbind(data_temp$t_totbuspp, data_temp$t_totbusnp))
    data_temp$inc_other = rowSums(cbind(data_temp$t_othbicpp, data_temp$t_othbinpp, data_temp$t_frstry_mis_incm_amt), na.rm = TRUE)
    data_temp$profit_total = data_temp$t_netilbus
    data_temp$profit_net_operating = rowSums(cbind(data_temp$profit_total, -1*data_temp$exp_interest), na.rm = TRUE)
    
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_new_vars)]
    data_master_t = smartbind(data_master_t, data_temp, fill = NA)
  }
  
  ## Partnerships ##
  keep_vars = c("bn", "tsid",  "p_totlasst", "p_currasst", "p_currliab", "p_totlliab", "p_opnstock", "p_clostock", "p_costsale",
                "p_totlwage", "p_salwgexp", "p_superann", "p_motrexps", "p_repmaint", "p_deprexps", "p_intextot", "p_baddebts",
                "p_royextot", "p_leaextot", "p_extnlabr", 'p_othrexps', "p_totlexps", "p_gpabnpp", "p_gpabnnpp", "p_gpsfrwpp",
                "p_gpsfrwnp", "p_asginppp", "p_asgipnpp", "p_netrentt", "p_grssintr", "p_othbicpp", "p_othbinpp", "p_frstry_mis_incm_amt",
                "p_totbuspp", "p_totbusnp", "p_netilbus")
  
  data_master_p = data.frame()
  
  for (file in 1:nrow(filenames_BIT_partnership)){
    print(file)
    data_temp = read.csv(file.path(BIT_path, filenames_BIT_partnership[file,2]))
    data_temp = data_temp[data_temp$bn %in% data_master$bn,] #subset out by anzsic codes
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_vars)]
    
    if (nrow(data_temp) == 0){
      next
    }
    
    data_temp$assets_total = data_temp$p_totlasst
    data_temp$assets_current = data_temp$p_currasst
    names(data_temp)[which(names(data_temp) == "p_currliab")] = "liability_current"
    names(data_temp)[which(names(data_temp) == "p_totlliab")] = "liability_total"
    data_temp$debt_total =  data_temp$liability_total
    names(data_temp)[which(names(data_temp) == "p_opnstock")] =  "stock_opening"
    names(data_temp)[which(names(data_temp) == "p_clostock")] =  "stock_closing"
    names(data_temp)[which(names(data_temp) == "p_costsale")] =  "exp_sales"
    names(data_temp)[which(names(data_temp) == "p_extnlabr")] =  "exp_contractors"
    names(data_temp)[which(names(data_temp) == "p_baddebts")] =  "exp_badDebts"
    names(data_temp)[which(names(data_temp) == "p_totlwage")] =  "exp_wages"
    names(data_temp)[which(names(data_temp) == "p_salwgexp")] =  "exp_wages_code"
    names(data_temp)[which(names(data_temp) == "p_superann")] =  "exp_super"
    names(data_temp)[which(names(data_temp) == "p_leaextot")] =  "exp_lease"
    names(data_temp)[which(names(data_temp) == "p_motrexps")] =  "exp_motorVehicles"
    names(data_temp)[which(names(data_temp) == "p_repmaint")] =  "exp_repairsMaintenance"
    data_temp$exp_royalties = data_temp$p_royextot
    names(data_temp)[which(names(data_temp) == "p_deprexps")] =  "exp_depreciation"
    data_temp$exp_revaluation = 0
    data_temp$exp_interest = data_temp$p_intextot
    names(data_temp)[which(names(data_temp) == "p_othrexps")] =  "exp_other"
    names(data_temp)[which(names(data_temp) == "p_totlexps")] =  "exp_total"
    data_temp$inc_grossPayments = rowSums(cbind(data_temp$p_gpabnpp , data_temp$p_gpabnnpp, data_temp$p_gpsfrwpp, data_temp$p_gpsfrwnp), na.rm = TRUE)
    data_temp$inc_govPayments = rowSums(cbind(data_temp$p_asginppp, data_temp$p_asginppp), na.rm = TRUE)
    data_temp$inc_lease = data_temp$p_netrentt
    data_temp$inc_interest = data_temp$p_grssintr
    data_temp$inc_revaluation = 0
    data_temp$inc_total = rowSums(cbind(data_temp$p_totbuspp, data_temp$p_totbusnp))
    data_temp$inc_other = rowSums(cbind(data_temp$p_othbicpp, data_temp$p_othbinpp, data_temp$p_frstry_mis_incm_amt), na.rm = TRUE)
    data_temp$profit_total = data_temp$p_netilbus
    data_temp$profit_net_operating = rowSums(cbind(data_temp$profit_total, -1*data_temp$exp_interest), na.rm = TRUE)
    
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_new_vars)]
    data_master_p = smartbind(data_master_p, data_temp, fill = NA)
  }
  
  #### Merge dataframes together ####
  data_master_c = smartbind(data_master_c, data_master_i)
  data_master_c = smartbind(data_master_c, data_master_t)
  data_master_c = smartbind(data_master_c, data_master_p)
  data_master = merge(data_master, data_master_c, by = c("bn", "tsid")) #merge with ID frame
  
  rm(data_master_c)
  rm(data_master_t)
  rm(data_master_i)
  rm(data_master_p)
  
  #Save backup due to length of compiling
  save(data_master, file = file.path(root_wd, data_path, "data_master_BITONLY.Rdata"))
  load(file.path(root_wd, data_path, "data_master_BITONLY.Rdata"))
  
  #### Merge in agricultural frame ####
  AGFRAME_path = # Enter AG frame file path here. 
  filenames_ag_frame = read.csv(file.path(root_wd, data_path, "ag_frame_names.csv"))
  
  keep_vars = c("bn", "tsid", "EVAO_derived", "area_holdings_derived", "state_geographic", 
                "sa1_code_2021", "sa2_code_2021", "sa2_name_2021")
  
  data_master_ag = data.frame()
  
  for (file in 1:nrow(filenames_ag_frame)){
    print(file)
    data_temp = read.csv(paste(file.path(AGFRAME_path, filenames_ag_frame[file,2]),'.csv', sep = ""))
    data_temp = data_temp[data_temp$bn %in% data_master$bn,] #subset out by anzsic codes
    
    if (nrow(data_temp) == 0){
      next
    }
    
    data_temp$EVAO_derived = data_temp$d_evao 
    data_temp$area_holdings_derived =  data_temp$d_aoh 
    data_temp$state_geographic =  data_temp$d_ag_state
  
    data_temp = data_temp[ ,which(names(data_temp) %in% keep_vars)]
    data_master_ag = rbind(data_master_ag, data_temp)
  }
  
  data_master_x = merge(data_master, data_master_ag, by = c("bn", "tsid"), all.x = TRUE)
  
  #### Save master dataframe ####
  save(data_master, file = file.path(root_wd, data_path, "data_master.Rdata"))
  
}

# End of function


