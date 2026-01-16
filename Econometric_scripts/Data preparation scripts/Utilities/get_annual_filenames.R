
# Retrieves filenames for annual BIT within the BIT standard filepath provided
# By the ABS

get_BIT_annual_filenames <- function() {
  
  BIT_files_company = c("bit_comp_bn_0102.csv",
                        "bit_comp_bn_0203.csv",
                        "bit_comp_bn_0304.csv",
                        "bit_comp_bn_0405.csv",
                        "bit_comp_bn_0506.csv",
                        "bit_comp_bn_0607.csv",
                        "bit_comp_bn_0708.csv",
                        "bit_comp_bn_0809.csv",
                        "bit_comp_bn_0910.csv",
                        "bit_comp_bn_1011.csv",
                        "bit_comp_bn_1112.csv",
                        "bit_comp_bn_1213.csv",
                        "bit_comp_bn_1314.csv",
                        "bit_comp_bn_1415.csv",
                        "bit_comp_bn_1516.csv",
                        "bit_comp_bn_1617.csv",
                        "bit_comp_bn_1718.csv",
                        "bit_comp_bn_1819.csv",
                        "bit_comp_bn_1920.csv",
                        "bit_comp_bn_2021.csv")
  
  BIT_files_individual = c("bit_indi_bn_0102.csv",
                           "bit_indi_bn_0203.csv",
                           "bit_indi_bn_0304.csv",
                           "bit_indi_bn_0405.csv",
                           "bit_indi_bn_0506.csv",
                           "bit_indi_bn_0607.csv",
                           "bit_indi_bn_0708.csv",
                           "bit_indi_bn_0809.csv",
                           "bit_indi_bn_0910.csv",
                           "bit_indi_bn_1011.csv",
                           "bit_indi_bn_1112.csv",
                           "bit_indi_bn_1213.csv",
                           "bit_indi_bn_1314.csv",
                           "bit_indi_bn_1415.csv",
                           "bit_indi_bn_1516.csv",
                           "bit_indi_bn_1617.csv",
                           "bit_indi_bn_1718.csv",
                           "bit_indi_bn_1819.csv",
                           "bit_indi_bn_1920.csv",
                           "bit_indi_bn_2021.csv")
  
  BIT_files_trust = c("bit_trus_bn_0102.csv",
                      "bit_trus_bn_0203.csv",
                      "bit_trus_bn_0304.csv",
                      "bit_trus_bn_0405.csv",
                      "bit_trus_bn_0506.csv",
                      "bit_trus_bn_0607.csv",
                      "bit_trus_bn_0708.csv",
                      "bit_trus_bn_0809.csv",
                      "bit_trus_bn_0910.csv",
                      "bit_trus_bn_1011.csv",
                      "bit_trus_bn_1112.csv",
                      "bit_trus_bn_1213.csv",
                      "bit_trus_bn_1314.csv",
                      "bit_trus_bn_1415.csv",
                      "bit_trus_bn_1516.csv",
                      "bit_trus_bn_1617.csv",
                      "bit_trus_bn_1718.csv",
                      "bit_trus_bn_1819.csv",
                      "bit_trus_bn_1920.csv",
                      "bit_trus_bn_2021.csv")
  
  BIT_files_partnership = c("bit_part_bn_0102.csv",
                            "bit_part_bn_0203.csv",
                            "bit_part_bn_0304.csv",
                            "bit_part_bn_0405.csv",
                            "bit_part_bn_0506.csv",
                            "bit_part_bn_0607.csv",
                            "bit_part_bn_0708.csv",
                            "bit_part_bn_0809.csv",
                            "bit_part_bn_0910.csv",
                            "bit_part_bn_1011.csv",
                            "bit_part_bn_1112.csv",
                            "bit_part_bn_1213.csv",
                            "bit_part_bn_1314.csv",
                            "bit_part_bn_1415.csv",
                            "bit_part_bn_1516.csv",
                            "bit_part_bn_1617.csv",
                            "bit_part_bn_1718.csv",
                            "bit_part_bn_1819.csv",
                            "bit_part_bn_1920.csv",
                            "bit_part_bn_2021.csv")
  
  indicative_frame_names = c("frame_bn_0102.csv",
                             "frame_bn_0203.csv",
                             "frame_bn_0304.csv",
                             "frame_bn_0405.csv",
                             "frame_bn_0506.csv",
                             "frame_bn_0607.csv",
                             "frame_bn_0708.csv",
                             "frame_bn_0809.csv",
                             "frame_bn_0910.csv",
                             "frame_bn_1011.csv",
                             "frame_bn_1112.csv",
                             "frame_bn_1213.csv",
                             "frame_bn_1314.csv",
                             "frame_bn_1415.csv",
                             "frame_bn_1516.csv",
                             "frame_bn_1617.csv",
                             "frame_bn_1718.csv",
                             "frame_bn_1819.csv",
                             "frame_bn_1920.csv",
                             "frame_bn_2021.csv")
  
  ag_frame_names = c("ag_bn_1011",
                        "ag_bn_1112",
                        "ag_bn_1213",
                        "ag_bn_1314",
                        "ag_bn_1415",
                        "ag_bn_1516",
                        "ag_bn_1617",
                        "ag_bn_1718",
                        "ag_bn_1819",
                        "ag_bn_1920",
                        "ag_bn_2021")
  
  bas_names = c("bas_bn_0102.csv",
               "bas_bn_0203.csv",
               "bas_bn_0304.csv",
               "bas_bn_0405.csv",
               "bas_bn_0506.csv",
               "bas_bn_0607.csv",
               "bas_bn_0708.csv",
               "bas_bn_0809.csv",
               "bas_bn_0910.csv",
               "bas_bn_1011.csv",
               "bas_bn_1112.csv",
               "bas_bn_1213.csv",
               "bas_bn_1314.csv",
               "bas_bn_1415.csv",
               "bas_bn_1516.csv",
               "bas_bn_1617.csv",
               "bas_bn_1718.csv",
               "bas_bn_1819.csv",
               "bas_bn_1920.csv",
               "bas_bn_2021.csv")
  
  payg_names = c("payg_bn_0102.csv",
                "payg_bn_0203.csv",
                "payg_bn_0304.csv",
                "payg_bn_0405.csv",
                "payg_bn_0506.csv",
                "payg_bn_0607.csv",
                "payg_bn_0708.csv",
                "payg_bn_0809.csv",
                "payg_bn_0910.csv",
                "payg_bn_1011.csv",
                "payg_bn_1112.csv",
                "payg_bn_1213.csv",
                "payg_bn_1314.csv",
                "payg_bn_1415.csv",
                "payg_bn_1516.csv",
                "payg_bn_1617.csv",
                "payg_bn_1718.csv",
                "payg_bn_1819.csv",
                "payg_bn_1920.csv",
                "payg_bn_2021.csv")
  
  write.csv(BIT_files_company, file.path(root_wd, data_path, "BIT_files_annual_company.csv"))
  write.csv(BIT_files_individual, file.path(root_wd, data_path, "BIT_files_annual_individual.csv"))
  write.csv(BIT_files_trust, file.path(root_wd, data_path, "BIT_files_annual_trust.csv"))
  write.csv(BIT_files_partnership, file.path(root_wd, data_path, "BIT_files_annual_partnership.csv"))
  write.csv(indicative_frame_names, file.path(root_wd, data_path, "indicative_frame_names.csv"))
  write.csv(ag_frame_names, file.path(root_wd, data_path, "ag_frame_names.csv"))
  write.csv(bas_names, file.path(root_wd, data_path, "bas_names.csv"))
  write.csv(payg_names, file.path(root_wd, data_path, "payg_names.csv"))
}




