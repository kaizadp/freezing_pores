# library(tidyverse)
# library(googlesheets4)
# WEOM and MBC processing

# corekey = read_sheet("https://docs.google.com/spreadsheets/d/1qpfho6Z7aHYg9zkT0dMtQy7Oth5MkVtFc7O7rB45_t4/edit?gid=0#gid=0")
# subsampling = read_sheet("https://docs.google.com/spreadsheets/d/1Zab9xJK-ACkxCwRfqAXps8NCVfx0ZkX2RyhmJ_92sWc/edit?gid=1453401056#gid=1453401056", sheet = "subsampling-USE THIS")


import_weoc_mbc = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
  
}

# process
process_weoc = function(weoc_data, subsampling){
  
  npoc_columns = 
    weoc_data %>% 
    dplyr::select(`Sample Name`, `Result(NPOC)`, `Result(TN)`) %>% 
    rename(tube_name = `Sample Name`,
           npoc_mgL = `Result(NPOC)`,
           tn_mgL = `Result(TN)`)
 
  npoc_blanks = 
    npoc_columns %>% 
    filter(grepl("filt", tube_name, ignore.case = T)) %>% 
    dplyr::summarise(npoc_blank = mean(npoc_mgL, na.rm = T),
                     tn_blank = mean(tn_mgL, na.rm = T))
    
  npoc_samples = 
    npoc_columns %>% 
    filter(grepl("tube|for", tube_name, ignore.case = T)) %>% 
    cross_join(npoc_blanks) %>% 
    mutate(npoc_mgL = npoc_mgL - npoc_blank,
           tn_mgL = tn_mgL - tn_blank) %>% 
    dplyr::select(-contains("blank")) %>% 
    # join the analysis key to get the sample_label
    left_join(subsampling) %>% 
    mutate(npoc_mgL = npoc_mgL * WEOM_dilution,
           tn_mgL = tn_mgL * WEOM_dilution,
           od_g = wt_WEOM_g/((gravimetric_water_percent/100)+1),
           soilwater_g = wt_WEOM_g - od_g,
           npoc_ugg = npoc_mgL * ((vol_WEOM_mL + soilwater_g)/od_g),
           WEOC_ugg = round(npoc_ugg, 2),
           tn_ugg = tn_mgL * ((vol_WEOM_mL + soilwater_g)/od_g),
           TDN_ugg = round(tn_ugg, 2)) %>% 
    mutate(water_treatment = case_when(grepl("FOR_07|FOR_08|FOR_09|FOR_20", tube_name) ~ "low",
                                       grepl("FOR_10|FOR_11|FOR_12|FOR_21", tube_name) ~ "high",
                                       .default = water_treatment))
  
  npoc_processed = 
    npoc_samples %>% 
    dplyr::select(tube_name, water_treatment, tube_type, ftc, WEOC_ugg, TDN_ugg) %>% 
    mutate(ftc = factor(ftc, levels = c("t0", "ftc1", "ftc2", "ftc3")))
  
}
process_mbc = function(mbc_data, subsampling){
  
  mbc_columns = 
    mbc_data %>% 
    dplyr::select(`Sample Name`, `Result(NPOC)`, `Result(TN)`) %>% 
    rename(tube_name = `Sample Name`,
           npoc_mgL = `Result(NPOC)`,
           tn_mgL = `Result(TN)`)
  
  mbc_blanks = 
    mbc_columns %>% 
    filter(grepl("filt", tube_name, ignore.case = T)) %>% 
    dplyr::summarise(npoc_blank = mean(npoc_mgL, na.rm = T),
                     tn_blank = mean(tn_mgL, na.rm = T))
  
  mbc_samples = 
    mbc_columns %>% 
    filter(grepl("tube|for", tube_name, ignore.case = T)) %>% 
    cross_join(mbc_blanks) %>% 
    mutate(npoc_mgL = npoc_mgL - npoc_blank,
           tn_mgL = tn_mgL - tn_blank) %>% 
    dplyr::select(-contains("blank")) %>% 
    left_join(subsampling) %>% 
    mutate(od_g = wt_MBC_g/((gravimetric_water_percent/100)+1),
           soilwater_g = wt_MBC_g - od_g,
           mbc_ugg = npoc_mgL * ((vol_MBC_mL + soilwater_g)/od_g),
           MBC_ugg = round(mbc_ugg, 2),
           
           mbn_ugg = tn_mgL * ((vol_MBC_mL + soilwater_g)/od_g),
           MBN_ugg = round(mbn_ugg, 2)) %>% 
    mutate(water_treatment = case_when(grepl("FOR_07|FOR_08|FOR_09|FOR_20", tube_name) ~ "low",
                                       grepl("FOR_10|FOR_11|FOR_12|FOR_21", tube_name) ~ "high",
                                       .default = water_treatment))
 
  mbc_processed = 
    mbc_samples %>% 
    dplyr::select(tube_name, water_treatment, tube_type, ftc, MBC_ugg, MBN_ugg) %>% 
    mutate(ftc = factor(ftc, levels = c("t0", "ftc1", "ftc2", "ftc3")))
   
}

#
# -------------------------------------------------------------------------

suva_fn = function(){
  ## suva
  suva = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Zab9xJK-ACkxCwRfqAXps8NCVfx0ZkX2RyhmJ_92sWc/",
                                   sheet = "SUVA")
  
  dilution = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Zab9xJK-ACkxCwRfqAXps8NCVfx0ZkX2RyhmJ_92sWc/",
                                       sheet = "subsampling-USE THIS") %>% dplyr::select(tube_name, WEOM_dilution, ftc, water_treatment)
  
  
  
  suva_processed = 
    weoc_data %>% 
    janitor::clean_names() %>% 
    dplyr::select(sample_name, result_npoc) %>% 
    rename(tube_name = sample_name) %>% 
    left_join(dilution) %>% 
    left_join(suva) %>% 
    filter(!is.na(abs_254nm)) %>% 
    mutate(npoc_dil_correction = result_npoc * WEOM_dilution,
           suva = abs_254nm / npoc_dil_correction)
  
  
  suva_processed %>% 
    ggplot(aes(x = ftc, y = suva, color = water_treatment, group = tube_name))+
    geom_point(size = 4)+
    facet_wrap(~water_treatment)
}



# -------------------------------------------------------------------------

# TC-TN

import_tctn = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  tctn_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_csv(path)
    df}))
  
  
}
process_tctn = function(tctn_data, subsampling){
  
  tctn_data %>% 
    janitor::clean_names() %>% 
    dplyr::select(name, n_percent, c_percent) %>% 
    rename(tube_name = name,
           TotalC_percent = c_percent,
           TotalN_percent = n_percent) %>% 
    left_join(subsampling %>% dplyr::select(tube_name, water_treatment, ftc)) %>% 
    filter(!is.na(ftc))
  
}

#

# -------------------------------------------------------------------------

combine_chemistry_data = function(weoc_processed, mbc_processed, tctn_processed){
  
  weoc_processed %>% 
    left_join(mbc_processed) %>% 
    left_join(tctn_processed)
}
