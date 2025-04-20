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
           npoc_ugg = round(npoc_ugg, 2),
           tn_ugg = tn_mgL * ((vol_WEOM_mL + soilwater_g)/od_g),
           tn_ugg = round(tn_ugg, 2)) %>% 
    mutate(water_treatment = case_when(grepl("FOR_07|FOR_08|FOR_09|FOR_20", tube_name) ~ "low",
                                       grepl("FOR_10|FOR_11|FOR_12|FOR_21", tube_name) ~ "high",
                                       .default = water_treatment))
  
  npoc_processed = 
    npoc_samples %>% 
    dplyr::select(tube_name, water_treatment, tube_type, ftc, npoc_ugg, tn_ugg) %>% 
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
           mbc_ugg = round(mbc_ugg, 2),
           
           mbn_ugg = tn_mgL * ((vol_MBC_mL + soilwater_g)/od_g),
           mbn_ugg = round(mbn_ugg, 2)) %>% 
    mutate(water_treatment = case_when(grepl("FOR_07|FOR_08|FOR_09|FOR_20", tube_name) ~ "low",
                                       grepl("FOR_10|FOR_11|FOR_12|FOR_21", tube_name) ~ "high",
                                       .default = water_treatment))
 
  mbc_processed = 
    mbc_samples %>% 
    dplyr::select(tube_name, water_treatment, tube_type, ftc, mbc_ugg, mbn_ugg) %>% 
    mutate(ftc = factor(ftc, levels = c("t0", "ftc1", "ftc2", "ftc3")))
   
}


# plot

plot_weoc = function(weoc_processed){
  
  weoc_processed %>% 
    filter(tube_type == "destructive cores") %>% 
    ggplot(aes(x = ftc, y = npoc_ugg, color = water_treatment))+
    geom_point(size = 3)
  
  weoc_processed %>% 
    filter(tube_type == "destructive cores") %>% 
    ggplot(aes(x = ftc, y = tn_ugg, color = water_treatment))+
    geom_point(size = 3)
  
  weoc_processed %>% 
    filter(tube_type == "GHG cores" | ftc == "t0") %>% 
    ggplot(aes(x = ftc, y = npoc_ugg, color = water_treatment))+
    geom_point(size = 3)
  
}

plot_mbc = function(mbc_processed){
  
  mbc_processed %>% 
    filter(tube_type == "destructive cores") %>% 
    ggplot(aes(x = ftc, y = mbc_ugg, color = water_treatment))+
    geom_point(size = 3)
  
  mbc_processed %>% 
    filter(tube_type == "destructive cores") %>% 
    ggplot(aes(x = ftc, y = mbn_ugg, color = water_treatment))+
    geom_point(size = 3)
  
  mbc_processed %>% 
    filter(tube_type == "GHG cores" | ftc == "t0") %>% 
    ggplot(aes(x = ftc, y = mbc_ugg, color = water_treatment))+
    geom_point(size = 3)
  
  
}



# -------------------------------------------------------------------------
