
# WEOM and MBC processing

core_key = read_sheet("https://docs.google.com/spreadsheets/d/1qpfho6Z7aHYg9zkT0dMtQy7Oth5MkVtFc7O7rB45_t4/edit?gid=0#gid=0")
subsampling = read_sheet("https://docs.google.com/spreadsheets/d/1Zab9xJK-ACkxCwRfqAXps8NCVfx0ZkX2RyhmJ_92sWc/edit?gid=1453401056#gid=1453401056", sheet = "subsampling-USE THIS")


import_weoc_mbc = function(FILEPATH, PATTERN){
  
  filePaths_weoc <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  weoc_dat <- do.call(bind_rows, lapply(filePaths_weoc, function(path) {
    df <- read_tsv(path, skip = 10)
    df}))
  
  
}


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
    mutate(od_g = wt_WEOM_g/((gravimetric_water_percent/100)+1),
           soilwater_g = wt_WEOM_g - od_g,
           npoc_ug_g = npoc_mgL * ((vol_WEOM_mL + soilwater_g)/od_g),
           npoc_ug_g = round(npoc_ug_g, 2))
  
}
process_mbc = function(weoc_data, analysis_key, moisture_processed, subsampling){
  
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
           mbc_ug_g = npoc_mgL * ((vol_MBC_mL + soilwater_g)/od_g),
           mbc_ug_g = round(mbc_ug_g, 2),
           
           mbn_ug_g = tn_mgL * ((vol_MBC_mL + soilwater_g)/od_g),
           mbn_ug_g = round(mbn_ug_g, 2))
  
}



# -------------------------------------------------------------------------
weoc_data = import_weoc_mbc(FILEPATH = "1-data/weom", PATTERN = "summary")
mbc_data = import_weoc_mbc(FILEPATH = "1-data/mbc", PATTERN = "summary")


mbc_samples %>% 
  ggplot(aes(x = ftc, y = mbc_ug_g, color = water_treatment))+
  geom_point(size = 3)

mbc_samples %>% 
  ggplot(aes(x = ftc, y = mbn_ug_g, color = water_treatment))+
  geom_point(size = 3)



npoc_samples %>% 
  ggplot(aes(x = ftc, y = npoc_ug_g, color = water_treatment))+
  geom_point(size = 3)
