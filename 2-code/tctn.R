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
    mutate(name = str_remove(name, "-")) %>% 
    rename(tube_name = name,
           TotalC_percent = c_percent,
           TotalN_percent = n_percent) %>% 
    left_join(subsampling %>% dplyr::select(tube_name, water_treatment, ftc)) %>% 
    filter(!is.na(ftc))
  
}

#
## LME
#library(nlme)
#lme(TotalC_percent ~ water_treatment, random= ~1|ftc, data = tctn_processed) %>% anova()
