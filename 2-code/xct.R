library(tidyverse)
theme_set(theme_bw(base_size = 14,
                   base_line_size = 1))
options(scipen = 10000)


tests = function(){

corekey = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qpfho6Z7aHYg9zkT0dMtQy7Oth5MkVtFc7O7rB45_t4/edit?gid=0#gid=0")



test = read.csv("1-data/xct/csv/FOR_01_02_F1__F0.csv", nrows = 35, na = "")
colnames(test) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
                   "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
                   "W", "X", "Y", "Z", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH")


diameter = test[-c(1:10),]$O %>% tibble()
colnames(diameter) = "diameter_mm"

diameter = 
  diameter %>% 
  mutate(diameter_mm = as.numeric(diameter_mm))


diameter %>% 
  filter(diameter_mm < 20) %>% 
  ggplot(aes(x = diameter_mm))+
  geom_density()
  xlim(0, 0.1)


diameter %>% 
  filter(diameter_mm < 20) %>% 
  ggplot(aes(y = diameter_mm, x = 1))+
  geom_jitter(size =0.1)
xlim(0, 0.1)
scale_x_log10()

test$A[20]


test_long =
  test %>% 
  rownames_to_column("ROW") %>% 
  pivot_longer(cols = -ROW)


}


import_xct_summaries = function(FILEPATH, PATTERN){
  
  filePaths_xct <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  xct_dat <- do.call(bind_rows, lapply(filePaths_xct, function(path) {
    df <- read.csv(path, nrows = 35, na = "") %>% mutate_all(as.character)
    colnames(df) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
                       "L", "M", "N", "O", "P")
    df$source = basename(path)
    df = df %>% rownames_to_column("ROW")
    
    df}))
  
  
}
process_xct_summaries = function(xct_summary){
  
  xct_summary_long =
    xct_summary %>% 
    pivot_longer(cols = -c(ROW, source)) %>% 
    filter(!is.na(value)) %>% 
    mutate(cell = paste0(name, ROW)) %>% 
    mutate(new_name = case_when(cell == "A14" ~ "PNM_z_k_um2",
                                cell == "A18" ~ "PNM_y_k_um2",
                                cell == "A22" ~ "PNM_x_k_um2",
                                
                                cell == "C14" ~ "flow_rate_z",
                                cell == "C18" ~ "flow_rate_y",
                                cell == "C22" ~ "flow_rate_x",
                                
                                cell == "D14" ~ "tortuosity_z",
                                cell == "D18" ~ "tortuosity_y",
                                cell == "D22" ~ "tortuosity_x",
                                
                                
                                cell == "B8" ~ "connected_pores_percent",
                                
                                cell == "A22" ~ "PNM_x_k_um2",
                                
                                cell == "F2" ~ "total_volume_pores_mm3",
                                cell == "F6" ~ "connected_pores_fraction",
                                cell == "10" ~ "unconnected_pores_fraction"
    )) %>% 
    mutate(value = str_remove(value, "%"),
           value = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    filter(!is.na(new_name)) %>% 
    dplyr::select(-c(ROW, cell, name)) %>% 
    pivot_wider(names_from = "new_name") %>% 
    mutate(core_name = str_extract(source, "FOR_[0-9]{2}"),
           timepoint = str_extract(source, "F[0-9]|T[0-9]"),
           timepoint = str_remove_all(timepoint, "_"),
           timepoint = factor(timepoint, 
                              levels = c("T0", "F1", "T1", "F2", "T2", "F3", "T3"))) %>% 
    left_join(corekey %>% dplyr::select(core_name, water_treatment)) %>% 
    dplyr::select(source, core_name, water_treatment, timepoint, everything())
  
  
}



summaries = function(){
  
  
  test = import_xct_summaries(FILEPATH = "1-data/xct/csv-summary", PATTERN = ".csv")

  
  
  
  test_long_clean %>% 
    filter(!is.na(timepoint)) %>% 
    ggplot(aes(x = timepoint, y = total_volume_pores_mm3, color = water_treatment, group = core_name))+
    geom_point(size = 3)+
    geom_line()+
    facet_wrap(~water_treatment)
  
  
  
  test_long_clean %>% 
    filter(!is.na(timepoint)) %>% 
    #  filter(core_name == "FOR_03") %>% 
    ggplot(aes(x = timepoint, y = connected_pores_percent, color = water_treatment, group = core_name))+
    geom_point(size = 3)+
    geom_line()+
    #  ylim(75, 100)+
    facet_wrap(~water_treatment)
  
  
  test_long_clean %>% 
    filter(!is.na(timepoint)) %>% 
    ggplot(aes(x = timepoint, y = PNM_y_k_um2, color = water_treatment, group = core_name))+
    geom_point(size = 3)+
    geom_line()+
    facet_wrap(~water_treatment)
  
  test_long_clean %>% 
    filter(!is.na(timepoint)) %>% 
    ggplot(aes(x = timepoint, y = tortuosity_z, color = water_treatment, group = core_name))+
    geom_point(size = 3)+
    geom_line()+
    facet_wrap(~water_treatment)
  
  
  
  test_long_clean %>% 
    filter(!is.na(timepoint)) %>% 
    ggplot(aes(x = timepoint, y = flow_rate_z, color = water_treatment, group = core_name))+
    geom_point(size = 3)+
    geom_line()+
    facet_wrap(~water_treatment)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  test_long %>% filter(value == "Coordination Number") %>% print(n = 40)
  
  
  test_long_checking = 
    test_long %>% 
    mutate(numeric = as.numeric(value)) %>% 
    filter(is.na(numeric)) %>% 
    filter(!grepl("%", value)) %>% 
    group_by(value, ROW, name) %>% 
    dplyr::summarise(n = n())
  distinct(value, ROW, name)
  
  
  
  
  # -------------------------------------------------------------------------
  
  
  # -------------------------------------------------------------------------
  
}

# Pore Network Model data
import_xct_pnm = function(FILEPATH, PATTERN){
  
  filePaths_xct <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  xct_dat <- do.call(bind_rows, lapply(filePaths_xct, function(path) {
    df <- read.csv(path, skip = 1, na = "") # %>% mutate_all(as.character)
    #   colnames(df) = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K",
    #                     "L", "M", "N", "O", "P", "Q")
    df$source = basename(path)
    
    df}))
  
  
}
process_xct_pnm = function(xct_pnm){
  
  pnm_clean = 
    xct_pnm %>% 
    mutate(core_name = str_extract(source, "FOR_[0-9]{2}"),
           timepoint = str_extract(source, "F[0-9]|T[0-9]"),
           timepoint = str_remove_all(timepoint, "_"),
           timepoint = factor(timepoint, 
                              levels = c("T0", "F1", "T1", "F2", "T2", "F3", "T3"))) %>% 
    left_join(corekey %>% dplyr::select(core_name, water_treatment)) %>% 
    dplyr::select(source, core_name, water_treatment, timepoint, everything())
  
  
}


pnm = function(){
  
#  pnm = import_xct_pnm(FILEPATH = "1-data/xct/csv-pnm", PATTERN = ".csv")
  

  # plot-radius


  
  
  pnm_clean %>% 
    filter(grepl("T", timepoint)) %>% 
    ggplot(aes(x = timepoint, y = EqRadius, color = timepoint, group = timepoint)) +
    geom_violin()+
    geom_jitter(width = 0.1)+
    facet_wrap(~core_name)
  
  
  
  pnm_clean %>% 
    #     filter(grepl("T", timepoint)) %>% 
    ggplot(aes(x = EqRadius.1, color = timepoint, group = timepoint)) +
    geom_density()+
    facet_wrap(~core_name)+
    #      scale_color_manual(values = c("pink", "lightblue", "red", "blue", "darkred", "darkblue", "orange"))+
    #      xlim(-1, 10)+
    NULL
  
  
  pnm_clean %>% 
    filter(grepl("T", timepoint)) %>% 
    ggplot(aes(x = timepoint, y = Coordination.Number, color = timepoint, group = timepoint)) +
    geom_violin()+
    geom_jitter(width = 0.2, size = 0.2, alpha = 0.4)+
    facet_wrap(~core_name)
  
  
  pnm_clean %>% 
    filter(grepl("T", timepoint)) %>%
    filter(`Coordination.Number` == 0) %>% 
    ggplot(aes(x = EqRadius.1, color = timepoint, group = timepoint)) +
    geom_density()+
    #      geom_jitter(width = 0.2, size = 0.2, alpha = 0.4)+
    facet_wrap(~core_name)
  
  
  pnm_clean %>% 
    filter(grepl("T", timepoint)) %>%
    #  filter(`Coordination.Number` == 0) %>% 
    ggplot(aes(x = Coordination.Number, color = timepoint, group = timepoint)) +
    geom_density()+
    #      geom_jitter(width = 0.2, size = 0.2, alpha = 0.4)+
    facet_wrap(~core_name)
  
  
  pnm_clean %>% 
    filter(grepl("T", timepoint)) %>%
    filter(`Coordination.Number` == 0) %>% 
    group_by(core_name, timepoint) %>% 
    dplyr::summarise(n = n()) %>% 
    knitr::kable()
  
  ggplot(aes(x = EqRadius.1, color = timepoint, group = timepoint)) +
    geom_density()+
    #      geom_jitter(width = 0.2, size = 0.2, alpha = 0.4)+
    facet_wrap(~core_name)
  
  
}
