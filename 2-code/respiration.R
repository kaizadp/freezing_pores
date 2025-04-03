## functions to import and process respiration data
## 
## 

library(tidyverse)
#library(googledrive)
theme_set(theme_bw(base_size = 14))


licor_map = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1CkMjaIOUSHGJJloa4W4ILtFO3CVoeRCgK3Cf1GTom5o/", sheet = "Sheet1",
                                 col_types = "c")

import_licor_data = function(FILEPATH){

  filePaths <- list.files(path = FILEPATH, pattern = ".82z", full.names = TRUE, recursive = TRUE)
  licor_dat <- do.call(bind_rows, lapply(filePaths, function(path) {
    
    data = read.csv(unzip(path, "data.csv"), skip = 1) %>% dplyr::select(DATE, TIME, CO2_DRY, CH4_DRY)
    port1_value = jsonlite::read_json(unzip(path, "metadata.json"))$`LI-8250`$PORT
    port2_value = jsonlite::read_json(unzip(path, "metadata.json"))$`8250-01`$PORT
    
    data %>% mutate(port1 = port1_value,
                    port2 = port2_value,
                    source = basename(path))
    
  }))
  
}

# licor_data = import_licor_data(FILEPATH = "1-data/respiration_subset")

process_licor_data = function(licor_data){
  
  processed1 = 
    licor_data %>% 
    #mutate_all(as.numeric) %>% 
    janitor::clean_names() %>% 
    dplyr::select(date, time, port1, port2, co2_dry) %>% 
    mutate(datetime = paste(date, time),
           datetime = ymd_hms(datetime),
          # datetime2 = as.POSIXct(datetime, tz = ""),
           co2_dry = as.numeric(co2_dry),
           port = case_when(!is.na(port2) ~ paste0(port1, "_", port2),
                            .default = as.character(port1))) %>% 
    dplyr::select(-port1, -port2) %>% 
    filter(!is.na(co2_dry)) %>% 
    filter(co2_dry >= 0) 
  
  licor_map = licor_map %>% 
    mutate(start_date_time = ymd_hm(start_date_time),
           stop_date_time = ymd_hm(stop_date_time))
  
  
  processed2 = 
    subset(merge(processed1, licor_map %>% dplyr::select(start_date_time, stop_date_time, port, core_name)), 
           datetime <= stop_date_time & datetime >= start_date_time) 
    
  
processed = 
  processed2 %>% 
    left_join(core_key %>% dplyr::select(core_name, water_treatment)) %>% 
    mutate(water_treatment = paste(water_treatment, "water")) %>% 
  filter(co2_dry <= 700)
  
}

random = function(){
  
  targets::tar_load(c(corekey, licor_map))
  
  
  processed %>% 
    ggplot(aes(x = datetime, y = co2_dry))+
    geom_line()+
    facet_wrap(~port)+
   # ylim(440, 600)+
    NULL  
  

    
}


x = function(){
  
  gg1 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-01-22 13:00:00") &
             datetime <= ymd_hms("2025-01-22 18:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL 
  
  
  gg2 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-01-28 13:00:00") &
             datetime <= ymd_hms("2025-01-28 18:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL 
  
  
  gg3 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-02-04 13:00:00") &
             datetime <= ymd_hms("2025-02-04 18:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL  
  
  ggpubr::ggarrange(gg2, gg3, common.legend = TRUE, legend = "top")
  
}


x = function(){
  
  gg1 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-01-22 13:00:00") &
             datetime <= ymd_hms("2025-01-22 15:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
    #facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL 
  
  
  gg2 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-01-28 13:00:00") &
             datetime <= ymd_hms("2025-01-28 15:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
  #  facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL 
  
  
  gg3 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-02-04 13:00:00") &
             datetime <= ymd_hms("2025-02-05 07:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
  #  facet_wrap(~water_treatment, ncol = 1)+
    ylim(440, 600)+
    NULL  
  
  
}



x = function(){
  
  gg1 = 
    processed %>% 
    filter(datetime >= ymd_hms("2025-01-22 13:05:00") & datetime <= ymd_hms("2025-01-24 00:05:00") 
    ) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
    facet_wrap(~water_treatment)+
    ylim(440, 600)+
    NULL 
  
  
  gg2 = 
    processed %>% 
    filter(
             (datetime >= ymd_hms("2025-01-28 13:05:00") & datetime <= ymd_hms("2025-01-30 00:05:00")) 
    ) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
    facet_wrap(~water_treatment, nrow = 1)+
    ylim(440, 600)+
    NULL 
  
  gg3 = 
    processed %>% 
    filter((datetime >= ymd_hms("2025-02-04 13:05:00") & datetime <= ymd_hms("2025-02-07 00:05:00"))
    ) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
    geom_point(size = 0.5)+
    facet_wrap(~water_treatment, nrow = 1)+
    ylim(440, 600)+
    NULL 
  
  
  ggpubr::ggarrange(gg2, gg3, ncol = 1, common.legend = TRUE, legend = "top")
  
  
  
  
  processed %>% 
    filter(datetime >= ymd_hms("2025-02-04 12:05:00")
    ) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry))+
    geom_point(size = 0.5)+
#    geom_path()+
    facet_wrap(~core_name)+
 #   ylim(440, 600)+
    NULL 
 
  
  processed %>% 
#    filter(datetime >= ymd_hms("2025-02-04 12:05:00")) %>% 
    filter(core_name %in% c("FOR_20", "FOR_21")) %>% 
#    filter(!core_name %in% "ambient") %>% 
    ggplot(aes(x = datetime, y = co2_dry))+
    geom_point(size = 0.5)+
    #    geom_path()+
    facet_wrap(~water_treatment, nrow = 2)+
    ylim(440, 600)+
    NULL 
  
   
}



processed %>% 
  filter(datetime >= ymd_hms("2025-02-11 13:00:00") &
           datetime <= ymd_hms("2025-02-13 10:05:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
  geom_point()+
  facet_wrap(~water_treatment, ncol = 1)+
#  ylim(440, 600)+
  NULL  

processed %>% 
  filter(datetime >= ymd_hms("2025-02-11 13:00:00") &
           datetime <= ymd_hms("2025-02-12 10:05:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
  geom_point()+
  facet_wrap(~water_treatment, ncol = 1)+
  #  ylim(440, 600)+
  NULL  


processed %>% 
  filter(datetime >= ymd_hms("2025-02-18 13:00:00") &
           datetime <= ymd_hms("2025-02-20 10:05:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
  geom_point()+
  facet_wrap(~water_treatment, ncol = 1)+
  #  ylim(440, 600)+
  NULL  


processed %>% 
  filter(datetime >= ymd_hms("2025-02-18 13:00:00") &
           datetime <= ymd_hms("2025-02-19 10:05:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = co2_dry, color = core_name))+
  geom_point()+
  facet_wrap(~water_treatment, ncol = 1)+
  #  ylim(440, 600)+
  NULL  
