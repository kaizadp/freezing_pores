

library(tidyverse)
theme_set(theme_bw(base_size = 14))

test_jan10 = read.csv("82m-0563-202501081556-202501101732/xxx-xxxx-20250110000000_dense_summary.csv", skip = 1)

clean = 
  test_jan10 %>%
  janitor::clean_names() %>% 
  dplyr::select(date, time, port, co2_dry, fco2) %>% 
  mutate(datetime = paste(date, time),
         datetime = ymd_hms(datetime),
         datetime2 = as.POSIXct(datetime, tz = "EST"),
         co2_dry = as.numeric(co2_dry),
         fco2 = as.numeric(fco2))


x = 
  clean %>% 
 filter(datetime2 >= as.POSIXct("2025-01-10 03:00:00", tz = "EST") & 
          datetime2 <= as.POSIXct("2025-01-10 10:00:00", tz = "EST")) %>% 
  mutate(x = "x")


ggplot(x, 
       aes(x = datetime, y = co2_dry, color = port))+
  geom_path(data = x %>% filter(port == 4))+
  geom_point(size = 3)+
  ylim(450, 460)+
#  facet_wrap(~port)+
  NULL


clean %>% 
  filter(datetime2 > as.POSIXct("2025-01-10 03:00:00")) %>% 
  ggplot(aes(x = datetime, y = fco2, color = port))+
  geom_point()+
  #  facet_wrap(~port)+
  NULL


# -------------------------------------------------------------------------
# IMPORTING RAW DATA

import_licor_data = function(FILEPATH){
  
  filePaths <- list.files(path = FILEPATH, pattern = ".82z", full.names = TRUE, recursive = TRUE)
  licor_dat <- do.call(bind_rows, lapply(filePaths, function(path) {
    
    data = read.csv(unzip(path, "data.csv"), skip = 1) %>% dplyr::select(DATE, TIME, CO2_DRY, CH4_DRY)
    meta = jsonlite::read_json(unzip(path, "metadata.json"))$`LI-8250`$PORT
    
    data %>% mutate(port = meta)
    
  }))
}

file2 = import_licor_data(FILEPATH = "82m-0563-202501081556-202501101732/2025/01/15")


file2_processed = 
  file2 %>% 
  #mutate_all(as.numeric) %>% 
  janitor::clean_names() %>% 
  dplyr::select(date, time, port, co2_dry) %>% 
  mutate(datetime = paste(date, time),
         datetime = ymd_hms(datetime),
         datetime2 = as.POSIXct(datetime, tz = "EST"),
         co2_dry = as.numeric(co2_dry),
         port = as.character(port)) %>% 
  filter(!is.na(co2_dry))


pre_freeze = import_licor_data(FILEPATH = "82m-0563-202501081556-202501101732/2025/01/11") %>% mutate(timepoint = "1-prefreeze")
post_freeze = import_licor_data(FILEPATH = "82m-0563-202501081556-202501101732/2025/01/15") %>% mutate(timepoint = "2-postfreeze")

processed = 
  pre_freeze %>% 
  bind_rows(post_freeze) %>% 
  janitor::clean_names() %>% 
  dplyr::select(date, time, port, co2_dry, timepoint) %>% 
  mutate(datetime = paste(date, time),
         datetime = ymd_hms(datetime),
         datetime2 = as.POSIXct(datetime, tz = "EST"),
         co2_dry = as.numeric(co2_dry),
         port = as.character(port)) %>% 
  filter(!is.na(co2_dry)) %>% 
  filter(co2_dry >= 0) %>% 
  mutate(soil = case_match(port, "1" ~ "forest", "2" ~ "pasture", "4" ~ "ambient")) %>% 
  filter((datetime2 <= as.POSIXct("2025-01-11 10:00:00", tz = "EST") & datetime2 >= as.POSIXct("2025-01-11 08:00:00", tz = "EST")) | 
           datetime2 >= as.POSIXct("2025-01-15 08:00:00", tz = "EST"))


processed %>% 
  filter(port %in% c(1, 4))%>% 
  ggplot(
    aes(x = datetime, y = co2_dry, color = soil))+
  # geom_path(data = file2_processed %>% filter(port == 4))+
  geom_point(size = 1, alpha = 0.4)+
  facet_wrap(~timepoint, scales = "free_x")+
  NULL  

processed %>% 
  filter(port %in% c(2, 4))%>% 
  ggplot(
    aes(x = datetime, y = co2_dry, color = soil))+
  # geom_path(data = file2_processed %>% filter(port == 4))+
  geom_point(size = 1, alpha = 0.4)+
  facet_wrap(~timepoint, scales = "free_x")+
  NULL  
