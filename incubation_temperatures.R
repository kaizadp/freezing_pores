
library(tidyverse)
library(googlesheets4)
theme_set(theme_bw(base_size = 14))

temp = read_sheet("https://docs.google.com/spreadsheets/d/1D3BQC-fHfAD2RuZekwNQ0p6J0GxeJrlaV5f4b7NIQa8/edit?gid=0#gid=0")

temp_processed = 
  temp %>% 
  mutate(datetime = paste(Date, Time),
         datetime = ymd_hms(datetime))

temp_processed %>% 
  ggplot(aes(x = datetime, y = Value, color = soil_type))+
  geom_line()
