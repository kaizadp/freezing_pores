library(tidyverse)
library(googlesheets4)

core_key = read_sheet("https://docs.google.com/spreadsheets/d/1qpfho6Z7aHYg9zkT0dMtQy7Oth5MkVtFc7O7rB45_t4/edit?gid=0#gid=0")
core_weights = read_sheet("https://docs.google.com/spreadsheets/d/1y1A7JxdqIZ__Y4OEJEBzo_k0ScwljTtKWfViN9PuT3g/edit?gid=0#gid=0")

gwc = tribble(
  ~"location", ~"gwc_percent",
  "forest", 39,
  "pasture", 32,
)

weights_processed = 
  core_weights %>% 
  left_join(core_key) %>% 
  left_join(gwc) %>% 
  mutate(soil_fm_g = round(soil_fm_g, 2),
         soil_od_g = round(soil_fm_g / ((gwc_percent/100) + 1), 2),
         soil_water_g = soil_fm_g - soil_od_g,
         target_water_g = case_when(water_treatment == "high" ~ soil_od_g * 60/100),
         add_water_g = target_water_g - soil_water_g,
         TARGET_WEIGHT_G = weight_sleeve_caps_soil_fm_g + add_water_g)

write_sheet(weights_processed,
            "https://docs.google.com/spreadsheets/d/19J4ZAUzMCDVZrQFXoziNXR8oMVrwYcr2wksLX09UGzo/edit?gid=0#gid=0",
            sheet = "calculation2")



soil_fm_g = 50
soil_od_g = round(soil_fm_g / ((40/100) + 1), 2)
soil_water_g = soil_fm_g - soil_od_g
target_water_g = soil_od_g * 60/100
add_water_g = target_water_g - soil_water_g
TARGET_WEIGHT_G = soil_fm_g + add_water_g
