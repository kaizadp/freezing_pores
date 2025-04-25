## functions to import and process respiration data
## 
## 

library(tidyverse)
#library(googledrive)
theme_set(theme_bw(base_size = 14))

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

process_licor_data = function(licor_data, licor_map, corekey){
  
  licor_columns = 
    licor_data %>% 
    janitor::clean_names() %>% 
    dplyr::select(date, time, port1, port2, co2_dry, ch4_dry) %>% 
    mutate(datetime = paste(date, time),
           datetime = ymd_hms(datetime),
           # datetime2 = as.POSIXct(datetime, tz = ""),
           co2_dry = as.numeric(co2_dry),
           port = case_when(!is.na(port2) ~ paste0(port1, "_", port2),
                            .default = as.character(port1))) %>% 
    dplyr::select(-port1, -port2) %>% 
    filter(!is.na(co2_dry)) %>% 
    filter(co2_dry >= 0) 
  
  licor_map = 
    licor_map %>% 
    mutate(start_date_time = ymd_hm(start_date_time),
           stop_date_time = ymd_hm(stop_date_time))
  
  licor_subset = 
    subset(merge(licor_columns, licor_map %>% dplyr::select(start_date_time, stop_date_time, port, core_name)), 
           datetime <= stop_date_time & datetime >= start_date_time) 
    
  licor_processed_ppm = 
    licor_subset %>% 
    left_join(corekey %>% dplyr::select(core_name, water_treatment)) %>% 
    mutate(water_treatment = paste(water_treatment, "water"),
           water_treatment = case_when(grepl("NA", water_treatment) ~ "ambient", 
                                       .default = water_treatment)) %>% 
    rename(co2_ppm = co2_dry,
           ch4_ppb = ch4_dry) %>% 
    filter(co2_ppm <= 700) %>% 
    arrange(date, time) %>% 
    mutate(timepoint = case_when(datetime >= ymd_hms("2025-02-18 12:30:00") ~ "ftc3",
                                 datetime >= ymd_hms("2025-02-11 12:30:00") ~ "ftc2",
                                 datetime >= ymd_hms("2025-02-04 12:30:00") ~ "ftc1",
                                 datetime >= ymd_hms("2025-01-28 12:30:00") ~ "t0")) %>% 
    filter(!is.na(timepoint))
  
  licor_processed_ppm
}

fit_slope = function(licor_processed_ppm){
  
  # Whenever the valve position changes, that's a new sample starting
  newsample <- licor_processed_ppm$port != c(NA, head(licor_processed_ppm$port, -1))
  newsample[is.na(newsample)] <- FALSE
  licor_processed_ppm$sample_number = cumsum(newsample)
  
  rate = 
    licor_processed_ppm %>% 
    mutate(datetime = as_datetime(paste(date, time))) %>% 
    group_by(sample_number) %>% 
    dplyr::mutate(elapsed_sec = as.double(difftime(datetime, min(datetime), units = "secs"))) %>% 
    group_by(core_name, water_treatment, sample_number) %>% 
    dplyr::summarize(datetime = min(datetime),
                     co2_ppm_s = lm(co2_ppm ~ elapsed_sec)$coefficients["elapsed_sec"],
                     ch4_ppb_s = lm(ch4_ppb ~ elapsed_sec)$coefficients["elapsed_sec"],
                     co2_max_ppm = max(co2_ppm),
                     sample_elapsed = max(elapsed_sec))  %>% 
    filter(sample_elapsed < 200)  %>% 
    group_by(core_name, timepoint) %>% 
    dplyr::mutate(elapsed_minutes = as.double(difftime(datetime, min(datetime), units = "mins")))
  
  
      ##  rate %>% 
      ##    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
      ##    ggplot(aes(x = datetime, y = co2_ppm_s, color = core_name))+
      ##    geom_point()+
      ##    facet_wrap(~water_treatment, ncol = 1)+
      ##    #  ylim(440, 600)+
      ##    NULL 
      ##  
      ##  
      ##  rate %>% 
      ##    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
      ##    ggplot(aes(x = datetime, y = ch4_ppb_s, color = core_name))+
      ##    geom_point()+
      ##    facet_wrap(~water_treatment, ncol = 1)+
      ##    #  ylim(440, 600)+
      ##    NULL 
      ##  
      ##  ## 8 hours of GHG
  
  rate
  
}


compute_rates <- function(licor_processed_ppm, volume_cm3 = 81.07, tair_C = 21, pressure_kPa = 101.325) {
  
    ## We want to compute rate of change (CO2 ppm/s and CH4 ppb/s),
    ## and then convert this to µmol/s using the ideal gas law:
    ## A = dC/dt * V * Pa/RT (cf. Steduto et al. 2002), where
    ## 	A is the flux (µmol/g/s)
    ##	  dC/dt is raw respiration as above (mole fraction/s)
    ## 	V is total chamber volume (cm3)
    ##	  Pa is atmospheric pressure (kPa)
    ##	  R is universal gas constant (8.3 x 10^3 cm3 kPa mol-1 K-1)
    ##	  T is air temperature (K)
  
  R <- 8.3145e+3			# cm3 kPa K−1 mol−1

  # Whenever the valve position changes, that's a new sample starting
  newsample <- licor_processed_ppm$port != c(NA, head(licor_processed_ppm$port, -1))
  newsample[is.na(newsample)] <- FALSE
  licor_processed_ppm$sample_number = cumsum(newsample)
  
  rate = 
    licor_processed_ppm %>% 
    mutate(datetime = as_datetime(paste(date, time))) %>% 
    group_by(sample_number) %>% 
    dplyr::mutate(elapsed_sec = as.double(difftime(datetime, min(datetime), units = "secs"))) %>% 
    group_by(core_name, water_treatment, sample_number, timepoint) %>% 
    dplyr::summarize(datetime = min(datetime),
                     co2_ppm_s = lm(co2_ppm ~ elapsed_sec)$coefficients["elapsed_sec"],
                     ch4_ppb_s = lm(ch4_ppb ~ elapsed_sec)$coefficients["elapsed_sec"],
                     co2_max_ppm = max(co2_ppm),
                     sample_elapsed = max(elapsed_sec))  %>% 
    filter(sample_elapsed < 200)  %>% 
    group_by(core_name, timepoint) %>% 
    dplyr::mutate(elapsed_hr = as.double(difftime(datetime, min(datetime), units = "hours")),
                  # Respiration, µmol/time unit via ideal gas law
                  co2_umol_s = co2_ppm_s * volume_cm3 * pressure_kPa / (R * (tair_C + 273.15)),
                  co2_umol_hr = co2_umol_s * 3600
                  )

              # Compute ppm/time unit change in gas concentration
              #m <- lm(gas_ppm ~ time)
              #gas_ppm_time <- unname(coefficients(m)["time"])
}
cumulative_evolution = function(licor_processed_rates){
  
  fn = function(licor_processed_rates){
    co2_interp = 
      approx(licor_processed_rates$elapsed_hr, 
             licor_processed_rates$co2_umol_hr, 
             ties = mean, rule = 2) %>% 
      as.tibble() %>% 
      mutate(delta_hr = x - lag(x),
             co2_umol = y * delta_hr) %>% 
      summarize(co2_umol_cum = sum(co2_umol, na.rm = T)) %>% 
      force()
  }
  
  
  cumulative = 
    licor_processed_rates %>% 
    filter(elapsed_hr < 20) %>% 
    filter(elapsed_hr > 0) %>% 
    group_by(water_treatment, core_name, timepoint) %>% 
    do(fn(.))
  
}




# -------------------------------------------------------------------------



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
# old graphs  
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


more = function(){
  
  
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
  
  
  
  
  
  processed_new %>% 
    filter(datetime >= ymd_hms("2025-02-05 13:00:00") &
             datetime <= ymd_hms("2025-02-05 15:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
    ggplot(aes(x = elapsed_sec, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~sample_number, scales = "free_x")+
    #  ylim(440, 600)+
    NULL 
  
  
  processed_new %>% 
    filter(datetime >= ymd_hms("2025-02-11 13:00:00") &
             datetime <= ymd_hms("2025-02-11 15:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
    ggplot(aes(x = elapsed_sec, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~sample_number, scales = "free_x")+
    #  ylim(440, 600)+
    NULL 
  
  
  
  
  
  processed_new %>% 
    filter(datetime >= ymd_hms("2025-02-18 13:00:00") &
             datetime <= ymd_hms("2025-02-18 15:05:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient")) %>% 
    ggplot(aes(x = elapsed_sec, y = co2_dry, color = core_name))+
    geom_point()+
    facet_wrap(~sample_number, scales = "free_x")+
    #  ylim(440, 600)+
    NULL  
  
  
  
  test = 
    processed_new %>% 
    filter(datetime >= ymd_hms("2025-02-18 13:00:00") &
             datetime <= ymd_hms("2025-02-18 14:00:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21", "ambient"))
  
  
  
  test_slope = 
    test %>% 
    group_by(core_name, sample_number) %>% 
    dplyr::summarize(slope = lm(co2_dry ~ elapsed_sec)$coefficients["elapsed_sec"])
  

  
}



more = function(){
licor_processed_ppm %>% 
  filter(datetime >= ymd_hms("2025-02-18 13:00:00") & datetime <= ymd_hms("2025-02-18 19:00:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = co2_ppm, color = core_name))+
  geom_line()+
  facet_wrap(~water_treatment, ncol = 1)
  
  
  
  
  licor_processed_rates %>% 
   # filter(datetime >= ymd_hms("2025-02-18 13:00:00") & datetime <= ymd_hms("2025-02-18 19:00:00")) %>% 
    filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
    ggplot(aes(x = datetime, y = co2_max_ppm, color = core_name))+
    geom_line()+
    facet_wrap(~water_treatment, ncol = 1)

licor_processed_ppm %>% 
  filter(datetime >= ymd_hms("2025-01-27 13:00:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = datetime, y = ch4_ppb, color = core_name))+
  geom_line()+
  facet_wrap(~water_treatment, ncol = 1)



test = 
  licor_processed_rates %>% 
  mutate(timepoint = case_when(datetime >= ymd_hms("2025-02-18 12:30:00") ~ "ftc3",
                               datetime >= ymd_hms("2025-02-11 12:30:00") ~ "ftc2",
                               datetime >= ymd_hms("2025-02-04 12:30:00") ~ "ftc1",
                               datetime >= ymd_hms("2025-01-28 12:30:00") ~ "t0"))


licor_processed_rates %>% 
  mutate(timepoint = factor(timepoint, levels = c("t0", "ftc1", "ftc2", "ftc3"))) %>% 
  filter(water_treatment != "ambient") %>% 
  filter(!is.na(timepoint)) %>% 
  filter(elapsed_minutes < 1200) %>% 
  # filter(datetime >= ymd_hms("2025-02-18 13:00:00") & datetime <= ymd_hms("2025-02-18 19:00:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = elapsed_minutes/60, y = co2_max_ppm, group = core_name, color = water_treatment))+
  geom_line(alpha = 0.4)+
  geom_smooth(se = F, aes(group = water_treatment))+
  facet_grid(. ~ timepoint, scales = "free_x")


licor_processed_rates %>% 
  mutate(timepoint = factor(timepoint, levels = c("t0", "ftc1", "ftc2", "ftc3"))) %>% 
  filter(water_treatment != "ambient") %>% 
  filter(!is.na(timepoint)) %>% 
  filter(elapsed_minutes < 1200) %>% 
  # filter(datetime >= ymd_hms("2025-02-18 13:00:00") & datetime <= ymd_hms("2025-02-18 19:00:00")) %>% 
  filter(!core_name %in% c("FOR_20", "FOR_21")) %>% 
  ggplot(aes(x = elapsed_minutes/60, y = co2_max_ppm, group = core_name, color = water_treatment))+
  geom_line()+
#  geom_smooth(se = F, aes(group = water_treatment))+
  geom_hline(yintercept = 450)+
  facet_grid(water_treatment+core_name ~ timepoint, scales = "free_x")




}