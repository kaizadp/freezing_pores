
options(scipen = 9999)

# gg_pore_vol = 
  xct_summary2 %>% 
  filter(!is.na(timepoint)) %>% 
    filter(grepl("T", timepoint)) %>% 
  ggplot(aes(x = timepoint, y = flow_rate_z, 
             color = core_name, fill = core_name, group = core_name))+
  geom_line(linewidth = 1)+
  geom_point(size = 5, color = "white", shape = 21, stroke = 2)+
#    geom_text(data = xct_summary2 %>% filter(timepoint == "T0") %>% mutate(label2 = parse_number(core_name)),
#              aes(label = label2),
#              nudge_x = -0.35, nudge_y = 0.3, size = 4)+
  scale_color_manual(values = pal_cores)+
  scale_fill_manual(values = pal_cores)+
  facet_wrap(~water_treatment)+
  labs(#y = "Total pore volume (mm3)",
       x = "")+
  theme(legend.position = "none")
  
  
  xct_summary2 %>% 
    filter(!is.na(timepoint)) %>% 
    filter(grepl("T", timepoint)) %>% 
    ggplot(aes(x = timepoint, y = PNM_z_k_um2, 
               color = core_name, fill = core_name, group = core_name))+
    geom_line(linewidth = 1)+
    geom_point(size = 5, color = "white", shape = 21, stroke = 2)+
    #    geom_text(data = xct_summary2 %>% filter(timepoint == "T0") %>% mutate(label2 = parse_number(core_name)),
    #              aes(label = label2),
    #              nudge_x = -0.35, nudge_y = 0.3, size = 4)+
    scale_color_manual(values = pal_cores)+
    scale_fill_manual(values = pal_cores)+
    facet_wrap(~water_treatment)+
    labs(#y = "Total pore volume (mm3)",
      x = "")+
    theme(legend.position = "none")
  
  xct_summary2 %>% 
    filter(!is.na(timepoint)) %>% 
    filter(grepl("T", timepoint)) %>% 
    ggplot(aes(x = timepoint, y = tortuosity_z, 
               color = core_name, fill = core_name, group = core_name))+
    geom_line(linewidth = 1)+
    geom_point(size = 5, color = "white", shape = 21, stroke = 2)+
    #    geom_text(data = xct_summary2 %>% filter(timepoint == "T0") %>% mutate(label2 = parse_number(core_name)),
    #              aes(label = label2),
    #              nudge_x = -0.35, nudge_y = 0.3, size = 4)+
    scale_color_manual(values = pal_cores)+
    scale_fill_manual(values = pal_cores)+
    facet_wrap(~water_treatment)+
    labs(#y = "Total pore volume (mm3)",
      x = "")+
    theme(legend.position = "none")
  
  
dunnett_cleaning = 
  xct_summary2 %>%
  filter(!is.na(timepoint)) %>% 
  
  pivot_longer(cols = contains(c("_x", "_y", "_z"))) %>% 
  mutate(direction = str_extract(name, "_x|_y|_z")) %>% 
  separate(name, sep = "_", into = c("variable"), remove = F) %>% 
  dplyr::select(core_name, water_treatment, timepoint, variable, direction, value) %>% 
#  pivot_wider(names_from = "timepoint", values_from = "value") %>% 
  force()


fit_dunnett = function(dat){
  
  d <-DescTools::DunnettTest((value) ~ timepoint, control = "T0", data = dat)
  d$T0 %>% data.frame() %>% rownames_to_column("comparison")
      ##      #create a tibble with one column for each treatment
      ##      # column 4 has the pvalue
      ##      t = tibble(`Drought` = d$`Time Zero`["Drought-Time Zero",4], 
      ##                 `Saturated` = d$`Time Zero`["Saturated-Time Zero",4],
      ##                 `Field Moist` = d$`Time Zero`["Field Moist-Time Zero",4])
      ##      # we need to convert significant p values to asterisks
      ##      # since the values are in a single row, it is tricky
      ##      t %>% 
      ##        # first, gather all p-values into a single column, pval
      ##        gather(trt, pval, 1:3) %>% 
      ##        # conditionally replace all significant pvalues (p<0.05) with asterisks and the rest remain blank
      ##        mutate(p = if_else(pval<0.05, "*","")) %>% 
      ##        # remove the pval column
      ##        dplyr::select(-pval) %>% 
      ##        # spread the p (asterisks) column bnack into the three columns
      ##        spread(trt, p)  ->
      ##        t
  
  
}  


x = dunnett_cleaning %>% 
 # filter(variable == "PNM" & direction == "_y") %>% 
  group_by(core_name, variable) %>% 
  do(fit_dunnett(.))




fit_hsd <- function(dat) {
  a <-aov(value ~ timepoint, data = dunnett_cleaning)
  h <-agricolae::HSD.test(a,"timepoint")
  h$groups %>% data.frame() %>% rownames_to_column("timepoint")
        ##  #create a tibble with one column for each treatment
        ##  #the hsd results are row1 = drought, row2 = saturation, row3 = time zero saturation, row4 = field moist. hsd letters are in column 2
        ##  tibble(`100` = h$groups["100",2], 
        ##         `50` = h$groups["50",2],
        ##         `FM` = h$groups["FM",2])
}


y = dunnett_cleaning %>% 
  # filter(variable == "PNM" & direction == "_y") %>% 
  group_by(core_name, variable, direction) %>% 
  do(fit_hsd(.))



fit_lme = function(dat){
  nlme::lme(value ~ timepoint, random = ~1|core_name, data = dat) %>% anova()

  
}

z = dunnett_cleaning %>% 
  filter(grepl("T", timepoint)) %>% 
  group_by(variable, direction, water_treatment) %>% 
  do(fit_lme(.))


dunnett_cleaning_wide = 
  dunnett_cleaning %>%
  pivot_wider(names_from = "direction")
  

