# install.packages("sm")
library(sm)

sm.density.compare(x = dat$EqRadius,
                   group = dat$water_treatment,
                   model = "none")



dat = xct_pnm_processed %>% dplyr::select(core_name, water_treatment, timepoint, EqRadius) %>% 
  drop_na()
  

sm.density(x = EqRadius,
             group = water_treatment,
             model = "none")



## coordination numbers

y = 
  pnm_clean2 %>% 
  group_by(water_treatment, core_name, timepoint, Coordination.Number) %>% 
  dplyr::summarise(n = n())


pnm_clean2 %>% 
#  filter(core_name %in% c("FOR_01", "FOR_04")) %>% 
#  filter(grepl("T", timepoint)) %>% 
  ggplot(aes(x = Coordination.Number, color = timepoint, group = timepoint)) +
  geom_density(linewidth = 1, show.legend = F)+
  facet_wrap(~water_treatment + core_name)+
  #xlim(-5, 30)+
  scale_color_manual(values = c("black", "lightblue", "#dd4124", "blue", "darkred", "darkblue", "orange"),
                     breaks = c("T0", "F1", "T1", "F2", "T2", "F3", "T3"))+
  labs(x = "radius (mm)")+
  theme(panel.grid = element_blank())  



y %>% 
  drop_na() %>% 
  #  filter(core_name %in% c("FOR_01", "FOR_04")) %>% 
    filter(grepl("T", timepoint)) %>% 
  ggplot(aes(x = Coordination.Number, y = n, color = timepoint, group = timepoint)) +
  geom_line(linewidth = 1, show.legend = T)+
  facet_wrap(~water_treatment + core_name)+
  #xlim(-1, 50)+
  scale_color_manual(values = c("black", "lightblue", "#dd4124", "blue", "darkred", "darkblue", "orange"),
                     breaks = c("T0", "F1", "T1", "F2", "T2", "F3", "T3"))+
  scale_y_log10()+
  labs(x = "Coordination number")+
  theme(panel.grid = element_blank())  
