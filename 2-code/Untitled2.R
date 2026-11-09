
df = 
  xct_summary_processed %>% 
  dplyr::select(core_name, water_treatment, timepoint, contains("PNM")) %>% 
  mutate(XY = PNM_x_k_um2/PNM_y_k_um2,
         YZ = PNM_y_k_um2/PNM_z_k_um2,
         XZ = PNM_x_k_um2/PNM_z_k_um2,
         XYZ = XY/PNM_z_k_um2)


df %>% 
  filter(!(core_name == "FOR_04" & XYZ > 4000000)) %>% 
  ggplot(aes(y = XYZ, x = timepoint))+
  geom_line(aes(group = 1), linewidth = 1)+
  geom_point(size = 5, shape = 21, fill = "black", color = "white", stroke = 2)+
  facet_nested_wrap(~ water_treatment + core_name, scale = "free_y")+
  labs(y = "permeability (X/Y)/Z",
       title = " WITHOUT huge data point in FOR_04")+
  scale_y_continuous(labels = scales::label_comma())

df %>% 
   filter(!(core_name == "FOR_04" & XYZ > 4000000)) %>% 
  ggplot(aes(y = XYZ, x = timepoint, color = core_name))+
  geom_point()+
  geom_line(aes(group = core_name))+
  facet_nested_wrap(~ water_treatment, scale = "free_y")

options(scipen = 9999)
