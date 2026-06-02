
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
  


# -------------------------------------------------------------------------




# kolmogorov-smirnoff test
# 

data1 = pnm_clean2 %>% filter(core_name == "FOR_06" & timepoint == "T0") %>% pull(EqRadius)
data2 = pnm_clean2 %>% filter(core_name == "FOR_06" & timepoint == "T3") %>% pull(EqRadius)
ks.test(data1, data2)



##  PAIRWISE KOLMOGOROV-SMIRNOV
devtools::install_github("happyrabbit/DataScienceR")
library(DataScienceR)

dat = 
pnm_clean2 %>% 
  filter(core_name == "FOR_01") 

x = 
  pnm_clean2


do_pairwise_ks_test = function(dat){
  
  pairwise_ks_test(value = dat$EqRadius,
                    group = dat$timepoint) %>% 
    as.data.frame() %>% 
    rownames_to_column("time1")
  
  
}

x = 
  pnm_clean2 %>% 
  group_by(water_treatment, core_name) %>% 
  do(do_pairwise_ks_test(.)) %>% 
  pivot_longer(cols = -c(water_treatment, core_name, time1), values_to = "p", names_to = "time2") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(time1 != time2) %>% 
  mutate(asterisk = case_when(p <= 0.05 ~ "*"))

x_wide = 
  x %>% 
  dplyr::select(-p) %>% 
  pivot_wider(names_from = "time2", values_from = "asterisk")



## trying compact letter display (like HSD) for pairwise -- DIDN'T WORK
multcomp:::insert_absorb(x_wide, 
                         decreasing=FALSE, 
                         comps=x_wide, 
                         lvl_order=lvl_order)


library(rcompanion)
library(multcompView)

data <- data.frame(group = rep(letters[18:26], each = 10),
                   var = rnorm(180, mean = 2, sd = 5))

PT <- pairwise.t.test(x=data$var, g=data$group, p.adjust.method = "none")#just to make sure i get some sigs for the example
PT = PT$p.value
PT1 = fullPTable(PT)
multcompLetters(PT1,
                compare="<",
                threshold=0.05,
                Letters=letters,
                reversed = FALSE)



x_wide2 = 
  x %>% 
  dplyr::select(-asterisk) %>% 
  mutate(p = case_when(p <= 0.05 ~ 0.01,
                       p > 0.05 ~ p)) %>% 
  pivot_wider(names_from = "time2", values_from = "p") %>% 
  filter(core_name == "FOR_04") %>% 
  column_to_rownames("time1") %>% 
  dplyr::select(-water_treatment, -core_name) 

x_wide2[is.na(x_wide2)] <- 1
x_wide2 = 
  x_wide2 %>% 
  as.matrix()

multcompLetters(data = x_wide2,
                compare=">",
                threshold=0.05,
                Letters=letters,
                reversed = FALSE)

###############
###############

## PAIRWISE KOLMOGOROV-SMIRNOV

assign_letters = function(df){
  
  letters = multcompLetters(setNames(as.numeric(df$p), df$comparison),
                            compare="<",
                            threshold=0.05,
                            #Letters=letters,
                            reversed = FALSE)
  
  letters$Letters %>% as.data.frame() %>% rownames_to_column("x")
  
}

df =  x %>% 
  filter(!is.na(p)) %>% 
  mutate(comparison = paste0(time1, "-", time2)) %>% 
  dplyr::select(core_name, comparison, p) %>% 
  #  pivot_wider(names_from = "comparison", values_from = "p") %>% 
  ungroup() %>% 
  
  #  filter(water_treatment == "low water") %>%
  # filter(core_name == "FOR_01") %>% 
  group_by(core_name) %>% 
  do(assign_letters(.))












## TRYING TO PLOT THE SIGNIFICANT PAIRS
x2 = 
  x %>% 
  mutate(number1 = recode(time1, T0 = 100, T1 = 10, T2 = 20, T3 = 30, F1 = 1, F2 = 2, F3 = 3),
         number2 = recode(time2, T0 = 100, T1 = 10, T2 = 20, T3 = 30, F1 = 1, F2 = 2, F3 = 3),
         number = number1 + number2) %>% 
  distinct(number, .keep_all = T) %>% 
  mutate(time1 = factor(time1, levels = c("T0", "F1", "T1", "F2", "T2", "F3", "T3")),
         time2 = factor(time2, levels = c("T0", "F1", "T1", "F2", "T2", "F3", "T3"))) 


x2 %>% 
  filter(!is.na(p)) %>% 
  ggplot(aes(x = time2, y = time1, fill = asterisk))+
  geom_tile(color = "black", size = 0.5, show.legend = F)+
  geom_text(aes(label = asterisk), color = "black", size = 7)+
  facet_wrap(~core_name)+
  scale_fill_manual(values = "darkorange", na.value = "grey50")+
  labs(x = "", y = "", 
       title = "Pairwise Kolmogorov-Smirnov Tests for Equivalent Radius
       
       ")+
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

#



pairwise_ks_test(value, group, n_min = 50, warning = 0,
                 alternative = "two.sided")




pnm_clean2 %>% 
  group_by(water_treatment, core_name, timepoint) %>% 
  dplyr::summarise(mean = round(mean(EqRadius, na.rm = T), 3),
                   median = round(median(EqRadius, na.rm = T), 3),
                   max = round(max(EqRadius, na.rm = T), 3),
                   min = round(min(EqRadius, na.rm = T), 3)) %>% 
  knitr::kable()


pnm_clean2 %>% 
  group_by(water_treatment, core_name, timepoint) %>% 
  dplyr::summarise(mean = round(mean(Volume, na.rm = T), 3),
                   median = round(median(Volume, na.rm = T), 3),
                   max = round(max(Volume, na.rm = T), 3),
                   min = round(min(Volume, na.rm = T), 3)) %>% 
  knitr::kable()
