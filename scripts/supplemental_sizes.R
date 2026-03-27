#pycnopie

set_flextable_defaults(
  font.size = 11, font.family = "Arial",
  font.color = "#333333",
  table.layout = "fixed",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4)

###############################################
#organism sizes
###############################################


#average urchin sizes
urchin_roster <- read_csv("data/raw/exp2_urchin_roster.csv") 

urchin_size <- urchin_roster %>% 
  group_by(Treatment) %>%
  summarise(mean_size_cm = mean(Size_cm, na.rm = TRUE), 
            sd_size_cm = sd(Size_cm, na.rm = TRUE)) %>%
  ungroup() %>%
  data.frame() %>%
  mutate(Predator_treatment = sub("(.*)_.*", "\\1", Treatment),
         Urchin_treatment = sub(".*_(.*)", "\\1", Treatment), .before = mean_size_cm) %>%
  mutate(Urchin_treatment = ifelse(Urchin_treatment == "KF", "Kelp Forest","Barren")) %>%
  rename("Urchin treatment" = Urchin_treatment,
         "Predator treatment" = Predator_treatment) %>%
  select(-Treatment)

#flextable formatting for urchin size table
flextable(urchin_size) %>%
  #header formatting
  add_header_lines(values = "Experiment 2") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels(mean_size_cm = 'Mean test size (cm)',
                    sd_size_cm = 'SD of test sizes (cm)') %>%
  italic(part = "all", j = c(1,2)) %>%
  #body formatting
  colformat_double(digits = 2) %>% #setting number of decimal places
  
  autofit() 


#average pycno sizes
pycno_roster <- read_csv("data/raw/exp2_pycno_roster.csv") %>%
  mutate(Urchin_treatment = ifelse(Urchin_treatment == "B", "Barren","Kelp Forest"))  %>%
  group_by(Urchin_treatment) %>%
  summarise(radius_mean_cm=mean(Radius_cm, na.rm = TRUE), 
            radius_sd_cm=sd(Radius_cm, na.rm = TRUE),
            wet_weight_mean_g=mean(Wet_weight_g, na.rm = TRUE),
            wet_weight_sd_g=sd(Wet_weight_g, na.rm = TRUE)) %>%
  ungroup() %>%
  data.frame()

#flextable formatting for pycno size table
flextable(pycno_roster) %>%
  #header formatting
  add_header_lines(values = "Experiment 2") %>%
  italic(i = 1, part = "header") %>%
  bold(i = 2, part = "header") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  set_header_labels(radius_mean_cm = 'Mean Radius (cm)',
                    Urchin_treatment = "Urchin treatment",
                    radius_sd_cm = 'SD of Radius (cm)',
                    wet_weight_mean_g= "Mean Wet Weight (g)",
                    wet_weight_sd_g= "SD of Wet Weight (g)") %>%
  italic(part = "all", j = 1) %>%
  #body formatting
  colformat_double(digits = 2) %>% #setting number of decimal places
  
  autofit() 

