#run behavior_cleaning first

behavior_processed <- behavior_cleaned %>%
  
  #removing position for now - might want to change later - changing to scale of day/num record
  select(-urchin_position, -urchin_location_count) %>%
  unique() %>% 
  
  
  #adding distance to pycno
  mutate(dist_slice_pyc1 = abs(urchin_slice - pycno1_slice),
         dist_slice_pyc2 = abs(urchin_slice - pycno2_slice)) %>%
  
  #adding upstream/downstream variable from urchin POV given pycno position
  mutate(up_dn_pyc1 = ifelse(urchin_slice < pycno1_slice , 
                             "upstream",
                             "downstream"),
         up_dn_pyc2 = ifelse(urchin_slice < pycno2_slice , 
                             "upstream",
                             "downstream")) %>%
  
  
  #changing up/dn to up for non pycno treatments (bc has to be something other than NA for model)
  mutate(up_dn_pyc1 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc1),
         up_dn_pyc2 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc2)) %>%
  

  #adding variance of pie slices + crevice use at day/numrecord scale. currently, num urch on kelp is at the 
  group_by(Trial, Treatment, Day_numrecord) %>%
  mutate(variance_of_urch_distrib = var(urchin_slice_count, na.rm = TRUE), #one value per tank
         var_to_mean = variance_of_urch_distrib/mean(urchin_slice_count),
         pcnt_in_crev = sum(crev_count, na.rm=TRUE)/Total_num_urchin *100,
         pcnt_on_kelp = mean(Num_urch_on_kelp, na.rm = TRUE)/Total_num_urchin *100) %>%
  ungroup() %>%
  
  #averaged between time points (trial scale)
  group_by(Trial, Treatment) %>%
  mutate(trial_avg_pcnt_in_crev = mean(pcnt_in_crev),
         trial_sd_pcnt_in_crev = sd(pcnt_in_crev, na.rm = TRUE),
         trial_avg_var_to_mean = mean(var_to_mean),
         trial_avg_pcnt_on_kelp = mean(pcnt_on_kelp),
         trial_sd_pcnt_on_kelp = sd(pcnt_on_kelp)) %>%
  ungroup()

#this is for a appending avg pycno pos to kelp data
behavior_processed_forkelp <- behavior_processed %>%
  group_by(Trial, Treatment) %>% #going from behavioral assay scale to trial scale
  summarise(avg_pycno1_slice = mean(pycno1_slice, na.rm = TRUE),
            avg_pycno2_slice = mean(pycno2_slice, na.rm = TRUE),
            #closer to 1 = upstream
            up_dn_pyc1_ratio = sum(up_dn_pyc1=="upstream", na.rm = TRUE)/sum(up_dn_pyc1=="downstream", na.rm = TRUE),
            up_dn_pyc2_ratio = sum(up_dn_pyc2=="upstream")/sum(up_dn_pyc2=="downstream")) %>%
  ungroup()




