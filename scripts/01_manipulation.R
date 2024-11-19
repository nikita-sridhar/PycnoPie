#' Data Manipulation for PycnoPie
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24

#Run 00_cleaning first, or load behavior_cleaned and kelp_cleaned


###########################
###     behavior      ####
##########################

behavior_processed <- behavior_cleaned %>%
  
  #removing position for now - might want to change later
  select(-position, -urchin_position_count) %>%
  unique() %>% 
  
  #adding distance to pycno
  mutate(dist_slice_pyc1 = abs(slice - pycno1_slice),
         dist_slice_pyc2 = abs(slice - pycno2_slice)) %>%
  
  #adding upstream/downstream variable from urchin POV given pycno position
  mutate(up_dn_pyc1 = ifelse(slice < pycno1_slice , 
                             "upstream",
                             "downstream"),
         up_dn_pyc2 = ifelse(slice < pycno2_slice , 
                             "upstream",
                             "downstream")) %>%
  
  #adding variance of pie slices
  group_by(Trial, Treatment, Day_numrecord) %>%
  mutate(variance_of_urch_distrib = var(urchin_slice_count, na.rm = TRUE), #one value per tank
         var_to_mean = variance_of_urch_distrib/mean(urchin_slice_count)) %>%
  ungroup() %>%
  
  #for model to work - can't be NA - also changing up/dn to up for non pycno treatments (bc has to be something other than NA for model)
  mutate(dist_slice_pyc1 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_pyc1),
         dist_slice_pyc2 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_pyc2),
         up_dn_pyc1 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc1),
         up_dn_pyc2 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc2)) %>%
    
  
  #adding crevice use - pcnt of urch in crevice
  group_by(Trial, Treatment, Day_numrecord) %>%
    mutate(pcnt_in_crev = sum(crev_count, na.rm=TRUE)/Total_num_urchin *100)  %>%  
  ungroup() %>%
  
  #crevice use - averaged between time points (trial scale)
  group_by(Trial, Treatment) %>%
  mutate(trial_avg_pcnt_in_crev = mean(pcnt_in_crev),
         trial_sd_pcnt_in_crev = sd(trial_avg_pcnt_in_crev, na.rm = TRUE)) %>%
  ungroup()


######################################
#######         kelp           #######
######################################

#this is for a appending avg pycno pos to kelp data
behavior_processed_forkelp <- behavior_processed %>%
  group_by(Trial, Treatment) %>% #going from behavioral assay scale to trial scale
  summarise(avg_pycno1_slice = mean(pycno1_slice, na.rm = TRUE),
            avg_pycno2_slice = mean(pycno2_slice, na.rm = TRUE),
            #closer to 1 = upstream
            up_dn_pyc1_ratio = sum(up_dn_pyc1=="upstream", na.rm = TRUE)/sum(up_dn_pyc1=="downstream", na.rm = TRUE),
            up_dn_pyc2_ratio = sum(up_dn_pyc2=="upstream")/sum(up_dn_pyc2=="downstream")) %>%
  ungroup()


#adding summary stats
kelp_processed <- kelp_cleaned %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                                                          Kelp_weight_before_g)*100) %>%
  
  merge(behavior_processed_forkelp, by = c("Treatment", "Trial")) %>%
  
  mutate(dist_slice_avgpyc1 = abs(slice - avg_pycno1_slice),
         dist_slice_avgpyc2 = abs(slice - avg_pycno2_slice), 
  
        #this is for model to work - can't be NA - so 100 is far away
        dist_slice_avgpyc1 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_avgpyc1),
        dist_slice_avgpyc2 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_avgpyc2), 
  
        #changing up dn ratio from inf to 1 for control (for model) and NaN to real NA
        up_dn_pyc1_ratio = case_when(Pred_treatment == "Control" ~ 1, .default = up_dn_pyc1_ratio),
        up_dn_pyc2_ratio = case_when(Pred_treatment == "Control" ~ 1, .default = up_dn_pyc2_ratio))
  
  
#reformat data to factor for model----------------------------------------------

kelp_processed$Urch_habitat_treatment <- as.factor(kelp_processed$Urch_habitat_treatment)
kelp_processed$Pred_treatment <- as.factor(kelp_processed$Pred_treatment)

kelp_processed$dist_slice_avgpyc1[is.nan(kelp_processed$dist_slice_avgpyc1)]<-NA
kelp_processed$dist_slice_avgpyc2[is.nan(kelp_processed$dist_slice_avgpyc2)]<-NA

kelp_processed$dist_slice_avgpyc1 <- as.numeric(kelp_processed$dist_slice_avgpyc1)
kelp_processed$dist_slice_avgpyc2 <- as.numeric(kelp_processed$dist_slice_avgpyc2)

behavior_processed$Urch_habitat_treatment <- as.factor(behavior_processed$Urch_habitat_treatment)
behavior_processed$Pred_treatment <- as.factor(behavior_processed$Pred_treatment)
behavior_processed$up_dn_pyc1 <- as.factor(behavior_processed$up_dn_pyc1)
behavior_processed$up_dn_pyc2 <- as.factor(behavior_processed$up_dn_pyc2)
  
#write CSVs
write.csv(kelp_processed, file = "data/manipulated/processed_kelp/kelp_processed.csv")
write.csv(behavior_processed, file = "data/manipulated/processed_behavior/behavior_processed.csv")








