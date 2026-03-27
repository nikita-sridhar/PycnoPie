#run 00_cleaning first
#source(here::here(./scripts/00_cleaning.R))

###########################################
#adding summary stats
###########################################

kelp_processed <- kelp_cleaned %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/Kelp_weight_before_g)*100,
         weight_diff = (Kelp_weight_before_g - Kelp_weight_after_g),
         across(c(Urch_habitat_treatment, Pred_treatment), as.factor)) %>%
  group_by(Urch_habitat_treatment, Pred_treatment) %>%
  mutate(pcnt_grazed_sd = sd(pcnt_grazed),
         pcnt_grazed_se = sd(pcnt_grazed)/sqrt(length(pcnt_grazed)),
         pcnt_grazed_mean = mean(pcnt_grazed),
         weight_diff_sd = sd(weight_diff),
         weight_diff_se = sd(weight_diff)/sqrt(length(weight_diff)),
         weight_diff_mean = mean(weight_diff)) %>%
  ungroup() %>%
  unique()


#finding averages
avg_treatment_kelp <- kelp_processed %>%
  group_by(Treatment, Urch_habitat_treatment, Pred_treatment) %>%
  summarise(avg_weight_diff = mean(weight_diff),
            sd_weight_diff = sd(weight_diff))

#calculating sample size for raw data
#whole experiment:
kelp_processed %>%  summarize(count = n())

#whole tank:
kelp_processed %>% group_by(Treatment) %>% summarize(count = n())



#write.csv(kelp_processed, file = "data/manipulated/processed_kelp/kelp_processed.csv")

#rm(kelp_cleaned)


