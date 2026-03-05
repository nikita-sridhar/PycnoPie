#run 00_cleaning first
#source(here::here(./scripts/00_cleaning.R))

###########################################
#adding summary stats
###########################################

kelp_processed <- kelp_cleaned %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/Kelp_weight_before_g)*100,
         weight_diff = (Kelp_weight_before_g - Kelp_weight_after_g),
         across(c(Urch_habitat_treatment, Pred_treatment), as.factor),
         pos_pcnt_change = ifelse(pcnt_grazed <= 0, 0.00001, pcnt_grazed)) %>%
  group_by(Urch_habitat_treatment, Pred_treatment) %>%
  mutate(pcnt_grazed_sd = sd(pcnt_grazed),
         pcnt_grazed_se = sd(pcnt_grazed)/sqrt(length(pcnt_grazed)),
         pcnt_grazed_mean = mean(pcnt_grazed),
         weight_diff_sd = sd(weight_diff),
         weight_diff_se = sd(weight_diff)/sqrt(length(weight_diff)),
         weight_diff_mean = mean(weight_diff)) %>%
  ungroup() %>%
  unique()

write.csv(kelp_processed, file = "data/manipulated/processed_kelp/kelp_processed.csv")

rm(kelp_cleaned)


