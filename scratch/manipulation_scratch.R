#creating new column for pred/urch treatment
#calculating % change kelp grazed
kelp_processed <- kelp %>%
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                          Kelp_weight_before_g)*100)





#graph 
ggplot(data = subset(kelp_processed, !is.na(pcnt_grazed)), aes(x = Urch_habitat_treatment, y = pcnt_grazed, 
                                                               fill = Pred_treatment) ) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c('#5A5A5A','#DAF7A6') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Pred treatment")

