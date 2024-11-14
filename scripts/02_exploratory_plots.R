#' Exploratory plots for PycnoPie 
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24
#' 
#'Run 00_cleaning and 01_manipulation first. OR, load kelp_processed and behavior_processed


#graph of kelp grazed by predator treatment
ggplot(data = subset(kelp_processed, !is.na(pcnt_grazed)), aes(x = Urch_habitat_treatment, y = pcnt_grazed, 
                                                               fill = Pred_treatment) ) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c('#5A5A5A','#DAF7A6') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Pred treatment")


#removing 0s in position_count for ggplot - takes away control plots
behavior6 <- behavior5 %>%
  filter(location_count > 0,
         dist_slice_pyc1 != "NA",
         dist_slice_pyc2 != "NA")

#graph showing number of urchins at different distances to the closest pycno, separated by whether the slice is upstream or downstream of the closest pycno
ggplot(behavior6, 
       aes(x = ifelse(dist_to_pyc1 < dist_to_pyc2, dist_to_pyc1, dist_to_pyc2),
           y = location_count)) +
  
  geom_point(aes(colour = ifelse(dist_to_pyc1 < dist_to_pyc2, up_dn_pyc1, up_dn_pyc2)),
             position = "jitter") +
  
  geom_smooth(method = "lm") +
  
  facet_wrap(vars(ifelse(dist_to_pyc1 < dist_to_pyc2, up_dn_pyc1, up_dn_pyc2), Treatment)) +
  
  labs(title = "Distribution of urchins at varying distances from the closest pycno",
       x = "Distance to closest pycno",
       y = "Count of urchins in pie slice",
       color = "Up/Downstream relative to closest pycno",
       caption = "Removed slice counts of urchins = 0")

#graph showing general distribution of urchin count in pie slices
ggplot(behavior5, 
       aes(x = slice,
           y = location_count)) +
  
  geom_point(aes(colour = Treatment),
             position = "jitter") +
  
  geom_smooth(method = "lm") +
  
  facet_wrap(vars(Treatment)) +
  
  labs(title = "General distribution of urchins in slices for different treatments",
       x = "Slice",
       y = "Count of urchins in pie slice",
       caption = "Note: inflow is at slice 1, slice 0 denotes inner circle")

#graph showing variance of urchin distribution over treatments
ggplot(behavior7, 
       aes(x = Treatment,
           y = variance_of_urch_distrib)) +
  
  geom_boxplot() +
  
  labs(title = "Variance of urchin distribution across treatments",
       x = "Treatment",
       y = "Variance of percentage of urchins per slice")


ggplot(kelp_processed, 
       aes(x = Pred_treatment,
           y = pcnt_grazed)) +
  
  geom_boxplot() 

#distance from star x kelp grazed
ggplot(kelp_processed %>% filter(Pred_treatment == "Star")) +
  
  geom_point(aes(x = dist_slice_avgpyc1,
                 y = pcnt_grazed,
                 colour = Treatment)) +
  
  geom_point(aes(x = dist_slice_avgpyc2,
                 y = pcnt_grazed,
                 colour = Treatment)) +
  xlab("distance from kelp slice to average pyc position")





