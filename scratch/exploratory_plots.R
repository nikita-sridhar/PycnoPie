#' Exploratory plots for PycnoPie 
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24
#' 
#'Run 00_cleaning and 01_manipulation first. OR, load kelp_processed and behavior_processed

######################
#   KELP GRAZED     #
#####################

#kelp grazed by predator/urchin treatment
ggplot(data = subset(kelp_processed, !is.na(pcnt_grazed)), aes(x = Urch_habitat_treatment, y = pcnt_grazed, 
                                                               fill = Pred_treatment) ) +
  geom_boxplot() +
  theme_classic() +
  scale_fill_manual(values = c('#5A5A5A','#DAF7A6') ) +
  ylab("% Change Kelp Grazed") +
  labs(fill = "Pred treatment")

#kelp grazed by distance of blade to star
ggplot(kelp_processed %>% filter(Pred_treatment == "Star")) +
  
  geom_point(aes(x = dist_slice_avgpyc1,
                 y = pcnt_grazed,
                 colour = Treatment)) +
  
  geom_point(aes(x = dist_slice_avgpyc2,
                 y = pcnt_grazed,
                 colour = Treatment)) +
  xlab("distance from kelp slice to average pyc position")

####################################
#        URCHIN DISTRIBUTION       #
####################################

#urchins at different distances to the closest pycno, separated by whether the slice is upstream or downstream of the closest pycno
ggplot(behavior_processed, 
       aes(x = ifelse(dist_slice_pyc1 < dist_slice_pyc2, dist_slice_pyc1, dist_slice_pyc2),
           y = urchin_slice_count)) +
  
  geom_point(aes(colour = ifelse(dist_slice_pyc1 < dist_slice_pyc2, up_dn_pyc1, up_dn_pyc2)),
             position = "jitter") +
  
  geom_smooth(method = "lm") +
  
  facet_wrap(vars(ifelse(dist_slice_pyc1 < dist_slice_pyc2, up_dn_pyc1, up_dn_pyc2), Treatment)) +
  
  labs(title = "Distribution of urchins at varying distances from the closest pycno",
       x = "Distance to closest pycno",
       y = "Count of urchins in pie slice",
       color = "Up/Downstream relative to closest pycno",
       caption = "Removed slice counts of urchins = 0")

#general distribution of urchin count in pie slices
ggplot(behavior_processed, 
       aes(x = slice,
           y = urchin_slice_count)) +
  
  geom_point(aes(colour = Treatment),
             position = "jitter") +
  
  geom_smooth(method = "lm") +
  
  facet_wrap(vars(Treatment)) +
  
  labs(title = "General distribution of urchins in slices for different treatments",
       x = "Slice",
       y = "Count of urchins in pie slice",
       caption = "Note: inflow is at slice 1, slice 0 denotes inner circle") +
  
  theme_classic()

#boxplot of urchin distribution separated by treatment 
ggplot(behavior_processed) +
  geom_boxplot(aes(x = slice, y = urchin_slice_count, color = Treatment, fill = Treatment, alpha = 0.2)) +
  labs(title = "Urchin counts at different distances (in pie slices) from inflow",
       xlab = "Slices away from inflow",
       ylab = "Urchin slice count",
       caption = "Note: half slices are where crevices are located") +
  theme_classic()
 



#VARIANCE TO MEAN RATIO of urchin distribution over treatments
#NOTE: v-to-mean of 1 = random, > 1 = clumped, < 1 = uniform
#should i find the variance of the var to mean ratios to find trial scale clustering dynamics?
ggplot(behavior_processed, 
       aes(x = Urch_habitat_treatment,
           y = var_to_mean,
           fill = Pred_treatment)) +
  
  geom_bar(stat = "identity", position = position_dodge()) +

  labs(title = "Variance to mean ratio of urchin slices across treatments",
       x = "Treatment",
       y = "Variance of percentage of urchins per slice") +
  
  theme_classic()

#var-to-mean boxplot
ggplot(behavior_processed, 
       aes(x = Urch_habitat_treatment,
           y = var_to_mean,
           fill = Pred_treatment)) +
  
  geom_boxplot() +
  
  labs(title = "Variance to mean ratio of urchin slices across treatments",
       x = "Treatment",
       y = "Variance of percentage of urchins per slice") +
  
  theme_classic()



#histograms of crevice use
ggplot(behavior_processed) +
  geom_histogram(aes(x = pcnt_in_crev, fill = Pred_treatment),position = "dodge") +
  facet_wrap(vars(Urch_habitat_treatment, Pred_treatment)) +
  labs(title = "Histogram for % of urchins in crevice") +
  theme_classic()

ggplot(behavior_processed) +
  geom_histogram(aes(x = trial_avg_pcnt_in_crev, fill = Pred_treatment),position = "dodge") +
  facet_wrap(vars(Urch_habitat_treatment, Pred_treatment)) +
  labs(title = "Histogram for trial avg % of urchins in crevice") +
  theme_classic()

#bar charts of crevice use (looks same for behavior scale and trial scale)
ggplot(behavior_processed, 
       aes(x = Pred_treatment, y = trial_avg_pcnt_in_crev)) +
  
  geom_bar(aes(fill = Urch_habitat_treatment), position = position_dodge(), stat = "identity") +
  
  geom_errorbar(aes(ymin = trial_avg_pcnt_in_crev-trial_sd_pcnt_in_crev, 
                ymax = trial_avg_pcnt_in_crev+trial_sd_pcnt_in_crev,
                color = Urch_habitat_treatment),
                position = position_dodge()) +
  
  labs(title = "Bar chart of % of urchins in crevice") +
  theme_classic()

#boxplot of crevice use - looks diff for behavior scale
ggplot(behavior_processed) +
  geom_boxplot(aes(x = Pred_treatment, y = trial_avg_pcnt_in_crev, 
               fill = Urch_habitat_treatment)) +
  labs(title = "Box plot of % of urchins in crevice") +
  theme_classic()

#boxplot of touching kelp
ggplot(behavior_processed) +
  geom_boxplot(aes(x = Urch_habitat_treatment, y = trial_avg_pcnt_on_kelp, 
                   fill =  Pred_treatment)) +
  labs(title = "Box plot of % of urchins on kelp") +
  theme_classic()


