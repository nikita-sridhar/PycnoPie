
#' Data Cleanup
#' AUTHOR: Nikita Sridhar
#' DATE: 10/14/24

#loading libraries
library(tidyverse)
library(here)
library(googlesheets4)
library(strex)
library(lme4)
library(performance)
library(sjPlot)

#pulling data from web ---------------------------------------------------------
kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=184409314#gid=184409314",
                                  "Kelp") 
behavior <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=1070675104#gid=1070675104",
                                      "Behavior")

###########################
## behavior processing  ##
##########################

#removing behavior entries where total number of urchins counted < 20
behavior <- behavior %>%
  filter(Total_num_urchin >= 20)

behavior$Num_urch_CREV1 <- as.double(behavior$Num_urch_CREV1)
behavior$Num_urch_CREV2 <- as.double(behavior$Num_urch_CREV2)
behavior$Num_urch_CREV3 <- as.double(behavior$Num_urch_CREV3)
behavior$Num_urch_CREV4 <- as.double(behavior$Num_urch_CREV4)
behavior$Num_urch_CREV5 <- as.double(behavior$Num_urch_CREV5)


behavior1 <- behavior %>%
  pivot_longer(Green_outer_SIDE:Num_urch_CREV5, names_to = "location", values_to = "location_count") 

#changing crev locations to just a pie slice location
behavior1.5 <- behavior1 %>% 
  mutate(crev = case_when(grepl("CREV",location) & location_count > 0  ~ "Y",
                          .default = "N"),
         location = recode(location, "Num_urch_CREV1" = ".5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV2" = "2.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV3" = "4.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV4" = "6.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV5" = "0_Outer_bottom"))

#2 columns for pie slice and location - for pycno, this was entered in the opposite direction (outer_bottom_green instead of green_outer_bottom)
behavior2 <- behavior1.5 %>%
  separate(location, c("slice", "position"), "_",extra = "merge") %>%
  separate(`Pycno1 position`, c("pycno1_position","pycno1_slice"), "\\_(?!.*_)", extra = "merge") %>%
  separate(`Pycno2 position`, c("pycno2_position","pycno2_slice"), "\\_(?!.*_)", extra = "merge")

#changing urch position/slice to same case as pycno 
#when pycno slice is "crev", that means inner - changing this to 0
behavior3 <- behavior2 %>%
  mutate(position = case_when(position == "outer_SIDE" ~ "Outer_side",
                              position == "outer_BOTTOM" ~ "Outer_bottom",
                              position == "INNER" ~ "Inner"),
         
         slice = case_when(slice == "Green" ~ "1",
                           slice == "Red" ~ "2",
                           slice == "Yellow" ~ "3",
                           slice == "Blue" ~ "4",
                           slice == "Orange" ~ "5",
                           slice == "White" ~ "6",
                           slice == "Black" ~ "7",
                           slice == "White2" ~ "8",
                           TRUE ~ slice),
         
         pycno1_slice = case_when(pycno1_slice == "green" ~ 1,
                                  pycno1_slice == "red" ~ 2,
                                  pycno1_slice == "yellow" ~ 3,
                                  pycno1_slice == "blue" ~ 4,
                                  pycno1_slice == "orange" ~ 5,
                                  pycno1_slice == "white" ~ 6,
                                  pycno1_slice == "black" ~ 7,
                                  pycno1_slice == "white2" ~ 8,
                                  pycno1_slice == "crev" ~ 0),
         
         pycno2_slice = case_when(pycno2_slice == "green" ~ 1,
                                  pycno2_slice == "red" ~ 2,
                                  pycno2_slice == "yellow" ~ 3,
                                  pycno2_slice == "blue" ~ 4,
                                  pycno2_slice == "orange" ~ 5,
                                  pycno2_slice == "white" ~ 6,
                                  pycno2_slice == "black" ~ 7,
                                  pycno2_slice == "white2" ~ 8,
                                  pycno2_slice == "crev" ~ 0))

behavior3$slice <- as.numeric(behavior3$slice)

#adding distance to pycno
behavior4 <- behavior3 %>%
  mutate(dist_slice_pyc1 = abs(slice - pycno1_slice),
         dist_slice_pyc2 = abs(slice - pycno2_slice))

#adding upstream/downstream variable from urchin POV given pycno position
behavior5 <- behavior4 %>%
  mutate(up_dn_pyc1 = ifelse(slice < pycno1_slice , 
                             "upstream",
                             "downstream"),
         up_dn_pyc2 = ifelse(slice < pycno2_slice , 
                             "upstream",
                             "downstream"))

#playing with urch distribution. - adding variance of pie slices
behavior7 <- behavior5 %>%
  mutate(pcnt_urch_in_slice = location_count/Total_num_urchin *100) %>%
  group_by(Trial, Treatment, Day_numrecord) %>%
  mutate(variance_of_urch_distrib = var(pcnt_urch_in_slice, na.rm = TRUE))


#reformatting data -------------------------------------------------------------

behavior_processed <- behavior5 %>%
  
  rename(urchin_position_count = location_count) %>%
  mutate(urchin_position_count = case_when(is.na(urchin_position_count) ~ 0, .default = urchin_position_count)) %>% #for now - changed urchin count NA to zero - but think about whether or not this is ok!!!!
  group_by(Tank, Treatment, Date, Day_numrecord, Trial, slice) %>%
  mutate(urchin_slice_count = sum(urchin_position_count)) %>%
  ungroup() %>%
  
  mutate(pyc_slice_count = case_when(slice == pycno1_slice | slice == pycno2_slice ~ 1, #NOTE: this is number of pycnos in a SLICE - not per position (outer/bottom/etc) - can add this variable too if you want to compare it later!
                                     slice == pycno1_slice & slice == pycno2_slice ~ 2, #for some reason no 2 vvalues showing up - in the data or prob w code?
                                     is.na(pycno1_slice) & Treatment %in% c("Star_KF", "Star_B") ~ NA, #turns pycno_count to 0 if pyc slice wasn't recorded in a treatment w a star
                                     is.na(pycno2_slice) & Treatment %in% c("Star_KF", "Star_B") ~ NA,
                                     .default = 0)) %>%
  
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) %>%
  
  #for model to work - can't be NA - also changing up/dn to up for non pycno treatments (bc has to be something other than NA for model)
  mutate(dist_slice_pyc1 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_pyc1),
         dist_slice_pyc2 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_pyc2),
         up_dn_pyc1 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc1),
         up_dn_pyc2 = case_when(Pred_treatment == "Control" ~ "upstream", .default = up_dn_pyc2))


######################################
#######     kelp processing  #########
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

kelp_processed <- kelp %>%
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) %>%
  mutate(pcnt_grazed = ((Kelp_weight_before_g - Kelp_weight_after_g)/ 
                          Kelp_weight_before_g)*100,
         
         inner_outer = ifelse(Kelp_ID %in% c(1,2,3,4,5,6,7,8), "outer", "inner"),
         
         slice = case_when(Kelp_ID %in% c(1,2,3,4,5,6,7,8) ~ Kelp_ID,
                           Kelp_ID == 9 ~ 1.5,
                           Kelp_ID == 10 ~ 3.5,
                           Kelp_ID == 11 ~ 5.5,
                           Kelp_ID == 12 ~ 7.5)) %>%
  
  
  merge(behavior_processed_forkelp, by = c("Treatment", "Trial")) %>%
  
  mutate(dist_slice_avgpyc1 = abs(slice - avg_pycno1_slice),
         dist_slice_avgpyc2 = abs(slice - avg_pycno2_slice)) %>%
  
  #this is for model to work - can't be NA - so 100 is far away

  mutate(dist_slice_avgpyc1 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_avgpyc1),
         dist_slice_avgpyc2 = case_when(Pred_treatment == "Control" ~ 100, .default = dist_slice_avgpyc2)) %>%
  
  #changing up dn ratio from inf to 1 for control (for model) and NaN to real NA
  mutate(up_dn_pyc1_ratio = case_when(Pred_treatment == "Control" ~ 1, .default = up_dn_pyc1_ratio),
         up_dn_pyc2_ratio = case_when(Pred_treatment == "Control" ~ 1, .default = up_dn_pyc2_ratio))
  

########################
####### plots. #########
########################

#graph for kelp grazed
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



write.csv(behavior5, file = "data/manipulated/processed_behavior/processed_behavior.csv")
write.csv(kelp_processed, file = "data/manipulated/processed_kelp/processed_kelp.csv")

