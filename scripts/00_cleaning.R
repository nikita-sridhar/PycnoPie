#' Data Cleanup for PycnoPie 
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24
#' 
#' Code for cleaning up data for PycnoPie mesocosm experiment (ran in Sitka during summer 2024) to determine how Pycnopodia
#' affect grazing rates of red urchins from urchin barrens and kelp forests.


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
###     behavior      ####
##########################

#Cleaning------------------------------------------------------------------------

#filtering behavior entries where total no of urch counted < 20
behavior1 <- behavior %>%
  filter(Total_num_urchin >= 20)

#changing crev count to doubles (used for pivot_longer)
behavior1$Num_urch_CREV1 <- as.double(behavior1$Num_urch_CREV1)
behavior1$Num_urch_CREV2 <- as.double(behavior1$Num_urch_CREV2)
behavior1$Num_urch_CREV3 <- as.double(behavior1$Num_urch_CREV3)
behavior1$Num_urch_CREV4 <- as.double(behavior1$Num_urch_CREV4)
behavior1$Num_urch_CREV5 <- as.double(behavior1$Num_urch_CREV5)

behavior_cleaned <- behavior1 %>%
  pivot_longer(Green_outer_SIDE:Num_urch_CREV5, names_to = "location", values_to = "location_count") %>%
 
   #changing crev locations to pie slice location
  mutate(crev = case_when(grepl("CREV",location) & location_count > 0  ~ "Y", 
                          .default = "N"),
         location = recode(location, "Num_urch_CREV1" = ".5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV2" = "2.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV3" = "4.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV4" = "6.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV5" = "0_Outer_bottom")) %>%

  #for pycno location was entered in the opposite direction direction as urchin (outer_bottom_green instead of green_outer_bottom) - changing for consistency
  separate(location, c("slice", "position"), "_",extra = "merge") %>%
  separate(`Pycno1 position`, c("pycno1_position","pycno1_slice"), "\\_(?!.*_)", extra = "merge") %>%
  separate(`Pycno2 position`, c("pycno2_position","pycno2_slice"), "\\_(?!.*_)", extra = "merge") %>%

  #changing urch position/slice to same case as pycno 
#when pycno slice is "crev", that means inner - changing this to 0
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
                                  pycno2_slice == "crev" ~ 0)) %>%
  
  mutate(slice = as.numeric(slice)) %>%
  
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
           remove = FALSE) 
  

######################################
#######         kelp           #######
######################################

kelp_cleaned <- kelp %>%
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) %>%
  
        
  #blades inside inner ring of tank vs outer ring
  mutate(inner_outer = ifelse(Kelp_ID %in% c(1,2,3,4,5,6,7,8), "outer", "inner"),
  
         slice = case_when(Kelp_ID %in% c(1,2,3,4,5,6,7,8) ~ Kelp_ID,
                           Kelp_ID == 9 ~ 1.5,
                           Kelp_ID == 10 ~ 3.5,
                           Kelp_ID == 11 ~ 5.5,
                           Kelp_ID == 12 ~ 7.5)) 
  

