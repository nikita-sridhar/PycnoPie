
#' Data Cleanup
#' AUTHOR: Nikita Sridhar
#' DATE: 7/16/24

# This script pulls raw data from the google drive where it lives and cleans it

#loading libraries
library(tidyverse)
library(here)
library(googlesheets4)
library(strex)

#pulling data from web ---------------------------------------------------------
kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=184409314#gid=184409314",
                          "Kelp") 
behavior <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=1070675104#gid=1070675104",
                                      "Behavior")

#MAIN GOAL OF THIS SECTION: change pie slice colors to numbers, and crevice locations as half numbers

#removing behavior entries where total number of urchins counted < 20
behavior <- behavior %>%
  filter(Total_num_urchin >= 20)

behavior$Num_urch_CREV1 <- as.double(behavior$Num_urch_CREV1)
behavior$Num_urch_CREV2 <- as.double(behavior$Num_urch_CREV2)
behavior$Num_urch_CREV3 <- as.double(behavior$Num_urch_CREV3)
behavior$Num_urch_CREV4 <- as.double(behavior$Num_urch_CREV4)
behavior$Num_urch_CREV5 <- as.double(behavior$Num_urch_CREV5)

#pivot longer bc hard to separate underscore of green_side/bottom/inner when they are all columns. will go back to wide format after this.
#now since we pivot longer we are at the resolution of 1 line = one pie slice combo (so as many combos as there are per tank = num rows in a day_time assay)
behavior1 <- behavior %>%
  pivot_longer(Green_outer_SIDE:Num_urch_CREV5, names_to = "location", values_to = "location_count") 

behavior1.5 <- behavior1 %>% 
  mutate(location = recode(location, "Num_urch_CREV1" = ".5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV2" = "2.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV3" = "4.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV4" = "6.5_Outer_bottom"),
         location = recode(location, "Num_urch_CREV5" = "0_Outer_bottom"))

#2 columns for pie slice and location - for pycno, this was entered in the opposite direction (outer_bottom_green instead of green_outer_bottom)
behavior2 <- behavior1.5 %>%
  separate(location, c("slice", "position"), "_",extra = "merge") %>%
  separate(`Pycno1 position`, c("pycno1_position","pycno1_slice"), "\\_(?!.*_)", extra = "merge") %>%
  separate(`Pycno2 position`, c("pycno2_position","pycno2_slice"), "\\_(?!.*_)", extra = "merge")

#pycno slice/position - used different cases - trying to clean this up by first looking at unique entries:
unique(behavior2$position)
unique(behavior2$pycno1_position)
unique(behavior2$pycno2_position)

unique(behavior2$slice)
unique(behavior2$pycno1_slice)
unique(behavior2$pycno2_slice)
 
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
                              
#adding upstream/downstream variable from urchin POV given pycno position
behavior4 <- behavior3 %>%
  mutate(up_dn_pyc1 = ifelse(pycno1_slice == 1, 
                             "downstream",
                             up_dn_pyc1),
         up_dn_pyc1 = ifelse(pycno1_slice == 2, 
                             ifelse(slice == 1, "upstream", "downstream"),
                             up_dn_pyc1))
                     

