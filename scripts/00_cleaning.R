#PYCNOPIE

###########
#clean kelp
###########


kelp_cleaned <- kelp %>%
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) 

rm(kelp)


###############
#clean behavior
###############

#note: slice = colored pie slice. position = outer/bottom/side/etc, 
#aka orientation within slice. location = unique pairing of slice + position


behavior_cleaned <- behavior %>%
  
  #filtering behavior entries where total no of urch counted < 20
  filter(Total_num_urchin >= 20) %>%
  
  #changing crev count to doubles (used for pivot_longer)
  mutate(across(Num_urch_CREV1:Num_urch_CREV5, as.double)) %>%
  
  pivot_longer(Green_outer_SIDE:Num_urch_CREV5, names_to = "urchin_location", 
               values_to = "urchin_location_count") %>% 
  
  #changing crev locations to pie slice location 
  mutate(crev_count = case_when(grepl("CREV",urchin_location) ~ urchin_location_count, 
                                .default = 0),
         #renaming values in urchin_location column to be more descriptive
         urchin_location = case_when(
           urchin_location == "Num_urch_CREV1" ~ ".5_Outer_bottom",
           urchin_location == "Num_urch_CREV2" ~ "2.5_Outer_bottom",
           urchin_location == "Num_urch_CREV3" ~ "4.5_Outer_bottom",
           urchin_location == "Num_urch_CREV4" ~ "6.5_Outer_bottom",
           urchin_location == "Num_urch_CREV5" ~ "0_Outer_bottom",
           urchin_location == "Green_outer_SIDE" ~ "1_Outer_side",
           urchin_location == "Green_outer_BOTTOM" ~ "1_Outer_bottom",
           urchin_location == "Green_INNER" ~ "1_Inner",
           urchin_location == "Red_outer_SIDE" ~ "2_Outer_side",
           urchin_location == "Red_outer_BOTTOM" ~ "2_Outer_bottom",
           urchin_location == "Red_INNER" ~ "2_Inner",
           urchin_location == "Yellow_outer_SIDE" ~ "3_Outer_side",
           urchin_location == "Yellow_outer_BOTTOM" ~ "3_Outer_bottom",
           urchin_location == "Yellow_INNER" ~ "3_Inner",
           urchin_location == "Blue_outer_SIDE" ~ "4_Outer_side",
           urchin_location == "Blue_outer_BOTTOM" ~ "4_Outer_bottom",
           urchin_location == "Blue_INNER" ~ "4_Inner",
           urchin_location == "White_outer_SIDE" ~ "6_Outer_side", #are you sure
           urchin_location == "White_outer_BOTTOM" ~ "6_Outer_bottom",
           urchin_location == "White_INNER" ~ "6_Inner",
           urchin_location == "Orange_outer_SIDE" ~ "5_Outer_side",
           urchin_location == "Orange_outer_BOTTOM" ~ "5_Outer_bottom",
           urchin_location == "Orange_INNER" ~ "5_Inner",
           urchin_location == "Black_outer_SIDE" ~ "7_Outer_side",
           urchin_location == "Black_outer_BOTTOM" ~ "7_Outer_bottom",
           urchin_location == "Black_INNER" ~ "7_Inner",
           urchin_location == "White2_outer_SIDE" ~ "8_Outer_side",
           urchin_location == "White2_outer_BOTTOM" ~ "8_Outer_bottom",
           urchin_location == "White2_INNER" ~ "8_Inner",
           
           .default = urchin_location)) %>%
  
  #making a column for slice and a column for position
  separate(urchin_location, c("urchin_slice", "urchin_position"), "_",extra = "merge") %>%
  
  
  mutate(urchin_slice = as.numeric(urchin_slice)) %>%
  mutate(urchin_location_count = case_when(is.na(urchin_location_count) ~ 0, .default = urchin_location_count)) %>% #for now - changed urchin count NA to zero - but think about whether or not this is ok!!!!
  
  #averaging over position scale to get one value per slice
  group_by(Tank, Treatment, Date, Day_numrecord, Trial, urchin_slice) %>%
  mutate(urchin_slice_count = sum(urchin_location_count)) %>%
  ungroup() %>%
  
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) 

