#' Data Cleanup for PycnoPie 
#' AUTHOR: Nikita Sridhar
#' DATE: 11/13/24
#' 
#' Code for cleaning up data for PycnoPie mesocosm experiment (ran in Sitka during summer 2024) to determine how Pycnopodia
#' affect grazing rates of red urchins from urchin barrens and kelp forests.

#*loading data
#if you need to pull from google sheet source:
#kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=184409314#gid=184409314",
#                                  "Kelp") 

kelp <- read.csv("data/raw/kelp.csv")


kelp_cleaned <- kelp %>%
  separate(Treatment, "_(?=[^_]*$)",
           into = c("Pred_treatment", "Urch_habitat_treatment"), 
           remove = FALSE) 
  
       
rm(kelp)

