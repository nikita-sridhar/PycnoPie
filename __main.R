#PYCNOPIE - EXPERIMENT 2
#Authored by: Nikita Sridhar

#This script loads libraries, cleans and manipulates data required for analyses.

#loading libraries
library(here)
library(tidyverse)
library(googlesheets4)
library(strex)
library(lme4)
library(performance)
library(sjPlot)
library(glmmTMB)
library(DHARMa)
library(ggeffects)
library(effects)
library(car)
library(emmeans)
library(ggsignif)
library(flextable)
library(lmerTest)
library(ggpubr)

#load files
kelp <- read.csv("data/raw/exp2_kelp_weight.csv")
behavior <- read.csv("data/raw/exp2_behavior.csv")
urchin_roster <- read_csv("data/raw/exp2_urchin_roster.csv") 
pycno_roster <- read_csv("data/raw/exp2_pycno_roster.csv")

#all of these files can also be found on google sheets:
#kelp <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=184409314#gid=184409314","Kelp") 
#behavior <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=1070675104#gid=1070675104","Behavior")
#urchin_roster <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=1070675104#gid=1070675104","Urchin Roster")
#pycno_roster <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yFkZm64m7c9b4EdSxK3-qR8B7I-5rWyk7CDs7fmmMKg/edit?gid=1070675104#gid=1070675104","Pycno Roster")


#Step 0 - Clean raw data
source(here::here("./scripts/00_cleaning.R"))
#Step 1 - Manipulate cleaned data to get ready for stats
source(here::here("./scripts/01_manipulation.R"))

#Save files
#write.csv(kelp_clean, file = "data/processed data/cleaned data/kelp_clean.csv")
#write.csv(behavior_clean, file = "data/processed data/cleaned data/behavior_clean.csv")
#write.csv(kelp, file = "data/processed data/manipulated data/kelp.csv")
#write.csv(urch_behavior, file = "data/processed data/manipulated data/urch_behavior.csv")

#clear environment
#rm(list = ls()); gc()
