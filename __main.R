#Run full analysis pipeline
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
