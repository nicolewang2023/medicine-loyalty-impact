#------------------------------------------------------------------------------#
#-----------------------------     PRIVILEGED     -----------------------------#
#-----------------------------    CONFIDENTIAL    -----------------------------#
#--------------------- PREPARED AT THE REQUEST OF COUNSEL ---------------------#
#------------------------------------------------------------------------------#
#
# Title:   Merge
# Project: Pesti Tied
#
# Author:  Nicole Wang
#
#------------------------------------------------------------------------------#
#---------------------------   DRAFT :: UNAUDITED   ---------------------------#
#------------------------------------------------------------------------------#

rm(list=ls())
gc()

if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load(data.table, stringr, magrittr, ggplot2, ggpubr, glue, janitor, dplyr, 
       tidyverse, readxl, openxlsx, lubridate, ggthemes, scales, writexl)

`%p%` = paste0
options(scipen = 999)

setwd("X:/8000/8742_Pesti_Tied_States/Analysis/Working Folders/NW/On Off Loyalty/")

##################################################
###  Merge Syngenta and Corteva Summaries
##################################################
ctva_sum_df <- read_excel("intermediate/corteva_summary.xlsx")
ctva_sum_df$manufacturer <- "Corteva"
syt_sum_df <- read_excel("intermediate/syngenta_summary.xlsx")
syt_sum_df[syt_sum_df$year == "2005" & syt_sum_df$distributor == "Winfield" & syt_sum_df$AI == "Abamectin", "loyalty"] <- 1 # it's a ? in the source document
syt_sum_df$manufacturer <- "Syngenta"
combined_df <- rbind(syt_sum_df, ctva_sum_df)
# write_csv(combined_df, "output/loyalty_program_changes_summary.csv")
# collapse distributor layer
combined_df <- combined_df %>% 
  group_by(year, manufacturer, AI) %>% 
  summarise(loyalty = max(loyalty)) %>% 
  mutate(year = sub(".*-", "", year)) # 666 obs, no dupes

##################################################
###  Merge with FarmTrak Data
##################################################
farmtrak_combined <- readRDS("../FarmTrak/intermediate/farmtrak_combined.rds")
merged_df <- left_join(combined_df, farmtrak_combined, combined_df, by = c("year", "manufacturer", "AI")) # 1065 obs
merged_df <- merged_df %>%
  arrange(manufacturer, AI) %>%
  mutate(total_area_treated_acres = suppressWarnings(as.numeric(total_area_treated_acres))) %>% 
  mutate(price = suppressWarnings(as.numeric(price)))
write_rds(merged_df, "intermediate/merged_df.rds")