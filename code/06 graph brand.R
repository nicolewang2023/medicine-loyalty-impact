#------------------------------------------------------------------------------#
#-----------------------------     PRIVILEGED     -----------------------------#
#-----------------------------    CONFIDENTIAL    -----------------------------#
#--------------------- PREPARED AT THE REQUEST OF COUNSEL ---------------------#
#------------------------------------------------------------------------------#
#
# Title:  Product Brand
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

setwd("X:/8000/8742_Pesti_Tied_States/Analysis/Working Folders/NW/FarmTrak")
AI_list <- c("AZOXYSTROBIN", "MESOTRIONE", "METOLACHLOR", "ACETOCHLOR", "RIMSULFURON", "OXAMYL")
farmtrak_output <- readRDS("X:/8000/8742_Pesti_Tied_States/Analysis/FTC FarmTrak Cleaning/AssemblyData/FarmTrak.rds")

# unique(sort(farmtrak_output$corn_herbicide_brand_ladder_12_17_2024))
# "All others"      "Low-tier"        "Mid-tier"        "Niche"           "Premium"         "Price-sensitive"

farmtrak_df <- farmtrak_output %>% 
  filter(single_vs_premix == "Single AI") %>% 
  group_by(brand, product_chemistry, virtual_portfolio_kynetec_beta, corn_herbicide_brand_ladder_12_17_2024, year) %>% 
  summarise(expenditures_us = sum(expenditures_us), total_area_treated_acres = sum(total_area_treated_acres)) %>% 
  mutate(price_per_acre = expenditures_us / total_area_treated_acres) %>% 
  mutate(year = as.numeric(year))

for (ai in AI_list) {
  p <- farmtrak_df %>% 
    filter(str_detect(product_chemistry, ai)) %>% 
    arrange(year) %>% 
    mutate(line_type = case_when(
      virtual_portfolio_kynetec_beta %in% c("SYNGENTA", "CORTEVA AGRISCIENCE") ~ "solid",
      TRUE ~ "dashed")) %>% 
    ggplot(aes(x = year, y = price_per_acre, 
               group = virtual_portfolio_kynetec_beta, 
               color = virtual_portfolio_kynetec_beta,
               linetype = line_type)) + 
    geom_line() +
    scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) + 
    scale_x_continuous(name = "Year", breaks = seq(min(farmtrak_df$year), max(farmtrak_df$year), by = 2)) +
    scale_y_continuous(name = "Price per Acre") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(linetype = "none")
  print(p)
  filename <- paste0("output/", ai, ".png")
  ggsave(filename, plot = p, width = 8, height = 5)
}