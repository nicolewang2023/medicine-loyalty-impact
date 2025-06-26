#------------------------------------------------------------------------------#
# Author:  Nicole Wang
# Date:    Updated June 2025
# Purpose: This script explores Foresight Seller Database
#------------------------------------------------------------------------------#

rm(list=ls())
gc()

if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load(data.table, stringr, magrittr, ggplot2, ggpubr, glue, janitor, dplyr, 
       tidyverse, readxl, openxlsx, lubridate, ggthemes, scales, writexl)

`%p%` = paste0
options(scipen = 999)

setwd("H:/GitHub/medicine-loyalty-impact")

foresight_output <- readRDS("input/foresight_data.rds")
foresight_output <- foresight_output %>% 
  rename(AI           = product_chemistry,
         manufacturer = virtual_portfolio_kynetec_beta,
         )
keep_cols <- c("year", "manufacturer", "AI", "single_vs_premix", "number_of_ai", "state", "total_area_treated_acres", "expenditures_us", "product_amt_used_lb_solid_gal_liq")
foresight_filtered <- foresight_output %>%
  select(all_of(keep_cols)) %>%
  filter(manufacturer %in% c("Symptom", "Cilios")) %>%
  mutate(manufacturer = sub("Symptom", "Symptom", manufacturer)) %>% 
  mutate(manufacturer = sub("Cilios", "Corteva", manufacturer)) %>% 
  mutate(single_vs_premix = sub("Multiple AI or Pre-Mix", "Mix", single_vs_premix)) %>% 
  mutate(single_vs_premix = sub("Single AI", "Single", single_vs_premix))

df_to_add <- data.frame()

for (i in 1:nrow(foresight_filtered)) {
  if (foresight_filtered$single_vs_premix[i] == "Single") next
  
  # Choose appropriate split method
  if (grepl("\\)$", foresight_filtered$AI[i])) {
    # Case: ends in parenthesis
    remove_last_char <- str_sub(foresight_filtered$AI[i], 1, nchar(foresight_filtered$AI[i]) - 1)
    split_AI <- str_split(remove_last_char, " \\(", simplify = FALSE)[[1]]
  } else {
    # Case: standard pre-mix with "-"
    split_AI <- str_split(foresight_filtered$AI[i], " - ", simplify = FALSE)[[1]]
  }
  
  # Add each AI as a separate row
  for (l in 1:length(split_AI)) {
    new_row <- data.frame(
      year = foresight_filtered$year[i],
      manufacturer = foresight_filtered$manufacturer[i],
      AI = split_AI[l],
      single_vs_premix = foresight_filtered$single_vs_premix[i],
      number_of_ai = foresight_filtered$number_of_ai[i],
      state = foresight_filtered$state[i],
      total_area_treated_acres = foresight_filtered$total_area_treated_acres[i],
      expenditures_us = foresight_filtered$expenditures_us[i],
      product_amt_used_lb_solid_gal_liq = foresight_filtered$product_amt_used_lb_solid_gal_liq[i],
      stringsAsFactors = FALSE
    )
    
    df_to_add <- rbind(df_to_add, new_row)
  }
}
# write_rds(df_to_add, "intermediate/df_to_add.rds")

foresight_single <- foresight_filtered %>% filter(single_vs_premix == "Single")
foresight_combined <- bind_rows(foresight_single, df_to_add)
foresight_combined$AI <- sapply(foresight_combined$AI, function(x) {
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}) # 389269 obs
# write_csv(foresight_combined, "intermediate/foresight_combined_before_group.csv")
foresight_combined <- foresight_combined %>%
  select(-"number_of_ai", "state", "product_amt_used_lb_solid_gal_liq") %>% 
  group_by(year, manufacturer, AI, single_vs_premix) %>% 
  summarise(total_area_treated_acres = sum(total_area_treated_acres), 
            expenditures_us = sum(expenditures_us)) # 2911 obs
foresight_combined$price <- foresight_combined$expenditures_us / foresight_combined$total_area_treated_acres
foresight_combined <- foresight_combined %>% select(-"expenditures_us")
write_rds(foresight_combined, "intermediate/foresight_combined.rds")