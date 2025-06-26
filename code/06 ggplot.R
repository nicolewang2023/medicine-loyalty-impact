#------------------------------------------------------------------------------#
# Author:  Nicole Wang
# Date:    Updated June 2025
# Purpose: This script creates plots to visualize trends in the merged table
#------------------------------------------------------------------------------#

##################################################
###  initialize workspace
##################################################
rm(list=ls())
gc()

if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load(data.table, stringr, magrittr, ggplot2, ggpubr, glue, janitor, dplyr, 
       tidyverse, readxl, openxlsx, lubridate, ggthemes, scales, writexl)

`%p%` = paste0
options(scipen = 999)

setwd("H:/GitHub/medicine-loyalty_impact")

##################################################
###  read in data & set variables
##################################################
merged_df <- readRDS("intermediate/merged_df.rds")
AI_list <- c("Acetochlor", "Azoxystrobin", "S-metolachlor", "Oxamyl", "Mesotrione", "Rimsulfuron", "Paraquat", "Abamectin", "Oxyfluorfen")
product_types <- c("Single", "Mix")

##################################################
###  plot effect by AI
##################################################
for (ai in AI_list) {
  for (product_type in product_types) {
    
    # Filter and sort
    df_filtered <- merged_df %>%
      filter(AI == ai, single_vs_premix == product_type) %>%
      arrange(year)
    
    if (nrow(df_filtered) == 0) next
    
    # Scaling factor
    range_area <- range(df_filtered$total_area_treated_acres, na.rm = TRUE)
    range_price <- range(df_filtered$price, na.rm = TRUE)
    scale_factor <- ifelse(diff(range_price) > 0, diff(range_area) / diff(range_price), 1)
    
    # Add scaled price
    df_filtered <- df_filtered %>%
      mutate(price_scaled = price * scale_factor)
    
    # Detect loyalty change
    loyalty_diff <- diff(df_filtered$loyalty)
    loyalty_change_year <- df_filtered$year[which(loyalty_diff != 0) + 1]
    
    # Pivot to long format for legend
    df_long <- df_filtered %>%
      select(year, total_area_treated_acres, price_scaled) %>%
      pivot_longer(cols = c(total_area_treated_acres, price_scaled),
                   names_to = "metric", values_to = "value")
    
    # Plot
    p <- ggplot(df_long, aes(x = year, y = value, color = metric, group = metric)) +
      geom_line(linewidth = 1.2, na.rm = TRUE) +
      scale_color_manual(
        name = NULL,
        values = c("total_area_treated_acres" = "steelblue", "price_scaled" = "firebrick"),
        labels = c("Total Area Treated", "Price")
      ) +
      scale_y_continuous(
        name = "Total Area Treated (Acres)",
        labels = comma_format(accuracy = 1),
        sec.axis = sec_axis(
          ~ . / scale_factor,
          name = "Price ($)"
        )
      ) +
      labs(title = paste(ai, product_type), x = NULL) +
      theme_minimal() +
      theme(
        axis.title.y.left = element_text(color = "black", margin = margin(r = 10)),
        axis.title.y.right = element_text(color = "black", margin = margin(l = 10)),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom"
      )
    
    # Loyalty change line
    if (length(loyalty_change_year) > 0) {
      p <- p + geom_vline(xintercept = loyalty_change_year, linetype = "dashed", color = "black")
    }
    
    print(p)
    filename <- paste0("output/Effect by AI", gsub("[^a-zA-Z0-9_]", "_", ai), "_", product_type, ".png")
    ggsave(filename, plot = p, width = 8, height = 5)
  }
}

##################################################
###  plot effect by brand
##################################################
foresight_df <- foresight_output %>% 
  filter(single_vs_premix == "Single AI") %>% 
  group_by(brand, product_chemistry, virtual_portfolio_kynetec_beta, year) %>% 
  summarise(expenditures_us = sum(expenditures_us), total_area_treated_acres = sum(total_area_treated_acres)) %>% 
  mutate(price_per_acre = expenditures_us / total_area_treated_acres) %>% 
  mutate(year = as.numeric(year))

for (ai in AI_list) {
  p <- foresight_df %>% 
    filter(str_detect(product_chemistry, ai)) %>% 
    arrange(year) %>% 
    mutate(line_type = case_when(
      virtual_portfolio_kynetec_beta %in% c("Symptom", "Cilios") ~ "solid",
      TRUE ~ "dashed")) %>% 
    ggplot(aes(x = year, y = price_per_acre, 
               group = virtual_portfolio_kynetec_beta, 
               color = virtual_portfolio_kynetec_beta,
               linetype = line_type)) + 
    geom_line() +
    scale_linetype_manual(values = c("solid" = "solid", "dashed" = "dashed")) + 
    scale_x_continuous(name = "Year", breaks = seq(min(foresight_df$year), max(foresight_df$year), by = 2)) +
    scale_y_continuous(name = "Price per Acre") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    guides(linetype = "none")
  print(p)
  filename <- paste0("output/", ai, ".png")
  ggsave(filename, plot = p, width = 8, height = 5)
}

##################################################
###  plot by manufacturer and AI
##################################################
merged_df %>% filter(virtual_portfolio_kynetec_beta %in% c('Symptom','Cilio')) %>% 
  mutate(manufacturer = case_when(virtual_portfolio_kynetec_beta %in% c('BAYER','Symptom','Cilio','BASF') ~ virtual_portfolio_kynetec_beta,
                                  TRUE ~ 'OTHER'),
         has_key_ai = case_when(str_detect(product_chemistry, '(Amlodipine)|(Alprazolam)|(Duloxetine)|(Hydrochlorothiazide)|(Rivaroxaban)|(Amiodarone)') ~ 'Yes',
                                TRUE ~ 'No')) %>% 
  mutate(ai_summary = case_when(product_chemistry %in% c('Amlodipine', 'Alprazolam', 'Duloxetine', 'Hydrochlorothiazide', 'Rivaroxaban', 'Amiodarone')~product_chemistry,
                                has_key_ai=='Yes' & number_of_ai != '1 AI' ~ 'Key AI + Other',
                                has_key_ai=='No'~ 'Other AI(s)')) %>% 
  summarize(sales = sum(expenditures_us),
            qty = sum(product_amt_used_lb_solid_gal_liq),
            .by = c(year,manufacturer,ai_summary)) %>% 
  ggplot() + geom_line(aes(year,sales,group=ai_summary,color=ai_summary)) + 
  facet_grid(cols=vars(manufacturer)) + 
  scale_x_discrete(breaks=seq(2000,2030,5)) +
  scale_y_continuous(labels = label_comma(scale=1/1000000000,prefix = '$',suffix = ' B')) + 
  theme(legend.position="bottom")