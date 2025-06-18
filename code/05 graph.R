###################################################################################
# Project: Medicine Loyalty Impact
# Purpose: Visualize the impact of loyalty programs on medication purchasing data
# Author: Nicole Wang
# Date: June 2025
###################################################################################

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
###  loop through drugs & generate graphs
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
    filename <- paste0("output/", gsub("[^a-zA-Z0-9_]", "_", ai), "_", product_type, ".png")
    ggsave(filename, plot = p, width = 8, height = 5)
  }
}