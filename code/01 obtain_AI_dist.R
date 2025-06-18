#------------------------------------------------------------------------------#
#-----------------------------     PRIVILEGED     -----------------------------#
#-----------------------------    CONFIDENTIAL    -----------------------------#
#--------------------- PREPARED AT THE REQUEST OF COUNSEL ---------------------#
#------------------------------------------------------------------------------#
#
# Title:   Obtain Syngenta & Corteva AIs and Distributors
# Project: Pesti Tied
#
# Author:  Nicole Wang
#
#------------------------------------------------------------------------------#
#---------------------------   DRAFT :: UNAUDITED   ---------------------------#
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

setwd("X:/8000/8742_Pesti_Tied_States/Analysis/Working Folders/NW/On Off Loyalty")

##################################################
###  Syngenta
##################################################
syt_path <- "input/2024.12.09 Syngenta Key AI Program By Year and Distributor (to Brattle).xlsx"
syt_sheet_names <- excel_sheets(syt_path)

##############################
###  AIs
##############################
syt_all_AI_df <- data.frame(
  year = character(),
  headers = I(list())
)

for (sheet in syt_sheet_names) {
  sheet_df <- read_excel(syt_path, sheet = sheet, n_max = 1, col_names = TRUE)
  headers <- colnames(sheet_df)[-1]
  
  first_row_df <- data.frame(
    year = sheet,
    headers = I(list(headers))
  )
  
  syt_all_AI_df <- bind_rows(syt_all_AI_df, first_row_df)
}

# obtain a list of unique AIs that have appeared in Syngenta's database
syt_unique_AIs <- unique(unlist(syt_all_AI_df$headers))
syt_unique_AIs <- sort(syt_unique_AIs) # length = 19

syt_all_AI_df <- syt_all_AI_df %>%
  mutate(headers = sapply(headers, paste, collapse = ", "))

# intermediate output that shows a list of AIs of Syngenta by year
# write_xlsx(syt_all_AI_df, "intermediate/syngenta_AIs_by_year.xlsx")

##############################
###  Distributors
##############################
syt_all_dist_df <- data.frame(
  year = character(),
  headers = I(list())
)

for (sheet in syt_sheet_names) {
  sheet_df <- read_excel(syt_path, sheet = sheet, col_names = TRUE)
  raw_headers <- sheet_df[[1]]
  headers <- raw_headers %>%
    na.omit() %>% 
    as.character() %>%
    str_extract("^[^,]+") %>%
    .[!grepl("^\\*", .)] %>% 
    sub(" - .*", "", .)
  
  first_col_df <- data.frame(
    year = sheet,
    headers = I(list(headers))
  )
  
  syt_all_dist_df <- bind_rows(syt_all_dist_df, first_col_df)
}

# obtain a list of unique distributors that have appeared in Syngenta's database
syt_unique_dist <- unique(unlist(syt_all_dist_df$headers))
syt_to_remove <- c("Wilbur Ellis", "Winfield (Solutions)", "Winfield Solutions", "Winfield (Agriliance)", "Winfield/Aligned Ag", "Nutrien/CPS")
syt_unique_dist <- syt_unique_dist[!syt_unique_dist %in% syt_to_remove] # remove duplicates
syt_unique_dist <- sort(syt_unique_dist) # length = 11

syt_all_dist_df <- syt_all_dist_df %>%
  mutate(headers = sapply(headers, paste, collapse = ", "))

# intermediate output that shows a list of distributors of Syngenta by year
# write_xlsx(syt_all_dist_df, "intermediate/syngenta_distributors_by_year.xlsx")

##################################################
###  Corteva
##################################################
ctva_path <- "input/2024.12.09 Corteva Key AIs in Loyalty By Year (to Brattle).xlsx"
ctva_sheet_names <- excel_sheets(ctva_path)
ctva_sheet_names <- setdiff(ctva_sheet_names, "Sheet1") # exclude the empty sheet

##############################
###  AIs
##############################
ctva_all_AI_df <- data.frame(
  year = character(),
  headers = I(list())
)

for (sheet in ctva_sheet_names) {
  sheet_df <- read_excel(ctva_path, sheet = sheet, col_names = TRUE)
  AI_row_index <- which(apply(sheet_df, 1, function(row) any(grepl("Active Ingredients", row, ignore.case = FALSE))))
  AI_row <- sheet_df[AI_row_index, ]
  AI_vector <- as.character(unlist(AI_row, use.names = FALSE))
  cleaned_AI <- AI_vector[!is.na(AI_vector) & AI_vector != "Active Ingredients"] # take out empty rows and unrecognized col header
  
  first_row_df <- data.frame(
    year = sheet,
    headers = I(list(cleaned_AI))
  )
  
  ctva_all_AI_df <- bind_rows(ctva_all_AI_df, first_row_df)
}

ctva_all_AI_df <- ctva_all_AI_df %>%
  mutate(headers = sapply(headers, paste, collapse = ", "))

# obtain unique a list of unique AIs in Corteva's database
ctva_unique_AIs <- ctva_all_AI_df$headers %>%
  strsplit(",") %>% 
  unlist() %>%
  trimws() %>%
  gsub("\\s+", " ", .) %>%
  sort() %>% 
  gsub("\\*", "", .) %>% 
  sub(" .*", "", .) %>% 
  unique() # length = 26

# intermediate output that shows a list of AIs of Corteva by year
# write_xlsx(ctva_all_AI_df, "intermediate/corteva_AIs_by_year.xlsx")

##############################
###  Distributors
##############################
ctva_all_dist_df <- data.frame(
  year = character(),
  headers = I(list())
)

for (sheet in ctva_sheet_names) {
  sheet_df <- read_excel(ctva_path, sheet = sheet, col_names = TRUE)
  AI_row_index <- which(apply(sheet_df, 1, function(row) any(grepl("Active Ingredients", row, ignore.case = FALSE))))
  dist_col <- sheet_df[(AI_row_index + 1):nrow(sheet_df), 1, drop = TRUE]
  
  dist_col <- dist_col %>%
    na.omit() %>%
    as.character() %>%
    sub(", .*", "", .) %>%
    .[!grepl("^\\*", .)] %>%
    .[!grepl("Notes:", ., fixed = TRUE)]
  
  
  dist_col_df <- data.frame(
    year = sheet,
    headers = I(list(dist_col))
  )

  ctva_all_dist_df <- bind_rows(ctva_all_dist_df, dist_col_df)
}

ctva_all_dist_df <- ctva_all_dist_df %>%
  mutate(headers = sapply(headers, paste, collapse = ", "))

# obtain unique a list of unique AIs in Corteva's database
ctva_unique_dist <- ctva_all_dist_df$headers %>%
  strsplit(",") %>% 
  unlist() %>%
  trimws() %>%
  gsub("\\s+", " ", .) %>%
  sub("/.*", "", .) %>% 
  sort() %>% 
  unique() # length = 10

# intermediate output that shows a list of distributors of Corteva by year
# write_xlsx(ctva_all_dist_df, "intermediate/corteva_distributors_by_year.xlsx")