#------------------------------------------------------------------------------#
# Author:  Nicole Wang
# Date:    Updated June 2025
# Purpose: This script creates a summary of target data for Symptom
#------------------------------------------------------------------------------#

setwd("H:/GitHub/medicine-loyalty-impact")
source("01 explore_seller_database.R")

##################################################
###  Create Symptom Summary
##################################################
syt_sum_df <- crossing(
  year = syt_sheet_names,
  distributor = syt_unique_dist,
  AI = syt_unique_AIs,
  loyalty = 0
)

for (i in 1:nrow(syt_sum_df)) {
  year <- syt_sum_df$year[i]
  dist <- syt_sum_df$distributor[i]
  AI <- syt_sum_df$AI[i]
  
  df <- read_excel(syt_path, sheet = year, col_names = TRUE)
  
  # Find all matching distributor rows
  dist_row_index <- which(apply(df, 1, function(row) any(grepl(dist, row, ignore.case = TRUE))))
  
  # Find all matching AI columns
  AI_col_index <- which(grepl(AI, colnames(df), ignore.case = TRUE))
  
  # Skip if no match
  if (length(dist_row_index) == 0 || length(AI_col_index) == 0) {
    next
  }
  
  # Extract target cell values and check if any are not NA
  target_vals <- df[dist_row_index, AI_col_index, drop = FALSE]
  if (any(!is.na(as.matrix(target_vals)))) {
    syt_sum_df$loyalty[i] <- 1
  }
}

write_xlsx(syt_sum_df, "intermediate/symptom_summary.xlsx")