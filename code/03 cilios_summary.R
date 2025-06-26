#------------------------------------------------------------------------------#
# Author:  Nicole Wang
# Date:    Updated June 2025
# Purpose: This script creates a summary of target data for Cilios
#------------------------------------------------------------------------------#

setwd("H:/GitHub/medicine-loyalty-impact")
source("01 explore_seller_database.R")

##################################################
###  Create Cilios Summary
##################################################
cl_sum_df <- crossing(
  year = cl_sheet_names,
  distributor = cl_unique_dist,
  AI = cl_unique_AIs,
  loyalty = 0
)

for (i in 1:nrow(cl_sum_df)) {
  year <- cl_sum_df$year[i]
  dist <- cl_sum_df$distributor[i]
  AI <- cl_sum_df$AI[i]
  
  df <- read_excel(cl_path, sheet = year, col_names = TRUE)
  
  dist_row_index <- which(apply(df, 1, function(row) any(grepl(dist, row, ignore.case = TRUE))))
  if (length(dist_row_index) == 0) next
  AI_row_index <- which(apply(df, 1, function(row) any(grepl("Active Ingredients", row, ignore.case = FALSE))))
  if (length(AI_row_index) == 0) next
  
  AI_row_vector <- as.character(unlist(df[AI_row_index, ]))
  AI_col_index <- which(grepl(AI, AI_row_vector, ignore.case = TRUE))
  
  # Extract all relevant cells
  target_vals <- df[dist_row_index, AI_col_index, drop = FALSE]

  if (any(!is.na(as.matrix(target_vals)))) {
    cl_sum_df$loyalty[i] <- 1
  }
}

write_xlsx(cl_sum_df, "intermediate/cilios_summary.xlsx")
