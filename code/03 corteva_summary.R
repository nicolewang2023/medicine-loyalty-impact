#------------------------------------------------------------------------------#
#-----------------------------     PRIVILEGED     -----------------------------#
#-----------------------------    CONFIDENTIAL    -----------------------------#
#--------------------- PREPARED AT THE REQUEST OF COUNSEL ---------------------#
#------------------------------------------------------------------------------#
#
# Title:   Summarize Corteva Loyalty Program Changes
# Project: Pesti Tied
#
# Author:  Nicole Wang
#
#------------------------------------------------------------------------------#
#---------------------------   DRAFT :: UNAUDITED   ---------------------------#
#------------------------------------------------------------------------------#

setwd("X:/8000/8742_Pesti_Tied_States/Analysis/Working Folders/NW/On Off Loyalty/code")
source("1 obtain_AI_dist.R")

##################################################
###  From Script 1
##################################################
ctva_unique_AIs
# [1] "Acetochlor"      "Aminopyralid"    "Arylex"          "Chlorimuron"     "Chlorpyrifos"    "Clopyralid"     
# [7] "Cloransulam"     "Cyhalofop"       "Fluroxypyr"      "Methomyl"        "Methoxyfenozide" "Myclobutanil"   
# [13] "Nitrapyrin"      "Optinyte"        "Oxamyl"          "Oxyfluorfen"     "Penoxsulam"      "Picloram"       
# [19] "Propyzamide"     "Pyroxsulam"      "Quinoxyfen"      "Rimsulfuron"     "Spinetoram"      "Spinosad"       
# [25] "Tebuthiuron"     "Triclopyr" 
ctva_unique_dist
# [1] "CNI"              "Growmark"         "Helena"           "IAP"              "Nutrien"         
# [6] "Pinnacle"         "Simplot"          "Tenkoz"           "United Suppliers" "Winfield"  

##################################################
###  Create Corteva Summary
##################################################
ctva_sum_df <- crossing(
  year = ctva_sheet_names,
  distributor = ctva_unique_dist,
  AI = ctva_unique_AIs,
  loyalty = 0
)

for (i in 1:nrow(ctva_sum_df)) {
  year <- ctva_sum_df$year[i]
  dist <- ctva_sum_df$distributor[i]
  AI <- ctva_sum_df$AI[i]
  
  df <- read_excel(ctva_path, sheet = year, col_names = TRUE)
  
  dist_row_index <- which(apply(df, 1, function(row) any(grepl(dist, row, ignore.case = TRUE))))
  if (length(dist_row_index) == 0) next
  AI_row_index <- which(apply(df, 1, function(row) any(grepl("Active Ingredients", row, ignore.case = FALSE))))
  if (length(AI_row_index) == 0) next
  
  AI_row_vector <- as.character(unlist(df[AI_row_index, ]))
  AI_col_index <- which(grepl(AI, AI_row_vector, ignore.case = TRUE))
  
  # Extract all relevant cells
  target_vals <- df[dist_row_index, AI_col_index, drop = FALSE]

  if (any(!is.na(as.matrix(target_vals)))) {
    ctva_sum_df$loyalty[i] <- 1
  }
}

write_xlsx(ctva_sum_df, "intermediate/corteva_summary.xlsx")
