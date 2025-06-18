#------------------------------------------------------------------------------#
#-----------------------------     PRIVILEGED     -----------------------------#
#-----------------------------    CONFIDENTIAL    -----------------------------#
#--------------------- PREPARED AT THE REQUEST OF COUNSEL ---------------------#
#------------------------------------------------------------------------------#
#
# Title:   Summarize Syngenta Loyalty Program Changes
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
syt_sheet_names
# [1] "2003-2004" "2004-2005" "2006-2007" "2007-2008" "2008-2009" "2009-2010" "2010-2011" "2011-2012" "2012-2013" "2013-2014"
# [11] "2014-2015" "2015-2016" "2016-2017" "2017-2018" "2018-2019" "2019-2020" "2020-2021" "2021-2022" "2022-2023" "2023-2024"
syt_unique_AIs
# [1] "Abamectin"          "Azoxystrobin"       "Chlorothalonil"     "Clodinafop"         "Cyprodinil"        
# [6] "Difenoconazole"     "Diquat"             "Flumetralin"        "Fomesafen"          "Lambda Cyhalothrin"
# [11] "Mefenoxam"          "Mesotrione"         "Paraquat"           "Pinoxaden"          "Prometryn"         
# [16] "Propiconazole"      "S-metolachlor"      "Thiamethoxam"       "Trinexapac-ethyl" 
syt_unique_dist
# [1] "CNI"              "Gar Bennett"      "Growmark"         "Helena"           "Nutrien"         
# [6] "Pinnacle"         "Simplot"          "Tenkoz"           "United Suppliers" "Wilbur-Ellis"    
# [11] "Winfield" 

##################################################
###  Create Syngenta Summary
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

write_xlsx(syt_sum_df, "intermediate/syngenta_summary.xlsx")