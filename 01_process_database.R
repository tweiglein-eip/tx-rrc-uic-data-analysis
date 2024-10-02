# File header -------------------------------------------------------------

# File Name: 01_process_database.R
# Created: 03 Sep 2024 by Tyler Weiglein
# Last Modified: 04 Sep 2024 by Tyler Weiglein

# Purpose: To process data from the Texas Railroad Commission (RRC)
# Underground Injection Control (UIC) database text file using the following
# steps for each table:
# 1) Subset table data
# 2) Parse table data
# 3) Write table data to file
#
# Because of how the RRC UIC database text file is structured (i.e.,
# relationships between entries in different tables depend on their
# order in the text file), this script also adds the UIC control number
# from Table 01 to the other tables to allow for the identification of
# which UIC control number a row in Tables 2 through 14 corresponds to.
# 
# **IMPORTANT**: Before running this script, embedded NUL characters must 
# be removed from uif700a.txt (the RRC UIC database text file) by searching
# for and replacing \x00 with ? in Notepad++. (Note: Notepad++ is available 
# for free here: https://notepad-plus-plus.org/)
#
# You should use the "Replace All" function and use "Save As" to append "_mod" 
# to the file name. If the above pre-processing step is not completed, 
# then there will be issues reading in the text files. (Note: I tried using 
# readLines with skipNul = TRUE to avoid this issue. However, the NUL characters 
# cannot be removed without affecting parsing because removing them shifts 
# character positions. I also tried using read_lines but kept getting a warning 
# message from vroom I could not access.)
#
# **IMPORTANT**: This script takes several hours to run because of the size
# of the RRC UIC database.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(readxl)
library(stringi)
library(tidyverse)

# Get most recent download

download_folder <- paste0("data/", list.files("data") %>% 
  sort(decreasing = TRUE) %>% 
  .[1])


# Read in RRC UIC database file -------------------------------------------

uic_db <- readLines(paste0(download_folder, "/uif700a_mod.txt"))

# Calculate number of rows

num_rows <- length(uic_db)


# Loop through RRC UIC database file to insert UIC control number ---------

for (i in 1:num_rows) {
  
  print(paste0("Processing row ", i, " out of ", num_rows, " (", round(i / num_rows * 100, 2), "%)"))
  
  if (str_sub(uic_db[i], 1, 2) == "01") {
    
    uic_cntl_no <- str_sub(uic_db[i], 3, 11)
    
  } else {
    
    stri_sub(uic_db[i], 3, 2) <- uic_cntl_no
    
  }
  
}


# Save/load modified RRC UIC database file --------------------------------

save(uic_db, file = paste0(download_folder, "/uic_db_cntl_no.RData"))

# load(paste0(download_folder, "/uic_db_cntl_no.RData"))


# Create variable name data frames for parsing ----------------------------

# Specify variable file

var_file <- "rrc_uic_db_var.xlsx"

# Get numbers in sheet names from rrc_uic_db_var.xlsx

sheet_num <- excel_sheets(var_file) %>%
  str_extract("[:digit:]+")

# Read in variables for each table

var <- map(sheet_num,
           function(x) {read_excel(var_file, sheet = paste0("Table ", x))})


# Subset RRC UIC data frame into tables -----------------------------------

for (i in seq_along(sheet_num)) {
  
  # Specify offset to account for adding UIC control number from Table 01 to other tables
  
  if (i == 1) {
    
    offset <- 0
    
  } else {
    
    offset <- 9
    
  }
  
  # Subset table data
  
  temp <- uic_db %>% 
    str_subset(paste0("^", sheet_num[i]))
  
  # Parse table data
  
  temp <- map2(var[[i]]$pos,
               var[[i]]$length,
               function(pos, len) {str_sub(temp,
                                           pos + offset,
                                           pos + offset + len - 1)}) %>% 
    as.data.frame(col.names = var[[i]]$var)
  
  # Save table data to file
  
  # Note: It is more efficient from a storage perspective to save tables as
  # .RData files than as .csv files. Consequently, this script saves them
  # as .RData files by default. Uncomment the code below to save them as
  # .csv files.
  
  # temp %>%
  #   write_excel_csv(paste0(download_folder, "/table_", sheet_num[i], ".csv"))
  
  assign(paste0("table_", sheet_num[i]), temp)
  
  save(list = paste0("table_", sheet_num[i]),
       file = paste0(download_folder, "/table_", sheet_num[i], ".RData"))
  
}
