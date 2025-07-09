
# In case of accidentally duplicating some effort data (which can happen if code crashes and is restarted)
# This script can be run to load a given effort dataset, check for, and remove any duplicated rows 

# ********** Update this path for a given dataset **********
effort_data_filepath <- "G:/Bioacoustics/SCR_2023/effort/survey_time.csv"

# Load in the data
effort_table <- read_csv("G:/Bioacoustics/SCR_2023/effort/survey_time.csv")
# Print number of hours
print(paste("\n New dataset loaded from ", effort_data_filepath, 
            " with ", round(nrow(effort_table) / 60 / 24, 1), " hours of audio.")
      , sep="")
# Look for duplicated rows
duplicated_rows <- duplicated(effort_table)
# Remove duplicated rows
table_trimmed <- effort_table[!duplicated_rows, ]
# Compare number of rows
print(paste("After removing duplicates, reduced by ", nrow(effort_table) - nrow(effort_table_trimmed), " records to ", 
            round(nrow(effort_table_trimmed) / 60 / 24, 1), " hours.", sep=""))
# Write filtered table to disk
write_csv(effort_table_trimmed, effort_data_filepath)
# Clean up data from memory 
rm(effort_table, effort_table_trimmed)