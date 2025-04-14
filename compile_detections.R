
# Run this file to aggregate all .csv detection files from BirdNET-Analyzer into one large .csv file for easier analysis
# Also adds timestamp and sitename based on the original file name 

library(tidyverse)
library(janitor)
library(sf)

# ******* CHANGE THESE PATHS for each use ******** 
# Superdirectory containing directories for each recording site
detections_directory <- "G:/Bioacoustics/Goleta_2024/detections/"
output_directory <- "G:/Bioacoustics/Goleta_2024/detections_compiled/"

# Create the output directory if it doesn't already exist
dir.create(file.path(output_directory), showWarnings = FALSE)

# Ingest a single recording from BirdNet .csv tab-delimited report
ingestDetectionsFile <- function(filename, directory, site_name)
{
  detection_df <- read_csv(paste(directory,filename,sep="/"),
                   col_types = cols()) %>% 
    janitor::clean_names() %>% 
    mutate(filename = paste(directory,filename,sep="/"),
           site = site_name) %>%
    select(-scientific_name)
  
  # Check whether there's any messy prefix on the filename before the datetime
  timestamp_str <- substr(filename, nchar(filename)-34, nchar(filename)-20)
  detection_df <- detection_df %>%
    mutate(year = as.integer(substr(timestamp_str,1,4)),
           month = as.integer(substr(timestamp_str,5,6)),
           day = as.integer(substr(timestamp_str,7,8)),
           hour = as.integer(substr(timestamp_str,10,11)),
           minute = as.integer(substr(timestamp_str,12,13)),
           second = as.integer(substr(timestamp_str,14,15))) %>%
    mutate(doy = as.integer(lubridate::yday(paste(year,month,day,sep="-"))))
  
  return(detection_df)
}
# Process all recording files associated with a single checklist directory
getBirdData <- function(site)
{
  # Get list of files within target directory
  site_directory <- paste(detections_directory,site,sep="")
  files <- list.files(path=site_directory)
  # Remove any files that aren't in .csv format
  csv_files <- files[grepl(".*\\.csv", files)] 
  print(paste("Working on data from ", site, ", which has ", length(files), " detection files."), sep="")
  
  # For each file, load it and apply a bit of basic processing
  dataframe_list <- lapply(csv_files, ingestDetectionsFile, directory=site_directory, site_name=site)
  # Drop any empty dataframes (no birds detected)
  dataframe_list <- dataframe_list[lapply(dataframe_list, nrow) > 0]
  # Convert list of dataframes to one dataframe
  output_df <- bind_rows(dataframe_list)
  write_csv(output_df, paste(output_directory,"/",site,".csv",sep=""))
  return(output_df)
}

# Get all sitenames (directory names) within detections_directory
recording_sites <- list.files(path=detections_directory)
# Filter out any .csv readme or config files
recording_sites <- recording_sites[(1:length(recording_sites))*(1-grepl(".*\\.csv", recording_sites))]

# For each site and associated directory, read in all .csv files
all_detections <- lapply(recording_sites, getBirdData)
# Combine the list of dataframes into one dataframe
all_detections <- bind_rows(all_detections)

# Write the output with a single large .csv file
write_csv(all_detections, paste(output_directory, "all_detections.csv", sep=""))
