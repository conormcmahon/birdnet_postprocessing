

library(tidyverse)
library(janitor)
library(tuneR)

detections_files <- list.files("G:/Bioacoustics/SCR_2023/detections_compiled/", full.names=TRUE)
detections_files <- detections_files[2:length(detections_files)]

output_path <- "G:/Bioacoustics/SCR_2023/detections_filtered/all_detections.csv"

target_species <- read_csv("G:/Bioacoustics/SCR_2023/allowed_species.csv")$common_name

all_data_filtered <- lapply(detections_files,
       function(filepath){
         # Load in data
         new_data <- read_csv(filepath) %>% 
           filter(common_name %in% target_species)
         # Build new filename
         new_filepath <- str_split(filepath, "/")[[1]]
         new_filepath <- paste(paste(new_filepath[1:(length(new_filepath)-2)], collapse="/"), # original filepath, minus detections directory
                               "detections_filtered",
                               new_filepath[length(new_filepath)], sep="/")
         print(paste("Working on data for ", new_filepath, sep=""))
         write_csv(new_data, new_filepath)
         return(new_data)
       }) %>% 
  bind_rows()

write_csv(all_data_filtered, output_path)


