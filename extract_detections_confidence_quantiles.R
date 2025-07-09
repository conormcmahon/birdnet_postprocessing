
library(tidyverse)
library(janitor)
library(tuneR)

# Come back in here and clean this up later...

# Load example data
all_detections <- rbind(read_csv("G:/Bioacoustics/SCR_2023/detections_filtered/all_detections.csv"))

output_dir <- "G:/Bioacoustics/SCR_2023/audio_for_validation/"
allowed_species <- read_csv("G:/Bioacoustics/SCR_2023/allowed_species.csv")$common_name

target_species <- c("Yellow Warbler", "Bell's Vireo", "Black-headed Grosbeak", "Yellow-breasted Chat", "California Towhee", "Lesser Goldfinch")

transferFile <- function(detections){
  # Split filename into chunks by '/'
  file_chunks <- str_split(detections$filename, "/")[[1]]
  # Get basename before ".wav"
  filename_base <- str_split(basename(detections$filename),'\\.')[[1]][[1]]
  # Get input filename of the raw audio (from detections .csv file)
  input_audio_filename <- paste(file_chunks[1], file_chunks[2], file_chunks[3],"source",file_chunks[5],"/",sep="/",collapse="/")
  input_audio_filename <- paste(input_audio_filename, filename_base, ".wav", sep="")
  # Clip the audio to the target semgent
  audio_clip <- tuneR::extractWave(tuneR::readWave(input_audio_filename),
                                   from = detections[1,]$start_s,
                                   to = detections[1,]$end_s,
                                   xunit = "time")
  output_filename <- paste(output_dir,    # "/output_dir/SPECIES_SITE_AUDIO-TIMESTAMP_START-S.wav"
                           paste(detections$common_name %>% make_clean_names(), "_",
                                 filename_base, "_", 
                                 sprintf("%03d", detections[1,]$start_s), 
                                 ".wav", sep=""),
                           sep="")
  tuneR::writeWave(audio_clip, output_filename)
  return(output_filename)
}

extractAudio <- function(target_species, logit_quantiles=((0:10)/10), count_per_quantile=10, select_by_quantile=TRUE)
{
  set.seed(1)
  
  print(paste("Working on", target_species))
  species_data <- all_detections %>%
    filter(common_name == target_species)
  
  if(select_by_quantile)
    quantile_values <- quantile(species_data$confidence, logit_quantiles)
  else
    quantile_values <- logit_quantiles
  
  output_df <- lapply(1:(length(quantile_values)-1),
         function(ind){
           # Filter data based on confidence quantile bin
           quantile_data <- species_data %>% 
             filter(confidence >= quantile_values[ind], confidence < quantile_values[ind+1])
           # Subsample a random set of observations
           sample_indices <- sample(1:nrow(quantile_data), count_per_quantile)
           selected_data <- quantile_data[sample_indices,]
           new_filepaths <- lapply(1:nrow(selected_data),
               function(ind){
                 return(transferFile(selected_data[ind,]))
               }) %>%
             unlist()
           selected_data$new_filepaths <- new_filepaths
           
           return(selected_data)
         }) %>%
    bind_rows()
  
  return(output_df)
}
  
selected_detections_df <- lapply(target_species, extractAudio) %>%
  bind_rows()
selected_detections_df %>% View()

write_csv(selected_detections_df, paste(output_dir, "data_summary.csv"))
