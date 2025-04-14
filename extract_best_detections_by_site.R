
library(tidyverse)
library(janitor)
library(tuneR)

# Come back in here and clean this up later...

# Load example data
target_detections <- rbind(read_csv("G:/Bioacoustics/Goleta_2022/detections_compiled/all_detections.csv"),
                           read_csv("G:/Bioacoustics/Goleta_2023/detections_compiled/all_detections.csv"))

output_dir <- "C:/Users/grad/Documents/test_audio/"
target_species <- read_csv("G:/Bioacoustics/SCR_2023/allowed_species.csv")$common_name

extractAudio <- function(target_species, requested_samples=10, enforce_different_files=FALSE)
{
  print(paste("Working on", target_species))
  new_data <- lapply(unique(target_detections$site), 
                     function(target_site){
                       print(paste("  Working on", target_site))
                       # Extract good examples for Yellow Warbler
                       high_conf_files <- target_detections %>% 
                         filter(common_name == target_species, 
                                confidence > 0.5, 
                                site==target_site) %>% 
                         arrange(-confidence)
                       if(enforce_different_files)
                         high_conf_files <- high_conf_files %>%
                         group_by(filename) %>% 
                         arrange(-confidence) %>%
                         filter(row_number()==1)
                       if(nrow(high_conf_files) < requested_samples)
                       {
                         used_samples <- nrow(high_conf_files)
                         #print(paste("  WARNING: For site ", target_site, " and species ", target_species, " there are only ",  used_samples, " available examples.", sep=""))
                       }
                       else 
                         used_samples <- requested_samples
                       if(used_samples == 0)
                         return(NA)
                       new_paths <- lapply(1:used_samples,
                                           function(row_index){
                                             # Get filename from data frame
                                             target_filename <- high_conf_files[row_index,]$filename
                                             # Split filename into chunks by '/'
                                             file_chunks <- str_split(target_filename, "/")[[1]]
                                             # Get basename before ".wav"
                                             filename_base <- str_split(basename(target_filename),'\\.')[[1]][[1]]
                                             # Get input filename of the raw audio (from detections .csv file)
                                             input_audio_filename <- paste(file_chunks[1], file_chunks[2], file_chunks[3],"source",file_chunks[5],"/",sep="/",collapse="/")
                                             input_audio_filename <- paste(input_audio_filename, filename_base, ".wav", sep="")
                                             # Clip the audio to the target semgent
                                             audio_clip <- tuneR::extractWave(tuneR::readWave(input_audio_filename),
                                                                              from = high_conf_files[row_index,]$start_s,
                                                                              to = high_conf_files[row_index,]$end_s,
                                                                              xunit = "time")
                                             output_filename <- paste(output_dir,    # "/output_dir/SPECIES_SITE_AUDIO-TIMESTAMP_START-S.wav"
                                                                      paste(target_species %>% make_clean_names(), "_",
                                                                            target_site, "_", 
                                                                            filename_base, "_", 
                                                                            sprintf("%03d", high_conf_files[row_index,]$start_s), 
                                                                            ".wav", sep=""),
                                                                      sep="")
                                             
                                             tuneR::writeWave(audio_clip, 
                                                              output_filename)
                                             return(output_filename)
                                           })
                       return(list(new_paths, high_conf_files[1:used_samples,]))
                     })
  # Search for empty lists
  null_values <- lapply(new_data, function(target_tuple){return(is.na(target_tuple[[1]])[[1]])})
  new_data <- new_data[!unlist(null_values)]
  filepaths <- lapply(new_data, 
                      function(target_tuple){return(unlist(target_tuple[[1]]))})
  dataframe <- lapply(new_data, 
                      function(target_tuple){return(target_tuple[[2]])}) %>%
    bind_rows()
  return(list(filepaths, dataframe))
}


output_lists <- lapply(target_species,
                       function(target_species){
                         extractAudio(target_species, requested_samples = 10, enforce_different_files = FALSE)
                       })
filepaths <- lapply(output_lists, 
                    function(target_tuple){return(target_tuple[[1]])}) %>%
  unlist()
dataframe <- lapply(output_lists, 
                    function(target_tuple){return(target_tuple[[2]])}) %>%
  bind_rows()
write_csv(dataframe, paste(output_dir, "detections.csv"))
write_csv(data.frame(filepath=filepaths), paste(output_dir, "filepath_manifest.csv"))


# Originally I was using this code to resample audio files, but decided not to do this in the end
#
# # Resample all files into a new sampling rate, cut off at 20 kHz (easier visualization in Whombat)
# all_files_init <- list.files(output_dir, full.names = TRUE)
# # The above includes directories - filter them out here:
# all_files_init <- basename(all_files_init[!file.info(all_files_init)$isdir])
# # Loop over files, creating a copy in the 'resampled' folder for each at 20 kHz
# new_filepaths <- lapply(all_files_init,
#                         function(filename){
#                           audio_init <- tuneR::readWave(paste(output_dir,filename,sep=""))
#                           new_filepath <- paste(output_dir, "resampled/", filename, sep="")
#                           resampled <- seewave::resamp(audio_init, f=audio_init@samp.rate, g=24000, output="Wave")
#                           print(new_filepath)
#                           tuneR::writeWave(resampled, new_filepath)
#                           return(new_filepath)
#                         })
# 
