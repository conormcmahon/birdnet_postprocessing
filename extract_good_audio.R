

# Come back in here and clean this up later...

# Load example data
target_detections <- rbind(read_csv("G:/Bioacoustics/Goleta_2022/detections_compiled/all_detections.csv"), 
                           read_csv("G:/Bioacoustics/Goleta_2023/detections_compiled/all_detections.csv"))

output_dir <- "C:/Users/grad/Documents/test_audio/"
target_species <- c("Yellow Warbler", "Song Sparrow", "Spotted Towhee")

extractAudio <- function(species_name, logit_bin_percentiles)
{
  # Extract good examples for Yellow Warbler
  high_conf_detections <- (target_detections %>% filter(common_name == "Yellow Warbler", confidence > 0.9) %>% arrange(-confidence))$filename
  new_paths <- lapply(unique(yewa_high_conf)[1:40],
                      function(filename){
                        file_chunks <- str_split(filename, "/")[[1]]
                        filename_base <- str_split(basename(filename),'\\.')[[1]][[1]]
                        audio_file <- paste(file_chunks[1], file_chunks[2], file_chunks[3],"source",file_chunks[5],"/",sep="/",collapse="/")
                        audio_file <- paste(audio_file, filename_base, ".wav", sep="")
                        
                        file.copy(audio_file, to=paste(output_dir,paste(file_chunks[5], "_", filename_base, ".wav", sep=""),sep=""))
                      })
}

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
