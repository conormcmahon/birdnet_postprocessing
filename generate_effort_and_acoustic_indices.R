
# Generate standardized .csv files which contain amount of survey effort at each site and time

# Load libraries
library(tidyverse)
library(here)
library(tuneR)
library(seewave)
library(soundecology)

# Target directory which contains all the audio files
target_directory <- "G:/Bioacoustics/Goleta_2025/source/"
# Output directory to save effort files
effort_output_directory <- "G:/Bioacoustics/Goleta_2025/effort/"

generateSurveyEffort <- function(audio_filename, compute_indices=TRUE, freq_step=1000, freq_max=10000, write_to_disk=TRUE, min_length=15)
{
  # Get date and time of audio file
  filename_chunks <- str_split(audio_filename, "/")[[1]]
  filename_without_path <- filename_chunks[[length(filename_chunks)]]
  datetime_str <- substring(filename_without_path, 
                            nchar(filename_without_path)-18, 
                            nchar(filename_without_path)-4)
  
  # Get Sitename
  sitename <- filename_chunks[[length(filename_chunks)-1]]
  
  # Set up metadata 
  metadata <- data.frame(site = sitename,
                         year = as.numeric(substr(datetime_str, 1,4)),
                         month = as.numeric(substr(datetime_str, 5,6)),
                         day = as.numeric(substr(datetime_str, 7,8)),
                         hour = as.numeric(substr(datetime_str, 10,11)),
                         minute = as.numeric(substr(datetime_str, 12,13)),
                         second = as.numeric(substr(datetime_str, 14,15))) %>% 
    mutate(doy = lubridate::yday(paste(year,month,day,sep="-")),
           time_frac = hour/24 + minute/24/60 + second/24/60/60)
  
  # Get the length of the audio file
  audio_header <- tuneR::readWave(audio_filename, header=TRUE)
  metadata$length <- round(audio_header$samples / audio_header$sample.rate, 2)

  # Get acoustic indices
  if(compute_indices & metadata$length >= min_length){
    # Load actual audio data
    audio <- tuneR::readWave(audio_filename, header=FALSE)
    
    # Get the average power across wavelength bands
    median_spec <- seewave::meanspec(audio, plot=FALSE, FUN=median, norm=FALSE)
    # Get the acoustic complexity (over time) also across wavelength bands 
    sink(tempfile()) # prevent annoying print messages from soundecology package
    aci <- invisible(soundecology::acoustic_complexity(audio, min_freq=0, max_freq=freq_max))
    sink()
    
    # Choose spacing for lower bounds of bands
    lower_bounds <- (1:(ceiling(freq_max/freq_step)))*freq_step - freq_step
    
    # Loop across  bands adding them to the output dataset
    for(lower_bound in lower_bounds){
      # Get row indices for start and stop in median spec
      start_row_median_spec <- floor(lower_bound * nrow(median_spec) / audio@samp.rate)
      stop_row_median_spec <- floor((lower_bound + freq_step) * nrow(median_spec) / audio@samp.rate)
      # Get row indices for start and stop in ACI      
      start_row_aci <- floor(nrow(aci[[7]]) * lower_bound / freq_max)
      stop_row_aci <- floor(nrow(aci[[7]]) * (lower_bound+freq_step) / freq_max)
      
      metadata <- metadata %>% 
        mutate(!!paste("amp_med_",sprintf("%04d", lower_bound),sep="") := median(median_spec[start_row_median_spec:stop_row_median_spec,2]),
               !!paste("aci_",sprintf("%04d", lower_bound),sep="") := median(aci[[7]][start_row_aci:stop_row_aci,] %>% unlist()))
    }
    
  }
  
  # Write metadata to a file to record which minutes were recorded at each site
  survey_time_file <- paste(effort_output_directory, "survey_time.csv", sep="")
  if(write_to_disk)
    write_csv(metadata, survey_time_file,
              append = file.exists(survey_time_file))
  
  return(metadata)
}


# Iterate across sites
site_dirs <- list.files(target_directory)
all_effort <- lapply(paste(target_directory, site_dirs, sep="/"), 
                     function(site_dir){
                       print(site_dir)
                       if(!dir.exists(site_dir))
                         return(-1)
                       filenames <- c(list.files(site_dir, pattern=".wav"), list.files(site_dir, pattern=".WAV"))
                       # Iterate across files within a site 
                       temp <- lapply(paste(site_dir, filenames, sep="/"),
                                      function(filename){
                                        return(generateSurveyEffort(filename, TRUE, 500, 15000, TRUE))
                                      })
                       return(1)
                     })




# Some deprecated code... come back and delete this once sure that I don't want it
# Initial version generated many more acoustic indices, but it's slow enough to take weeks to process all the audio
# For now only generating Acoustic_Complexity and a measure of median energy in frequency bands
# 
# # Load actual audio data
# audio <- tuneR::readWave(audio_filename, header=FALSE)
# # First, filter to just the 0 to 10 kHz frequency range
# audio <- seewave::ffilter(audio, from=0, to=10000, output="Wave")
# # Compute some pre-defined indices
# aci <- soundecology::acoustic_complexity(audio, max_freq = 10000)
# ndsi <- soundecology::ndsi(audio, bio_max = 10000)
# bci <- soundecology::bioacoustic_index(audio, max_freq = 10000)
# adi <- soundecology::acoustic_diversity(audio, max_freq = 10000)
# aei <- soundecology::acoustic_evenness(audio, max_freq = 10000)
# # Get Entropy and Median Amplitidue within 2-kHz bands
# audio_0_2 <- seewave::ffilter(audio, from=0, to=2000, output="Wave")
# H_0_2 <- seewave::H(audio_0_2)
# M_0_2 <- seewave::H(audio_0_2)
# audio_2_4 <- seewave::ffilter(audio, from=2000, to=4000, output="Wave")
# H_2_4 <- seewave::H(audio_2_4)
# M_2_4 <- seewave::H(audio_2_4)
# audio_4_6 <- seewave::ffilter(audio, from=4000, to=6000, output="Wave")
# H_4_6 <- seewave::H(audio_4_6)
# M_4_6 <- seewave::H(audio_4_6)
# audio_6_8 <- seewave::ffilter(audio, from=6000, to=8000, output="Wave")
# H_6_8 <- seewave::H(audio_6_8)
# M_6_8 <- seewave::H(audio_6_8)
# audio_8_10 <- seewave::ffilter(audio, from=8000, to=10000, output="Wave")
# H_8_10 <- seewave::H(audio_8_10)
# M_8_10 <- seewave::H(audio_8_10)
# 
# # Append all acoustic index information to metadata file
# metadata <- metadata %>%
#   mutate(aci = mean(c(aci[[3]], + aci[[4]]), na.rm=TRUE),
#          biophony = mean(c(ndsi$biophony_left, ndsi$biophony_right), na.rm=TRUE),
#          anthrophony = mean(c(ndsi$anthrophony_left, ndsi$anthrophony_right), na.rm=TRUE),
#          ndsi = mean(c(ndsi$ndsi_left, ndsi$ndsi_right), na.rm=TRUE),
#          bci = mean(c(bci$left_area, bci$right_area), na.rm=TRUE),
#          adi = mean(c(adi$adi_left, adi$adi_right), na.rm=TRUE),
#          aei = mean(c(aei$aei_left, aei$aei_right), na.rm=TRUE),
#          H_0_2 = H_0_2,
#          M_0_2 = M_0_2,
#          H_2_4 = H_2_4,
#          M_2_4 = M_2_4,
#          H_4_6 = H_4_6,
#          M_4_6 = M_4_6,
#          H_6_8 = H_6_8,
#          M_6_8 = M_6_8,
#          H_8_10 = H_8_10,
#          M_8_10 = M_8_10)
