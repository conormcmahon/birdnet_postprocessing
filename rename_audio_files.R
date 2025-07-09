

all_files <- list.files("G:/Bioacoustics/SCR_2025/SCR_27/", full.names = TRUE)

renameFile <- function(filepath_in, starting_pattern, replacement_pattern){
  
  filepath_out <- filepath_in
  
  for(ind in 1:length(starting_pattern))
  {
    filepath_out <- str_replace(filepath_out, pattern=starting_pattern[ind], replacement=replacement_pattern[ind])
  }
  
  file.rename(filepath_in, filepath_out)
  
}

lapply(all_files, 
       renameFile, 
       starting_pattern=c("SCR-036", " \\(2\\)"), 
       replacement_pattern=c("SCR-027", ""))