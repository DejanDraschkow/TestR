EyerImportBehavioralData <- function(raw.data.path){

  # Documentation here
  #
  # Args:
  #
  # Returns:

  file.list <- list.files(raw.data.path , pattern = "csv", recursive = F)

  behav_dat<- NULL
  for(file in file.list){
    jj <- read.table(paste(raw.data.path, file, sep = ""),sep = ",", header = T)
    fileName <- gsub(".csv", "", file)
    jj$Participant <- factor(fileName)
    jj$start <- NULL

    # in case there is no dtplyr package - use plyr solution
    if (!require("dtplyr")) {
      behav_dat <- rbind.fill(behav_dat, jj)
    }
    else {
      behav_dat <- bind_rows(behav_dat, jj)
    }

    # Providing some feedback on processing progress.
    print(paste("The file:", file, "has been imported successfully.", sep = " "))
  }
  writeLines("All available files were imported and row-bound.\n Columns are matched by name, and any values that don't match will be filled with NA.")
#  print("Columns are matched by name, and any values that don't match will be filled with NA")
  return(behav_dat)
}
