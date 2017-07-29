EyerFixations <- function(rawPath){


dir.create(paste(rawPath, "fixationData", sep = ""), showWarnings = F)

rawFileList <- list.files(path = paste(rawPath, "sampleData/", sep = ""), pattern = "\\.Rda$")

# create data frame which gets returned in the end
preprocessed <- NULL # _DD 

for(i in 1:length(rawFileList)) {
  
  load(paste(rawPath, "sampleData/", rawFileList[i], sep = ""))

  # check how many trials are in the data set
  trialNumber <- max(eye.mov.data$trial.index, na.rm = TRUE) # drop NAs # DD_edit
  
  # checks how many periodes of interest are defined in raw data  
  #PoIcount <- ncol(ETdata[which(grepl( "PoI_" , colnames(ETdata)))])

  fixationData <- data.frame(start = numeric(),
                              end = numeric(), 
                              dur = numeric(), 
                              x = numeric(),
                              y = numeric(),
                             fixationIndex = numeric(),
                             trialIndex = numeric(),
                             stimulus_ID = numeric()) 
  
  for(trialCounter in 1:trialNumber){
  
    trialSubset <- subset(eye.mov.data, eye.mov.data$trial.index == trialCounter)
    trialSubset <- subset(trialSubset, !is.na(trialSubset$avgx))
    trialSubset <- subset(trialSubset, !is.na(trialSubset$avgy))
    fixations = emov.idt(trialSubset$time, trialSubset$avgx, trialSubset$avgy, 50, 3)
    
    if (length(fixations)>1) { ## DD_edit -  in case there are no fixations in a trial
      fixations$fixationIndex <- 1: nrow(fixations)
      fixations$trialIndex <- trialCounter
      fixations$stimulus_ID <- trialSubset$Stimulus_ID[1]
      fixationData <- rbind(fixationData, fixations)
    }

    
     
}
    save(fixationData, file = paste(rawPath, "fixationData/processed_", rawFileList[i], sep = ""))
    fixationData$Participant <-  factor( gsub(".Rda", "", rawFileList[i])) # _DD
    preprocessed <- rbind(preprocessed, fixationData) # _DD
}
return(preprocessed) # _DD
}
