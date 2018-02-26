EyerImportRawData <- function(raw.data.path, poi.array){

  # Documentation here
  #
  # Args:
  # #
  # Returns:

  dir.create(paste(raw.data.path, "sampleData", sep = ""), showWarnings = F)
  raw.file.list <- list.files(path= raw.data.path, pattern = "\\.tsv$")

  for(file.counter in 1:length(raw.file.list)) {
    raw.data <- read.csv(paste(raw.data.path, raw.file.list[file.counter], sep = ""),sep="\t", header=T, encoding = "UTF-8", strip.white=TRUE, stringsAsFactors=FALSE)
    raw.data$time <- suppressWarnings(as.numeric(raw.data$time))
    message.data <- subset(raw.data, raw.data$timestamp == "MSG")
    message.data$time <- as.numeric(message.data$fix)
    message.data$fix <- NULL
    eye.mov.data <- subset(raw.data, raw.data$timestamp != "MSG")

    # Exeption handlers:
    if (nrow(eye.mov.data) < 1) print(paste("The file", raw.file.list[file.counter], "does not contain eye movement data"))
    if (nrow(eye.mov.data) < 1) next

    # Distance between subsequent data points should be 17 or 34 ms, for 30 or 60 Hz, respectively.
    eye.mov.data$time.shift <- c(eye.mov.data$time[-1], NA)
    eye.mov.data$dropped.frame <- eye.mov.data$time - eye.mov.data$time.shift
    eye.mov.data$time.shift <- NULL

    # Fixing rowname order for indexing.
    row.names(eye.mov.data) <- 1: nrow(eye.mov.data)

    # Replaces 0,0 eye movement values with NAs
    eye.mov.data$rawx[eye.mov.data$rawx== 0 & eye.mov.data$rawy == 0] <- NA
    eye.mov.data$rawy[is.na(eye.mov.data$rawx) & eye.mov.data$rawy == 0] <- NA
    eye.mov.data$avgx[eye.mov.data$avgx== 0 & eye.mov.data$avgy == 0] <- NA
    eye.mov.data$avgy[is.na(eye.mov.data$avgx) & eye.mov.data$avgy == 0] <- NA

    # Splitting messages by space.
    message.data$message.1 <- sapply(strsplit(as.character(message.data$state),' '), "[", 1)
    message.data$message.2 <- sapply(strsplit(as.character(message.data$state),' '), "[", 2)
    message.data$message.3 <- sapply(strsplit(as.character(message.data$state),' '), "[", 3)
    message.data$message.4 <- sapply(strsplit(as.character(message.data$state),' '), "[", 4)

    # Further message.2 split by underscore.
    message.data$message.5<- sapply(strsplit(as.character(message.data$message.2), '_'),"[",1)
    message.data$message.6 <- sapply(strsplit(as.character(message.data$message.2), '_'),"[",2)
    message.data$message.7 <- sapply(strsplit(as.character(message.data$message.2), '_'),"[",3)

    # Extracting sample rate and adding this info to eye movement data
    eye.mov.data$sample.rate <- as.numeric(message.data$message.2[which(message.data$message.1 == "samplerate:")])

    # Extracting display resolution x y and adding this info to eye movement data
    display.resolution <- message.data$message.3[which(message.data$message.2 == "resolution:")]
    eye.mov.data$disp.x <- as.numeric(unlist(strsplit(display.resolution,"x"))[1])
    eye.mov.data$disp.y <- as.numeric(unlist(strsplit(display.resolution,"x"))[2])
    rm(display.resolution)

    # Extracting date of recording and adding this info to eye movement data
    eye.mov.data$record.date <- as.character((message.data$message.3[which(message.data$message.2 == "datetime")])[1])

    # Extracting participant number and  adding this info to eye movement data
    eye.mov.data$participant.nr <- as.numeric((message.data$message.3[which(message.data$message.2 == "subject_nr")])[1])

    # Extracting file name (participant ID) and adding this info to eye movement data
    eye.mov.data$file.name <- gsub(".tsv", "", raw.file.list[file.counter])


    # Extracting info about time points for trial start, trial end, period of interest and writing to a new data frame
    number.of.trials <- length(message.data$time[which(message.data$message.1 == "start_trial")])
    trial.info <- as.data.frame(matrix(ncol = 2, nrow = number.of.trials))
    colnames(trial.info) <- c("start.message", "stop.message") #, "stimulus.message"
    trial.info$start.message <- message.data$time[which(message.data$message.1 == "start_trial")]
    trial.info$stop.message <- message.data$time[which(message.data$message.1 == "stop_trial")]

    # in low level perception studies each trial is unique and randomly generated
    # so the stimulus information is specific to the trial and doesn't repeat across participants
    trial.info$stimulus.message <- 1:number.of.trials
    trial.info$stimulus.id <- 1:number.of.trials

    # but in language or scene perception studies stimuli are repeated across participants
    # so if there is information about stimuli use it
    # for now there needs to be a variable called stimulus and stimulus_id to code the stimuli
    # ideally we could let people specify which variable they want to use
    if ("stimulus" %in% message.data$message.2){
      trial.info$stimulus.message <- gsub("var stimulus ", "", message.data$state[which(message.data$message.2 == "stimulus")])
      trial.info$stimulus.id <- message.data$message.3[as.numeric(which(message.data$message.2 == "stim_id"))]
    }

    # The number of Variables that must be created in the trial.info for periodes of interest (PoI) is not fixed.
    # This loop checks the poi.array provided in the main script and creates PoI variables accordingly.
    for(poi.counter in 1:length(poi.array)){
      if (poi.array[poi.counter] == "full") break
      trial.info[,(ncol(trial.info)+1)] <- trial.info$start.message + as.numeric(message.data$message.3[which(message.data$message.2 == poi.array[poi.counter])])
      colnames(trial.info)[ncol(trial.info)] <- paste("poi.", poi.counter, sep = "")
    }

    # Creates new variables in the eye.mov.data data frame that will be filled with info in the subsequent loop
    eye.mov.data$trial.index <- 0
    eye.mov.data$stimulus <- 0
    eye.mov.data$stimulus.id <- 0

    # Defining rownames in eye movement data, wich correspond to time points of start and stop messages.
    # Timing of eye movement data (~ 17 ms each time point) has to be aligned with more precise stimulus
    # and response timing. By using (which.min (abs(eyetracker time - message time))) the nearest eye movement
    # time point will be chosen. Some time points might not be asigned to a trial!
    for(trial.counter in 1:nrow(trial.info)) {
      start.row <- as.numeric(row.names(eye.mov.data)[which.min(abs(eye.mov.data$time - trial.info$start.message[trial.counter]))])
      stop.row <- as.numeric(row.names(eye.mov.data)[which.min(abs(eye.mov.data$time - trial.info$stop.message[trial.counter]))])
      eye.mov.data$trial.index[start.row:stop.row]<- trial.counter
      eye.mov.data$stimulus[start.row:stop.row] <- trial.info$stimulus.message[trial.counter]
      eye.mov.data$stimulus.id[start.row:stop.row] <- trial.info$stimulus.id[trial.counter]
    }

    # The number of variables that must be created in the eye.mov.data for periods of interest (PoI) is not a
    # fixed number. The following loop checks the poi.array provided in the main script and creates PoI variables.
    ###  Attention: The values for PoI_2 are currently incorrect -> should be from end of reading to reponse to gist!
    for(poi.counter in 1:length(poi.array)){
      eye.mov.data[,ncol(eye.mov.data)+1] <- 0
      if (poi.array[poi.counter] == "full") {
        colnames(eye.mov.data)[ncol(eye.mov.data)] <- paste("poi.", 1, sep = "")
        eye.mov.data$poi.1 <- eye.mov.data$trial.index
      }
      else{
        for (trial.counter in 1 : nrow(trial.info)) {
          start.row <- as.numeric(row.names(eye.mov.data)[which.min(abs(eye.mov.data$time - trial.info$start.message[trial.counter]))])
          poi.stop.row <- as.numeric(row.names(eye.mov.data)[which.min(abs(eye.mov.data$time - trial.info[trial.counter,4 + poi.counter]))])
          eye.mov.data[start.row: poi.stop.row, ncol(eye.mov.data)] <- trial.counter
        }
        colnames(eye.mov.data)[ncol(eye.mov.data)] <- paste("poi.", poi.counter, sep = "")
      }
    }


    # Deleting rows that were not asigned to a trial due to differences in sample precision (i.e., 17 ms vs. 1 ms).
    eye.mov.data <- subset(eye.mov.data, eye.mov.data$trial.index > 0)

    # Preparing file name and saving processed data.
    sample.data.file.name <- raw.file.list[file.counter]
    sample.data.file.name <- gsub(".tsv", "", sample.data.file.name)
    save(eye.mov.data, file = paste(raw.data.path, "sampleData/", sample.data.file.name,".Rda", sep = ""))

    # Providing some feedback on processing progress.
    processed.file <- raw.file.list[file.counter]
    print(paste("The file:", processed.file, "has been imported successfully.", sep = " "))
  }
  return("All available files were processed.")
}
