
# Script made by Christopher Dwane, 11/15/24
#make sure you remove unneeded columns in temperature file
# Load required libraries

{ 
{library(dplyr)
  library(lubridate)
  
  # Set the folder path and string of characters. 
  #Make sure both the csv of the temperature data, and the output files from the script 01 (which must contain the word "output") are located in the folder below
  #If you have multiple output files from a single trial, they all need to start with the name of the trial, and include "output" somewhere in the name
  # so "F2_Trial2_27C_output2.csv" would be fine
  #The script will merge all the data from all the output files into a single "fulldata" file
  
  folder <-  "/Users/chanceyan/Documents/R/ThesisCrab/CrabHRData/"
  trialname<- "Crab_Trial2_17up_Ci" #this should match the name of the temperature data file
  file2=read.csv(paste(paste(folder, trialname, sep=''),'.csv', sep=''))
  # List all files in the folder
  files <- list.files(folder)
  
  # Find the output files that contains the matching name as the temperature data
  matching_file <- grep(trialname, files, value = TRUE)
  matching_file <- (grep("output", matching_file, value = TRUE))
  
  for (i in 1:length(matching_file)){
    filename= matching_file[i] 
    load=read.csv(paste(folder, filename, sep=''), header=TRUE, stringsAsFactors=F)
    assign(filename, load)}
  
  #merge output files
  matching_file <- lapply(matching_file, get)
  file1 <- Reduce(function(x, y) merge(x, y, all = TRUE), matching_file)
  
  # Convert date and start_time columns to appropriate formats
  file1$date <- ymd_hms(file1$time)
  file1$start_time <- as.numeric(file1$start)
  file1$end_time <- as.numeric(file1$end)
  file1$recording_length=as.numeric(seconds(file1$end_time)-seconds(file1$start_time))
  
  # Add a new column with the actual time that the recording
  file1$time <- file1$date + (file1$end_time-(file1$recording_length/2))
  
  # Load required libraries
  library(dplyr)
  library(lubridate)
  
  
  # Convert date and time columns in File 1 to appropriate formats
  file1$date <- ymd_hms(quiet=TRUE, file1$date, tz = "UTC")
  file1$time <- ymd_hms(quiet=TRUE, file1$time, tz = "UTC")
  
  # Convert timestamp column in File 2 to appropriate format
  #CHANGE BASED ON NUMBER OF INPUTS*******************************************
  colnames(file2)=c("timestamp","0")
  
  # Convert ISO time to ymd_hms format
  file2$timestamp = gsub("(\\+|-)\\d{2}:\\d{2}$", "", file2$timestamp)
  file2$timestamp <- ymd_hms(quiet=TRUE, file2$timestamp, tz = "UTC")
  
  #Use this ONLY when time in file2 is not in ISO format
  #file2$timestamp=hms(file2$timestamp)+file1$date[1]
  
  
  
  # Initialize an empty column for temperature in File 1
  file1$temperature <- NA
  
  # Iterate over each row in File 1
  for (i in 1:nrow(file1)) {
    # Get the individual's sensor number and corresponding timepoint
    Input <- file1$Input[i]
    timepoint <- file1$time[i]
    
    # Find the closest timestamp in File 2 that is not NA
    closest_timestamp <- file2$timestamp[which.min(abs(file2$timestamp - timepoint))]
    closest_timestamp <- closest_timestamp[!is.na(closest_timestamp)][1]
    
    if (!is.na(closest_timestamp)) {
      # Find the index of the individual's column in File 2
      col_index <- Input + 2
      
      # Get the temperature value from File 2 at the closest timestamp and individual's column
      temperature <- file2[which(file2$timestamp == closest_timestamp), col_index]
      
      # Assign the temperature value to the corresponding row in File 1
      file1$temperature[i] <- temperature
    } else {
      # Handle the case when no valid closest timestamp is found
      file1$temperature[i] <- NA
    }
  }
}

file1$Heartrate=file1$heart_rate
file1 <- file1 %>% select(Input, Trial, Treatment, date, temperature, Heartrate, sd, std_err, confidence, recording_length,time, start, end)
# Write the updated data to a new CSV file
savename=paste(trialname,"_fulldata.csv",sep='')
savename=paste(folder,savename,sep='')
write.csv(file1, savename, row.names = FALSE)}







