## author: Debora Schierano aka Emeraldas Hofmann
## corse project in Data Science at "Johns Hopkins Bloomberg School of Public Health"

## HOW TO WORK
## download the script in "./yourdirectory"
## open R (or RStudio) and set "./yourdirectory" as your work-directory 
##    > setwd("./yourdirectory")
##    > source("./plot3.R")
##    > plot3()

library(dplyr)
library(lubridate)

file_download <- function(){
  if(!file.exists("./data_energy")){dir.create("./data_energy/")}
  
  #download dataset in temporary file and unzip DB
  fURLzip <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  temp <- tempfile()
  download.file(fURLzip, temp, method = "curl")
  unzip(temp, exdir = "./data_energy/")
  # rename file
  file.rename("./data_energy/household_power_consumption.txt", "./data_energy/hpc.txt")
  
}

make_dataset <- function(){
  #sercing data file, if no exist call file_download() function
  if(!file.exists("./data_energy/hpc.txt")){file_download()}
  ## read dataset
  hpc_data <- data.frame(read.table("./data_energy/hpc.txt", sep = ";", header = TRUE, stringsAsFactors = FALSE, colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric"), na.strings = "?"))
  
  # subsetting dataframe from 1/2/2007 to 2/2/2007
  hpc_sub <- filter(hpc_data, Date == "1/2/2007" | Date == "2/2/2007")
  rm("hpc_data")
  hpc_sub
}


plot3 <- function(){
  # serch to subset dataframe, if no exist in wd call make_dataset() function
  if(sum(ls() == "hpc_sub") == 0){hpc_sub <- make_dataset()}
  datetime <- dmy_hms(paste(hpc_sub$Date, hpc_sub$Time))
  
  # MAKE PLOT 3 on screen
  plot(datetime, hpc_sub$Sub_metering_1, type = "l",   xlab ="", ylab = "Energy sub metering")
  lines(datetime, hpc_sub$Sub_metering_2,  col = "red")
  lines(datetime, hpc_sub$Sub_metering_3,  col = "blue")
  
  legend("topright", lty=1, col = c("black",  "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  # PRIN PLOT 3 IN PNG FILE
  png(filename = "./plot3.png", width = 480, height = 480)
  
  plot(datetime, hpc_sub$Sub_metering_1, type = "l",   xlab ="", ylab = "Energy sub metering")
  lines(datetime, hpc_sub$Sub_metering_2,  col = "red")
  lines(datetime, hpc_sub$Sub_metering_3,  col = "blue")
  
  legend("topright", lty=1, col = c("black",  "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
}


