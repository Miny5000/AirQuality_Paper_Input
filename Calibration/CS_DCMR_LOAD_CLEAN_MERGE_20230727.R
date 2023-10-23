#Script to load datasets, clean & merge 
#Readme
#Step 1: Load datasets DCMR NO, DCMR PM, CS3, CS4 + make into dfs + same timestamp (between 12-05-2023 15:00 - 19-05-2023 09:00)
#Step 2: DCMR datasets (NO + PM) merge on timestamp local 
#Step 3: CS data - clean, 10-sec datasets
#Step 4: CS datasets, merge on timestamp local
#Step 5: Merge DCMR, CS, write.csv combined dataset
#Step 6: (Optional) tests and plots 
#End Readme 

#Packages

library(randomForest)
library(rio)
library(tidyverse)
library(dplyr)

#Step 1 - import and clean data
#dcmr_no 
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/DCMR_files_calibration_NOX")
mypath="C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/DCMR_files_calibration_NOX"
## Create list of text files
txt_files_ls_dcmr = list.files(path=mypath, pattern="*.CSV")
## Read the files in, assuming comma separator
txt_files_df_dcmr <- lapply(txt_files_ls_dcmr, function(x) {read.csv(file = x, sep =";", na.strings = "", header = FALSE)})
## Combine to one df
dcmr_no <- do.call("rbind", lapply(txt_files_df_dcmr, as.data.frame))
#added a colname "timestamp_local_min_1" because original datafiles report timestamp -1 hour (=Dutch wintertime)
colnames(dcmr_no) <- c("time", "NOx_ppb", "a", "NO_ppb", "b","NO2_ppb" ,"c", "timestamp_local_min_1", "timestamp_local")
##changing coma to points
dcmr_no <- data.frame(lapply(dcmr_no, function(x) gsub(",", ".", x)))
#filter rows if not equal to "A"
dcmr_no <- subset(dcmr_no, a == "A" & b == "A" & c == "A") 
num_columns <- colnames(dcmr_no[c(2,4,6)])
#make column values numeric 
for (col in num_columns) {
  dcmr_no[[col]] <- as.numeric(dcmr_no[[col]])
}
#filter out the negative values
dcmr_no <- subset(dcmr_no, NOx_ppb > -1 & NO_ppb > -1 & NO2_ppb > -1) #difference of 10 points -5, 11 -4, 13 -3, 15 -2,  15 -1, 

##dcmr timestamp
dcmr_no$timestamp_local <- as.POSIXct(dcmr_no$timestamp_local, format = "%d-%m-%Y %H:%M:%OS") 
##only selects time from calibration - 12 July 15.00 to 19 July 9.00
dcmr_no <- dcmr_no[dcmr_no$timestamp_local >= "2023-05-12 15:00:00",]
dcmr_no <- dcmr_no[dcmr_no$timestamp_local <= "2023-05-19 09:00:00",]

#dcmr_pm 
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/DCMR_files_calibration_PM25")
mypath="C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/DCMR_files_calibration_PM25"
## Create list of text files
txt_files_ls_dcmr = list.files(path=mypath, pattern="*.CSV")
## Read the files in, assuming comma separator
txt_files_df_dcmr <- lapply(txt_files_ls_dcmr, function(x) {read.csv(file = x, sep =";", na.strings = "", header = FALSE)})
## Combine to one df
dcmr_pm <- do.call("rbind", lapply(txt_files_df_dcmr, as.data.frame))
#add column name "timestamp_local_min_1" because original data is recorded in timestamp - 1 hour (=Dutch wintertime)
colnames(dcmr_pm) <- c("time", "palas pm25", "a", "palas pm4", "b","palas pm10" ,"c", "timestamp_local_min_1", "timestamp_local")
##changing coma to points
dcmr_pm <- data.frame(lapply(dcmr_pm, function(x) gsub(",", ".", x)))

##dcmr timestamp
dcmr_pm$timestamp_local <- as.POSIXct(dcmr_pm$timestamp_local, format = "%d-%m-%Y %H:%M:%OS") 
##only selects time from calibration - 12 July 15.00 to 19 July 9.00
dcmr_pm <- dcmr_pm[dcmr_pm$timestamp_local >= "2023-05-12 15:00:00",]
dcmr_pm <- dcmr_pm[dcmr_pm$timestamp_local <= "2023-05-19 09:00:00",]

#filter rows if not equal to "A"
dcmr_pm <- subset(dcmr_pm, a == "A" & b == "A" & c == "A")
#make column values numeric 
num_columns <- colnames(dcmr_pm[c(2,4,6)])
for (col in num_columns) {
  dcmr_pm[[col]] <- as.numeric(dcmr_pm[[col]])
}
#filter out the negative values
dcmr_pm <- subset(dcmr_pm, palas.pm25 > -1 & palas.pm4 > -1 & palas.pm10 > -1)


#cs3 
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/CS_files_calibration_AMS3")
mypath="C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/CS_files_calibration_AMS3"
## Create list of text files
txt_files_ls = list.files(path=mypath, pattern="*.CSV") 
## Read the files in, assuming comma separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.csv(file = x, sep =",", na.strings = "", header = FALSE)})
## Combine them to one df
cs3 <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
colnames(cs3) <- c("A"," deviceID","time","latitude","longitude","PM1","PM25","PM10","bin0",
                  "bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10","bin11",
                  "bin12","bin13","bin14","bin15","bin16","bin17","bin18","bin19","bin20","bin21",
                  "bin22","bin23","flowrate","countglitch","laser_status","temperature_opc","humidity_opc",
                  "data_is_valid","temperature","humidity","ambient_IR","object_IR","gas_op1_w","gas_op1_r",
                  "gas_op2_w","gas_op2_r","noise")     

cs3 <- subset(cs3, A == 0)

## CS time converter
Time <- data.frame(column_name = cs3$time)
colnames(Time) <- "timestamp_local"
timestamp_local <- as.POSIXct(Time$timestamp, origin='1970-01-01')
timestamp_local <- cbind(my_column = as.data.frame(timestamp_local))
cs3["timestamp_local"] <- timestamp_local$timestamp_local

##omit nas
cs3 <- na.omit(cs3)
##only selects time from calibration - 12 July 15.00 to 19 July 9.00
cs3 <- cs3[cs3$timestamp_local >= "2023-05-12 15:00:00",]
cs3 <- cs3[cs3$timestamp_local <= "2023-05-19 09:00:00",]

#cs4
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/CS_files_calibration_AMS4")
mypath="C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/CS_files_calibration_AMS4"
## Create list of text files
txt_files_ls = list.files(path=mypath, pattern="*.CSV") 
## Read the files in, assuming comma separator
txt_files_df <- lapply(txt_files_ls, function(x) {read.csv(file = x, sep =",", na.strings = "", header = FALSE)})
## Combine them to one df
cs4 <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
colnames(cs4) <- c("A"," deviceID","time","latitude","longitude","PM1","PM25","PM10","bin0",
                   "bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10","bin11",
                   "bin12","bin13","bin14","bin15","bin16","bin17","bin18","bin19","bin20","bin21",
                   "bin22","bin23","flowrate","countglitch","laser_status","temperature_opc","humidity_opc",
                   "data_is_valid","temperature","humidity","ambient_IR","object_IR","gas_op1_w","gas_op1_r",
                   "gas_op2_w","gas_op2_r","noise")      

## CS time converter
Time <- data.frame(column_name = cs4$time)
colnames(Time) <- "timestamp_local"
timestamp_local <- as.POSIXct(Time$timestamp_local, origin='1970-01-01')
timestamp_local <- cbind(my_column = as.data.frame(timestamp_local))
cs4["timestamp_local"] <- timestamp_local$timestamp_local
##omit nas
cs4 <- na.omit(cs4)
##only selects time from calibration - 12 July 15.00 to 19 July 9.00
cs4 <- cs4[cs4$timestamp_local >= "2023-05-12 15:00:00",]
cs4 <- cs4[cs4$timestamp_local <= "2023-05-19 09:00:00",]


#step 2 merge dcmr datasets NO and PM 
dcmr <- merge(dcmr_no, dcmr_pm, by = "timestamp_local")
dcmr <- select(dcmr, -a.x, -b.x, -c.x, -a.y, -b.y, -c.y, -time.y, -timestamp_local_min_1.x, -timestamp_local_min_1.y)


#step 3 - 10 second datasets (similar to dcmr data) for cs3 and cs4 
## cs3
dataset_10sec_cs3 <- data.frame(datetime_clean = seq(from = cs3$timestamp_local[1], to = cs3$timestamp_local[length(cs3$timestamp_local)], by = "10 sec"),
                            value = NA)

###takes out columns value because values are na
dataset_10sec_cs3 <- select(dataset_10sec_cs3, -value)
cs3$PM1 <- as.numeric(cs3$PM1)

###Iterate through the 5-second interval dataset and calculate the average of every two consecutive observations
for (i in 1:length(cs3$timestamp_local)) {
  if (i %% 2 == 1) {
    # Calculate average for every two consecutive observations
    humidity_opc <- mean(cs3$humidity_opc[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs3$humidity_opc[(i+1)/2] <- humidity_opc
    humidity <- mean(cs3$humidity[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs3$humidity[(i+1)/2] <- humidity
    # Calculate average for every two consecutive observations
    temperature_opc <- mean(cs3$temperature_opc[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs3$temperature_opc[(i+1)/2] <- temperature_opc
    # Calculate average for every two consecutive observations
    temperature <- mean(cs3$temperature[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs3$temperature[(i+1)/2] <- temperature
    
    PM1 <- mean(cs3$PM1[i:(i+1)])
    dataset_10sec_cs3$PM1[(i+1)/2] <- PM1
    PM25 <- mean(cs3$PM25[i:(i+1)])
    dataset_10sec_cs3$PM25[(i+1)/2] <- PM25
    PM10 <- mean(cs3$PM10[i:(i+1)])
    dataset_10sec_cs3$PM10[(i+1)/2] <- PM10
    gas_op2_w <- mean(cs3$gas_op2_w[i:(i+1)])
    dataset_10sec_cs3$gas_op2_w[(i+1)/2] <- gas_op2_w
  }
}

sum(is.na(dataset_10sec_cs3))
dataset_10sec_cs3 <- na.omit(dataset_10sec_cs3)

###round the datetime column to the nearest ten seconds
rounded_datetime <- round(as.numeric(dataset_10sec_cs3$datetime_clean) / 10) * 10
rounded_datetime <- as.POSIXct(rounded_datetime, origin = "1970-01-01")
#duplicate dataset_10sec_cs4 without rounded values as backup
dataset_10sec_cs3_check <- dataset_10sec_cs3
#use this dataset to continue
dataset_10sec_cs3$datetime_clean <- rounded_datetime 

##cs4
dataset_10sec_cs4 <- data.frame(datetime_clean = seq(from = cs4$timestamp_local[1], to = cs4$timestamp_local[length(cs4$timestamp_local)], by = "10 sec"),
                                value = NA)

###takes out columns value because values are na
dataset_10sec_cs4 <- select(dataset_10sec_cs4, -value)
cs4$PM1 <- as.numeric(cs4$PM1)

###Iterate through the 5-second interval dataset and calculate the average of every two consecutive observations
for (i in 1:length(cs4$timestamp_local)) {
  if (i %% 2 == 1) {
    # Calculate average for every two consecutive observations
    humidity_opc <- mean(cs4$humidity_opc[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs4$humidity_opc[(i+1)/2] <- humidity_opc
    humidity <- mean(cs4$humidity[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs4$humidity[(i+1)/2] <- humidity
    # Calculate average for every two consecutive observations
    temperature_opc <- mean(cs4$temperature_opc[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs4$temperature_opc[(i+1)/2] <- temperature_opc
    # Calculate average for every two consecutive observations
    temperature <- mean(cs4$temperature[i:(i+1)])
    # Store the averaged value in the corresponding time slot of the 10-second interval dataset
    dataset_10sec_cs4$temperature[(i+1)/2] <- temperature
    
    PM1 <- mean(cs4$PM1[i:(i+1)])
    dataset_10sec_cs4$PM1[(i+1)/2] <- PM1
    PM25 <- mean(cs4$PM25[i:(i+1)])
    dataset_10sec_cs4$PM25[(i+1)/2] <- PM25
    PM10 <- mean(cs4$PM10[i:(i+1)])
    dataset_10sec_cs4$PM10[(i+1)/2] <- PM10
    gas_op2_w <- mean(cs4$gas_op2_w[i:(i+1)])
    dataset_10sec_cs4$gas_op2_w[(i+1)/2] <- gas_op2_w
  }
}

sum(is.na(dataset_10sec_cs4))
dataset_10sec_cs4 <- na.omit(dataset_10sec_cs4)

###round the datetime column to the nearest ten seconds
rounded_datetime <- round(as.numeric(dataset_10sec_cs4$datetime_clean) / 10) * 10
rounded_datetime <- as.POSIXct(rounded_datetime, origin = "1970-01-01")
#duplicate dataset_10sec_cs4 without rounded values as backup
dataset_10sec_cs4_check <- dataset_10sec_cs4
#use this dataset to continue
dataset_10sec_cs4$datetime_clean <- rounded_datetime 

#step 4 - merge cs dataframes
dataset_10sec_cs <- merge(dataset_10sec_cs3, dataset_10sec_cs4, by = "datetime_clean", suffixes = c("_cs3", "_cs4"))

#step 5 - make combined dataframe and write.csv
## Change the name of the "rounded_datetime" column to "timestamp_local"
dataset_10sec_cs$timestamp_local <- dataset_10sec_cs$datetime_clean
##create final dataset
final_set <- merge(dcmr, dataset_10sec_cs, by = "timestamp_local")
##write final_set to csv  (in desired wd)
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/Output")
write.csv(final_set, "dcmrNO_dcmrPM_cs3_cs4_combined.csv")

#step 6 test and plots
sum(is.na(dataset_10sec_cs))
ggplot(data=final_set,aes(x=timestamp_local, y=(NO2_ppb))) +
geom_point()


