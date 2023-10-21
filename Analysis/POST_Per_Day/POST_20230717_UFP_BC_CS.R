#Script to load datasets, clean & merge - per day 
#MR_20231003
#Readme
#Step 1: Load datasets UFP, BC, CS3, + make into dfs + same timestamp (in this case: between 17-07-2023 6:21 - 17-07-2023 10:58)
#Step 2: Plots and analysis per pollutant
#Step 3: Plots and analysis together
#Step 4: Analyse break 
#Step 5: Merge dataframes on timestamp, write.csv combined dataset
#end Readme

#Packages
library(randomForest)
library(rio)
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(ggmap)
library(leaflet)
library(viridis)
library(scales)
library(GGally)
library(zoo)
library(patchwork)
library(grid)
library(anytime)
library(data.table)
library(xts)
library(ggplot2)
library(tidyr)
library(glue)
library(ggtext)
library(cowplot)
library(ggpubr)
library(magrittr)
library(gridExtra)

#Step 1 - import and clean data
##cs3 
setwd("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/CS_AMS3/2023_07_17")
mypath <- "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/CS_AMS3/2023_07_17"

### Create list of text files, read files assuming comma separator, combine in one df
txt_files_ls = list.files(path=mypath, pattern="*.CSV") 
txt_files_df <- lapply(txt_files_ls, function(x) {read.csv(file = x, sep =",", na.strings = "", header = FALSE)})
cs3 <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
colnames(cs3) <- c("A"," deviceID","time","latitude","longitude","PM1","PM25","PM10","bin0",
                   "bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10","bin11",
                   "bin12","bin13","bin14","bin15","bin16","bin17","bin18","bin19","bin20","bin17",
                   "bin22","bin23","flowrate","countglitch","laser_status","temperature_opc","humidity_opc",
                   "data_is_valid","temperature","humidity","ambient_IR","object_IR","gas_op1_w","gas_op1_r",
                   "gas_op2_w","gas_op2_r","noise")     

### Clean data: 1. A are the valid rows, 2. humidity levels must not exceed 85%, 3. omit nas.
cs3 <- subset(cs3, A == 0)
cs3 <- subset(cs3, humidity < 85.0) 
cs3 <- na.omit(cs3)

### CS time converter
Time <- data.frame(column_name = cs3$time)
colnames(Time) <- "time_local"
time_local <- as.POSIXct(Time$time_local, origin='1970-01-01')
time_local <- cbind(my_column = as.data.frame(time_local))
cs3["time_local"] <- time_local$time_local

###selects timeframe measured (6.21-11.00)
cs3 <- cs3[cs3$time_local >= "2023-07-17 6:21:00",]
cs3 <- cs3[cs3$time_local <= "2023-07-17 11:00:00",]

#Calibration PM25 and NO2
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/Output")
loaded_rf_pm25 <- readRDS(file = "rf_CS3_pm25.rds")
cs3$PM25_cs3 <- cs3$PM25
cs3$temperature_opc_cs3 <- cs3$temperature_opc
cs3$humidity_opc_cs3 <- cs3$humidity_opc
pred_17072023_pm25 <- predict(loaded_rf_pm25, cs3)
cs3$PM25_cs3 <- pred_17072023_pm25
summary(pred_17072023_pm25)

loaded_rf_no2 <- readRDS(file = "rf_CS3_no2.rds")
cs3$gas_op2_w_cs3 <- cs3$gas_op2_w
cs3$temperature_opc_cs3 <- cs3$temperature_opc
cs3$humidity_opc_cs3 <- cs3$humidity_opc
pred_17072023_no2 <- predict(loaded_rf_no2, cs3)
cs3$gas_op2_w_cs3 <- pred_17072023_no2
summary(pred_17072023_no2)

##UFP 
Partector <- read_delim("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/UFP/2023_07_17/8365_062156.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE, skip = 18) #trims off header lines
 
Partector_start=as_datetime("2023-07-17 06:22:13 CET")
Partector <- Partector %>%
  mutate(Part_time = time + Partector_start)

# Calculate standard deviation
sd_value <- sd(Partector$number)

# Perform a t-test (assuming you want to compare it to a hypothetical mean, e.g., 0)
t_test_result <- t.test(Partector$number, mu = 0)
t_test_result

##BC
ObservAirDST <- read_csv("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/BC_Julie/Data34_230717_06h04m/Data34_2023-07-17.txt", 
                         col_names = c("TS", "Iref", "Isig", "ATN", "BC", "RH", "Temp", "FR", "Vbat", "GPSlat", "GPSlong"))

ObservAirDST <- ObservAirDST %>%
  mutate(DST_time = as_datetime(TS))

ObservAirDST <- na.omit(ObservAirDST)
ObservAirDST<- subset(ObservAirDST, BC > 0)

#step 2 - Time series all four sensors individually (ggplot)
## Plots for UFP 
###Plot(UFP1: Whole sampling time 17 July 2023 UFP 
Partector_Filter <- Partector %>% filter(Part_time >= as_datetime("2023-07-17 06:21:00 CET") & Part_time <= as_datetime("2023-07-17 11:00:00 CET"))

UFP <- ggplot(data=Partector_Filter, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "Number",
       title = "Number",
       subtitle = "17 July 2023 - POST",
       caption = "Data: Partector 2",
       tag = "Fig. 1") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-1000,50000) +
  scale_x_datetime(
    date_breaks = "20 mins",
    date_labels = "%H:%M"
  )
UFP
###Plot_UFP2:whole sampling time period 17 July 2023 UFP - using viridis turbo
ggplot(data = Partector_Filter, aes(x = Part_time, y = number, color = number)) +
  geom_line(size = .5) +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "UFP",
    subtitle = "17 July 2023 - POST",
    caption = "Data: Partector 2",
    tag = "Fig. 1"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  geom_rug() +
  ylim(-1000, 55000) +
  scale_color_viridis_c(option = "turbo") +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

###Plot_UFP3:boxplot all period 17 July 2023 UFP, including mean 
mean_working <- mean(Partector$number)
summary(Partector$number)
ggplot(data = Partector, aes(x = Part_time, y = number)) +
  geom_boxplot(fill = "darkred", alpha = 0.2) +
  geom_text(x = max(Partector_Filter$Part_time), y = mean_working, label = paste("Mean:", mean_working), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "17 July 2023 - POST Rotterdam",
    subtitle = "Box Plot of Number - Working Crane SK2400-R",
    caption = "Data: Partector",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  )

###Plot_UFP4:Geom_point plot 17 July 2023 UFP
ggplot(Partector, aes(x=Part_time, y=number, colour=diam)) +
  geom_point(size = .5)

###Plot_UFP5: Map UFP
Partector_map <- Partector 
ObservAirDST_map <- ObservAirDST
ObservAirDST_map$Part_time <-ObservAirDST_map$DST_time
##Full join
Par_DST_join <- ObservAirDST_map %>% full_join(Partector_map, by="Part_time")
Par_DST_join <-Par_DST_join %>%
  filter(Part_time >= as_datetime("2023-07-17 06:38:00 CET") & Part_time <= as_datetime("2023-07-17 11:00:00 CET") & GPSlat >= 51.0 & GPSlat <= 52.00 & GPSlong >= 4.48 & GPSlong <= 4.49)

POST17_UFP <- qmplot(data = Par_DST_join,  
                    x = GPSlong, y = GPSlat, 
                    geom = "blank", 
                    zoom = 18, 
                    maptype = "toner-lite", 
                    darken = c(0.2, "white")) +
  geom_point(aes(color = number), alpha = 0.8, size = 4.0) + 
  scale_color_viridis(option = "turbo", limits = c(1000,15000)) +
  scale_size("number") + 
  theme_void() + theme(title = element_text(size = 12), legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.text = element_text(size = 15), legend.title = element_text(size =15)) + 
  labs(title = "Number map 17 July 2023", color = "Number")

POST17_UFP


##Plots for BC
###Plot_BC1: 
ObservAirDST <- ObservAirDST %>% filter(DST_time >= as_datetime("2023-07-17 6:21:00 CET") & DST_time <= as_datetime("2023-07-17 11:00:00 CET"))

BC <- ggplot(data=ObservAirDST, aes(x=DST_time, y=BC, color = BC)) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "BC",
       title = "BC",
       subtitle = "17 July 2023 - POST",
       caption = "Data: DST",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-17 06:35:00 CET"),as_datetime("2023-07-17 11:30:00 CET"))+
  ylim(0,20)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
BC

###Plot_BC2: Check some diagnostics to see why BC signal went to 0?
ggplot(data=ObservAirDST, aes(x=DST_time, y=Vbat)) +
  geom_line() +
  theme_minimal()

ggplot(data=ObservAirDST, aes(x=DST_time, y=ATN)) +
  geom_line() +
  theme_minimal()

###Plot_BC3: boxplot all period, including mean 
mean_working_bc <- mean(ObservAirDST$BC)

ggplot(data = ObservAirDST, aes(x = DST_time, y = BC)) +
  geom_boxplot(fill = "darkred", alpha = 0.2, coef = 0) +
  geom_text(x = max(ObservAirDST_filter$DST_time), y = mean_working_bc, label = paste("Mean:", mean_working_bc), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "BC",
    title = "17 July 2023 - POSTRotterdam",
    subtitle = "Boxplot of BC - Working Crane SK2400-R",
    caption = "Data: DST",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(-0.5, 3.0))

###:Plot_BC4: Summary BC
summary(ObservAirDST$BC)

###:Plot_BC5: Map BC
ObservAirDST_filter <- ObservAirDST %>%
  filter(DST_time >= as_datetime("2023-07-17 06:38:00 CET") & DST_time <= as_datetime("2023-07-17 11:00:00 CET") & GPSlat >= 51.0 & GPSlat <= 52.00 & GPSlong >= 4.47 & GPSlong <= 4.9)
POST17_BC <- qmplot(data = ObservAirDST_filter,  
                   x = GPSlong, y = GPSlat, 
                   geom = "blank", 
                   zoom = 18, 
                   maptype = "toner-lite", 
                   darken = c(0.2, "white")) +
  geom_point(aes(color = BC), alpha = 0.8, size = 4.0) + 
  scale_color_viridis(option = "turbo", limits = c(0, 5)) +
  scale_size("BC") + 
  theme_void() + theme(title = element_text(size = 12), legend.position = "bottom", 
                       legend.key.width = unit(2, 'cm'), legend.text = element_text(size = 15), 
                       legend.title = element_text(size =15)) + 
  labs(title = "BC map 17 July 2023", color = "BC") 

POST17_BC
#CS
cs3_filter <- cs3 %>%
  filter(time_local >= as_datetime("2023-07-17 04:21:00 CET") & time_local <= as_datetime("2023-07-17 11:00:00 CET")) 

PM25 <- ggplot(data=cs3_filter, aes(x=time_local, y=PM25_cs3, color = PM25)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "PM25",
       title = "PM25",
       subtitle = "17 July 2023 - POST",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-21 06:35:00 CET"),as_datetime("2023-07-21 11:30:00 CET"))+
  ylim(0,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
PM25

PM25 <- ggplot(data=cs3_filter, aes(x=time_local, y=PM25, color = PM25)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "PM25",
       title = "PM25",
       subtitle = "17 July 2023 - POST",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  xlim(as_datetime("2023-07-17 06:35:00 CET"),as_datetime("2023-07-17 9:30:00 CET"))+
  ylim(0,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
PM25


NO2 <- ggplot(data=cs3, aes(x=time_local, y=gas_op2_w_cs3, color = gas_op2_w_cs3)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "NO2",
       title = "NO2",
       subtitle = "17 July 2023 - POST",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-21 06:35:00 CET"),as_datetime("2023-07-21 11:30:00 CET"))+
  ylim(0,40) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
NO2
NO2 <- ggplot(data=cs3, aes(x=time_local, y=gas_op2_w, color = gas_op2_w_cs3)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "NO2",
       title = "NO2",
       subtitle = "17 July 2023 - Switi",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  xlim(as_datetime("2023-07-21 06:35:00 CET"),as_datetime("2023-07-21 11:30:00 CET"))+
  ylim(300,375) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
NO2
#cs3_filter <- cs3_filter %>%
 # filter(time_local >= as_datetime("2023-07-17 06:38:00 CET") & time_local <= as_datetime("2023-07-17 11:00:00 CET") & latitude >= 51.00 & latitude <= 52.00 & longitude >= 4.48 & longitude <= 4.49)

# POST17_CS <- qmplot(data = cs3_filter,  
#                    x = longitude, y = latitude, 
#                    geom = "blank", 
#                    zoom = 18, 
#                    maptype = "toner-lite", 
#                    darken = c(0.2, "white")) +
#   geom_point(aes(color = PM25), alpha = 0.8, size = 4.0) + 
#   scale_color_viridis(option = "turbo", limits = c(0, 5)) +
#   scale_size("PM25") + 
#   theme_void() + theme(title = element_text(size = 12), legend.position = "bottom", 
#                        legend.key.width = unit(2, 'cm'), legend.text = element_text(size = 15), 
#                        legend.title = element_text(size =15)) + 
#   labs(title = "BC map 17 July 2023", color = "PM25") 
# 
# POST17_CS

#Step 3: Plot together
UFP2 <- ggplot(data=Partector_Filter, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "Particles") +
  theme(axis.text.x=element_blank()) + 
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-1000,50000) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

BC2 <- ggplot(data=ObservAirDST, aes(x=DST_time, y=BC, color = BC)) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "BC") +
  theme(axis.text.x=element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-17 06:35:00 CET"),as_datetime("2023-07-17 11:30:00 CET"))+
  ylim(0,8)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

NO22 <- ggplot(data=cs3_filter, aes(x=time_local, y=gas_op2_w_cs3, color = gas_op2_w_cs3)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "NO2") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(5,25) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

PM252 <- ggplot(data=cs3_filter, aes(x=time_local, y=PM25, color = PM25)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "PM25") +
  theme(axis.text.x=element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-21 06:35:00 CET"),as_datetime("2023-07-21 11:30:00 CET"))+
  ylim(0,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

POST_sampling <- UFP2 / BC2

POST_sampling + plot_annotation(title="POST Rotterdam 17-07-2023",
                                 subtitle= "UFP & BC")

POST_sampling <- PM252 / NO22

POST_sampling + plot_annotation(title="POST Rotterdam 17-07-2023",
                                 subtitle= "PM25 & NO2")
# 

POST_sampling <- UFP2 / BC2/ PM252 / NO22

POST_sampling + plot_annotation(title="POST Rotterdam 17-07-2023",
                                subtitle= "UFP & BC PM25 & NO2")



#Step 5: Analysing the breaks

#6.21-06.49 Drizzling. No activity. - green 
#6.50-7.50 Cement mixing  - red
#8.11-8.37 New location - blue
#8.37-8.57 New location - red
#8.57-9.22 Pauze - yellow
#9:23-10.14 Working - red 
#10.14- 10.24 new location - blue
#10.30-10.56 new location - red


# "2023-07-17 06:21:00", "2023-07-17 06:49:00"
# "2023-07-17 06:50:00", "2023-07-17 7:50:00"
# "2023-07-17 08:11:00", "2023-07-17 8:37:00"
# "2023-07-17 08:37:00", "2023-07-17 8:57:00"
# "2023-07-17 08:57:00", "2023-07-17 9:22:00"
# "2023-07-17 09:23:00", "2023-07-17 10:14:00"
# "2023-07-17 10:14:00", "2023-07-17 10:24:00"
# "2023-07-17 10:30:00", "2023-07-17 11:56:00"



intervals <- data.frame(
  start = as.POSIXct(c("2023-07-17 06:21:00", "2023-07-17 06:50:00", "2023-07-17 08:11:00", "2023-07-17 08:37:00", "2023-07-17 08:57:00", "2023-07-17 09:23:00", "2023-07-17 10:14:00", "2023-07-17 10:30:00"), tz = "Europe/Amsterdam"),
  end = as.POSIXct(c("2023-07-17 06:49:00", "2023-07-17 7:50:00", "2023-07-17 8:37:00", "2023-07-17 8:57:00","2023-07-17 9:22:00","2023-07-17 10:14:00", "2023-07-17 10:24:00", "2023-07-17 11:56:00"), tz = "Europe/Amsterdam"),
  label = c("Interval 1", "Interval 2", "Interval 3", "Interval 4", "Interval 5", "Interval 6", "Interval 7", "Interval 8")
)

intervals$start <- intervals$start - hours(2)
intervals$end <- intervals$end - hours(2)

intervals$label <- factor(intervals$label, levels = c("Interval 1", "Interval 2", "Interval 3", "Interval 4", "Interval 5", "Interval 6", "Interval 7", "Interval 8"))

Interval <- ggplot() +
  geom_line(data = Partector, aes(x = Part_time, y = number)) +
  geom_rect(data = intervals, aes(xmin = as.POSIXct(start), xmax = as.POSIXct(end), ymin = -Inf, ymax = Inf, fill = label), alpha = 0.3) +
  labs(title = "UFP Intervals work vs break",
       x = "Timestamp",
       y = "Number") +
  scale_fill_manual(values = c("Interval 1" = "green", "Interval 2" = "red", "Interval 3" = "blue", "Interval 4" = "red", "Interval 5" = "yellow", "Interval 6" = "red", "Interval 7" = "blue", "Interval 8" = "red")) +
  theme_minimal()
Interval


# Interval2 <- ggplot() +
#   geom_line(data = ObservAirDST, aes(x = DST_time, y = BC)) +
#   geom_rect(data = intervals, aes(xmin = as.POSIXct(start), xmax = as.POSIXct(end), ymin = -Inf, ymax = Inf, fill = label), alpha = 0.3) +
#   labs(title = "BC Intervals work vs break",
#        x = "Timestamp",
#        y = "Number") +
#   scale_fill_manual(values = c("Interval 1" = "green", "Interval 2" = "red", "Interval 3" = "blue", "Interval 4" = "red", "Interval 5" = "yellow", "Interval 6" = "red", "Interval 7" = "blue", "Interval 8" = "red")) +
#   theme_minimal()
# Interval2


combined_table <- intervals %>%
  rowwise() %>%
  mutate(
    mean_value_UFP = mean(Partector$number[Partector$Part_time >= start & Partector$Part_time <= end]),
    mean_value_BC = mean(ObservAirDST$BC[ObservAirDST$DST_time >= start & ObservAirDST$DST_time <= end])
  ) %>%
  select(start, end, mean_value_UFP, mean_value_BC)
combined_table$start <- combined_table$start - hours(2)
combined_table$start <- combined_table$end - hours(2)
View(combined_table)

#Write csv2
write.csv(combined_table, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/Table_20230717_POST.CSV")

intervals$start <- intervals$start - hours(2)
intervals$end <- intervals$end - hours(2)

Interval3 <- ggplot() +
  geom_line(data = cs3, aes(x = time_local, y = PM25)) +
  geom_rect(data = intervals, aes(xmin = as.POSIXct(start), xmax = as.POSIXct(end), ymin = -Inf, ymax = Inf, fill = label), alpha = 0.3) +
  labs(title = "UFP Intervals work vs break",
       x = "Timestamp",
       y = "PM25") +
  scale_fill_manual(values = c("Interval 1" = "green", "Interval 2" = "red", "Interval 3" = "yellow", "Interval 4" = "red")) +
  theme_minimal()
Interval3

Interval4 <- ggplot() +
  geom_line(data = cs3, aes(x = time_local, y = gas_op2_w)) +
  geom_rect(data = intervals, aes(xmin = as.POSIXct(start), xmax = as.POSIXct(end), ymin = -Inf, ymax = Inf, fill = label), alpha = 0.3) +
  labs(title = "UFP Intervals work vs break",
       x = "Timestamp",
       y = "PM25") +
  scale_fill_manual(values = c("Interval 1" = "green", "Interval 2" = "red", "Interval 3" = "yellow", "Interval 4" = "red")) +
  theme_minimal()
Interval4


ggplot() + geom_line(data=cs3, aes(x = time_local, y = humidity))


#to do:
#1. Make nice graphs per day
#2. note times working vs pauze. Then use that to filter data, and subsets make violin plots. 
#3. calculate averages (/average change) with or without pauze
#4. use the randomforest model onto our new data collected from sites
#5. Look into peaks per day (with notes)

#write.csv(cs3, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/CS_20230717_POST.CSV") 
#write.csv(Partector, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/UFP_20230717_POST.CSV") 
#write.csv(ObservAirDST, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/BC_20230717_POST.CSV") 
