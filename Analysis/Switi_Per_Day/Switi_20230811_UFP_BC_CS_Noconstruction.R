#Script to load datasets, clean & merge - per day 
#MR_20231011
#Readme
#Step 1: Load datasets UFP, BC, CS3 + make into dfs + same timestamp (in this case: between 11-08-2023 07:18:00 - 11-08-2023 10:30)
#Step 2: Plots and analysis per pollutant - UFP, BC, PM25, NO2 
#Step 3: Overview plots
#Step 4: Analyse break 
#Step 5: Write.csv datasets
#End Readme 

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

#Step 1 - import and clean data
##cs3 
setwd("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/CS_AMS3/2023_08_11(No_construction)/")
mypath <- "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/CS_AMS3/2023_08_11(No_construction)/"

### Create list of text files, read files assuming comma separator, combine in one df
txt_files_ls = list.files(path=mypath, pattern="*.CSV") 
txt_files_df <- lapply(txt_files_ls, function(x) {read.csv(file = x, sep =",", na.strings = "", header = FALSE)})
cs3 <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 
colnames(cs3) <- c("A"," deviceID","time","latitude","longitude","PM1","PM25","PM10","bin0",
                   "bin1","bin2","bin3","bin4","bin5","bin6","bin7","bin8","bin9","bin10","bin11",
                   "bin12","bin13","bin14","bin15","bin16","bin17","bin18","bin19","bin20","bin21",
                   "bin22","bin23","flowrate","countglitch","laser_status","temperature_opc","humidity_opc",
                   "data_is_valid","temperature","humidity","ambient_IR","object_IR","gas_op1_w","gas_op1_r",
                   "gas_op2_w","gas_op2_r","noise")     

### Clean data: 1. A are the valid rows, 2. humidity levels must not exceed 85%, 3. omit nas.
cs3 <- subset(cs3, A == 0)
cs3 <- subset(cs3, humidity<85.0)
cs3 <- na.omit(cs3)

### CS time converter
Time <- data.frame(column_name = cs3$time)
colnames(Time) <- "time_local"
time_local <- as.POSIXct(Time$time_local, origin='1970-01-01')
time_local <- cbind(my_column = as.data.frame(time_local))
cs3["time_local"] <- time_local$time_local

###selects timeframe measured (7:18-13.00)
cs3 <- cs3[cs3$time_local >= "2023-08-11 07:18:00",]
cs3 <- cs3[cs3$time_local <= "2023-08-11 13:00:00",]

###Calibration PM25 and NO2
setwd("C:/Users/Miny Rajiv/Documents/RScripts_Cityscanner/Calibration/Output")
loaded_rf_pm25 <- readRDS(file = "rf_CS3_pm25.rds")
cs3$PM25_cs3 <- cs3$PM25
cs3$temperature_opc_cs3 <- cs3$temperature_opc
cs3$humidity_opc_cs3 <- cs3$humidity_opc
pred_11082023_pm25 <- predict(loaded_rf_pm25, cs3)
cs3$PM25_cs3 <- pred_11082023_pm25
summary(pred_11082023_pm25)

loaded_rf_no2 <- readRDS(file = "rf_CS3_no2.rds")
cs3$gas_op2_w_cs3 <- cs3$gas_op2_w
cs3$temperature_opc_cs3 <- cs3$temperature_opc
cs3$humidity_opc_cs3 <- cs3$humidity_opc
pred_11082023_no2 <- predict(loaded_rf_no2, cs3)
cs3$gas_op2_w_cs3 <- pred_11082023_no2
summary(pred_11082023_no2)

##UFP 
Partector <- read_delim("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/UFP/2023_08_11_(No_Construction)/8365_071745.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE, skip = 18) #trims off header lines
 
Partector_start=as_datetime("2023-08-11 07:18:01 CET")
Partector <- Partector %>%
  mutate(Part_time = time + Partector_start)

### Calculate standard deviation
sd_value <- sd(Partector$number)

### Perform a t-test (assuming you want to compare it to a hypothetical mean, e.g., 0)
t_test_result <- t.test(Partector$number, mu = 0)
t_test_result

##BC
ObservAirDST <- read_csv("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/BC_Julie/Data34_230811_06h46m_(No_Construction)/Data34_2023-08-11.txt", 
                         col_names = c("TS", "Iref", "Isig", "ATN", "BC", "RH", "Temp", "FR", "Vbat", "GPSlat", "GPSlong"))

ObservAirDST <- ObservAirDST %>%
  mutate(DST_time = as_datetime(TS))

###Clean data 
ObservAirDST <- na.omit(ObservAirDST)
ObservAirDST<- subset(ObservAirDST, BC > 0)

#Step 2 - Analysis per sensor - UFP, BC, PM25, NO2
## Plots for UFP 
###UFP_a: Whole sampling time 12 July 2023 UFP 
UFP <- ggplot(data=Partector, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "Number",
       title = "Number",
       subtitle = "21 July 2023 - Switi",
       caption = "Data: Partector 2",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  xlim(as_datetime("2023-07-12 08:20:00 CET"),as_datetime("2023-07-12 13:00:00 CET"))+
  ylim(-1000,180000) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
UFP
###UFP_b:whole sampling time period 12 July 2023 UFP - using viridis turbo
ggplot(data = Partector, aes(x = Part_time, y = number, color = number)) +
  geom_line(size = .5) +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "UFP",
    subtitle = "12 July 2023 - Switi",
    caption = "Data: Partector 2",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8),
    legend.position = "none"
  ) +
  geom_rug() +
  ylim(-1000, 180000) +
  scale_color_viridis_c(option = "turbo") +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

###UFP_c:boxplot all period 12 July 2023 UFP, including mean 
Partector_Filter <- Partector %>% filter(Part_time >= as_datetime("2023-07-12 08:25:00 CET") & Part_time <= as_datetime("2023-07-12 13:00:00 CET"))
mean_working <- mean(Partector$number)
ggplot(data = Partector, aes(x = Part_time, y = number)) +
  geom_boxplot(fill = "darkred", alpha = 0.2) +
  geom_text(x = max(Partector_Filter$Part_time), y = mean_working, label = paste("Mean:", mean_working), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "12 July 2023 - Switi Amsterdam",
    subtitle = "Box Plot of Number - Working Crane SK2400-R",
    caption = "Data: Partector",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  )

###UFP_d:Geom_point plot 12 July 2023 UFP
ggplot(Partector, aes(x=Part_time, y=number, colour=diam)) +
  geom_point(size = .5)

###UFP_e: Map UFP
Partector_map <- Partector 
ObservAirDST_map <- ObservAirDST
ObservAirDST_map$Part_time <-ObservAirDST_map$DST_time
##Full join
Par_DST_join <- ObservAirDST_map %>% full_join(Partector_map, by="Part_time")

Par_DST_join <-Par_DST_join %>%
  filter(Part_time >= as_datetime("2023-07-12 08:25:00 CET") & Part_time <= as_datetime("2023-07-12 13:00:00 CET") & GPSlat >= 52.3 & GPSlat <= 52.45 & GPSlong >= 4.75 & GPSlong <= 4.98)

Swi21_UFP <- qmplot(data = Par_DST_join,  
                    x = GPSlong, y = GPSlat, 
                    geom = "blank", 
                    zoom = 18, 
                    maptype = "toner-lite", 
                    darken = c(0.2, "white")) +
  geom_point(aes(color = number), alpha = 0.8, size = 4.0) + 
  scale_color_viridis(option = "turbo", limits = c(1000,15000)) +
  scale_size("number") + 
  theme_void() + theme(title = element_text(size = 12), legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.text = element_text(size = 15), legend.title = element_text(size =15)) + 
  labs(title = "Number map 21 July 2023", color = "Number")

Swi21_UFP

##Plots for BC
###BC_a: 
ObservAirDST <- ObservAirDST %>% filter(DST_time >= as_datetime("2023-07-12 8:25:00 CET") & DST_time <= as_datetime("2023-07-12 13:00:00 CET"))

BC <- ggplot(data=ObservAirDST, aes(x=DST_time, y=BC, color = BC)) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "BC",
       title = "BC",
       subtitle = "21 July 2023 - Switi",
       caption = "Data: DST",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-10,55)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
BC

###BC_b: Check some diagnostics to see why BC signal went to 0?
ggplot(data=ObservAirDST, aes(x=DST_time, y=Vbat)) +
  geom_line() +
  theme_minimal()

ggplot(data=ObservAirDST, aes(x=DST_time, y=ATN)) +
  geom_line() +
  theme_minimal()

###BC_c: boxplot all period, including mean 
mean_working_bc <- mean(ObservAirDST$BC)

ggplot(data = ObservAirDST, aes(x = DST_time, y = BC)) +
  geom_boxplot(fill = "darkred", alpha = 0.2, coef = 0) +
  geom_text(x = max(ObservAirDST$DST_time), y = mean_working_bc, label = paste("Mean:", mean_working_bc), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "BC",
    title = "12 July 2023 - Switi",
    subtitle = "Boxplot of BC - Working Crane SK2400-R",
    caption = "Data: DST",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  ) +
  coord_cartesian(ylim = c(-0.5, 3.0))

###BC_d: Summary BC
summary(ObservAirDST$BC)

###BC_e: Map BC
ObservAirDST_filter <- ObservAirDST %>%
  filter(DST_time >= as_datetime("2023-07-12 08:25:00 CET") & DST_time <= as_datetime("2023-07-12 13:00:00 CET") & GPSlat >= 52.3 & GPSlat <= 52.45 & GPSlong >= 4.75 & GPSlong <= 4.98)

Swi21_BC <- qmplot(data = ObservAirDST_filter,  
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
  labs(title = "BC map 12 July 2023", color = "BC") 

Swi21_BC

##PM25
PM25 <- ggplot(data=cs3, aes(x=time_local, y=PM25, color = PM25)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "PM25",
       title = "PM25",
       subtitle = "12 July 2023 - Switi",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(0,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
PM25

# Swi21_CS <- qmplot(data = cs3,  
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
#   labs(title = "BC map 21 July 2023", color = "PM25") 
# 
# Swi21_CS

##NO2
NO2 <- ggplot(data=cs3, aes(x=time_local, y=gas_op2_w_cs3, color = gas_op2_w_cs3)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "NO2",
       title = "PM25",
       subtitle = "12 July 2023 - Switi",
       caption = "Data: CS",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(5,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
NO2

#Step 3: Overview Plots
Switi_sampling <- UFP / BC
Switi_sampling + plot_annotation(title="Switi Amsterdam 11-08-2023",
                                 subtitle= "UFP & BC - No construction")
Switi_sampling <- PM25 / NO2
Switi_sampling + plot_annotation(title="Switi Amsterdam 11-08-2023",
                                 subtitle= "PM25 & NO2- No construction")


UFP2 <- ggplot(data=Partector, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "Particles") +
  theme(axis.text.x=element_blank()) + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-1000,30000) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

BC2 <- ggplot(data=ObservAirDST, aes(x=DST_time, y=BC, color = BC)) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "BC") +
  theme(axis.text.x=element_blank()) + 
  theme(legend.position = "none") +
  geom_rug() +
  ylim(0,6)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

NO22 <- ggplot(data=cs3, aes(x=time_local, y=gas_op2_w_cs3, color = gas_op2_w_cs3)) +
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

PM252 <- ggplot(data=cs3, aes(x=time_local, y=PM25, color = PM25)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "", y = "PM25") +
  theme(axis.text.x=element_blank()) + 
  theme(legend.position = "none") +
  geom_rug() +
  ylim(0,30) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )

Switi_sampling <- UFP2 / BC2/ PM252 / NO22
Switi_sampling + plot_annotation(title="Switi Amsterdam 11-08-2023",
                                 subtitle= "UFP, BC, PM25 & NO2 - No construction")
#Step 4: Analysing the breaks


#Step 5: write dfs to csv
#write.csv(cs3, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/CS_20230811_Switi.CSV") 
#write.csv(Partector, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/UFP_20230811_Switi.CSV") 
#write.csv(ObservAirDST, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/BC_20230811_Switi.CSV") 


