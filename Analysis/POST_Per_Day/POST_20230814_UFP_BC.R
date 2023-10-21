#Script to load datasets, clean & merge - per day 
#MR_20231015
#Readme
#Nb. No CS today
#Step 1: Load datasets UFP, BC + make into dfs + same timestamp (in this case: between 14-08-2023 08:04:57 - 14-08-2023 10:58)
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

##UFP 
Partector <- read_delim("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/UFP/2023_08_14(No_Construction)/8365_080440.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE, skip = 18) #trims off header lines
 
Partector_start=as_datetime("2023-08-14 08:04:57 CET")
  Partector <- Partector %>%
  mutate(Part_time = time + Partector_start)

# Calculate standard deviation
sd_value <- sd(Partector$number)

# Perform a t-test (assuming you want to compare it to a hypothetical mean, e.g., 0)
t_test_result <- t.test(Partector$number, mu = 0)
t_test_result

##BC
ObservAirDST <- read_csv("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/POST/BC_Julie/Data34_230814_07h59m(no_construction)/Data34_2023-08-14.txt", 
                         col_names = c("TS", "Iref", "Isig", "ATN", "BC", "RH", "Temp", "FR", "Vbat", "GPSlat", "GPSlong"))

ObservAirDST <- ObservAirDST %>%
  mutate(DST_time = as_datetime(TS))
ObservAirDST <- na.omit(ObservAirDST)
ObservAirDST<- subset(ObservAirDST, BC > 0)


#step 2 - Time series all four sensors individually (ggplot)
## Plots for UFP 
###Plot(UFP1: Whole sampling time 14 August 2023 UFP 
Partector_Filter <- Partector %>% filter(Part_time >= as_datetime("2023-08-14 08:04:00 CET") & Part_time <= as_datetime("2023-08-14 11:00:00 CET"))

UFP <- ggplot(data=Partector_Filter, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "Number",
       title = "Number",
       subtitle = "14 August 2023 - POST",
       caption = "Data: Partector 2",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-1000,50000) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
UFP
###Plot_UFP2:whole sampling time period 14 August 2023 UFP - using viridis turbo
ggplot(data = Partector_Filter, aes(x = Part_time, y = number, color = number)) +
  geom_line(size = .5) +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "UFP",
    subtitle = "14 August 2023 - POST",
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

###Plot_UFP3:boxplot all period 14 August 2023 UFP, including mean 
mean_working <- mean(Partector$number)
ggplot(data = Partector, aes(x = Part_time, y = number)) +
  geom_boxplot(fill = "darkred", alpha = 0.2) +
  geom_text(x = max(Partector_Filter$Part_time), y = mean_working, label = paste("Mean:", mean_working), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "14 August 2023 - POST Rotterdam",
    subtitle = "Box Plot of Number - Working Crane SK2400-R",
    caption = "Data: Partector",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  )

###Plot_UFP4:Geom_point plot 14 August 2023 UFP
ggplot(Partector, aes(x=Part_time, y=number, colour=diam)) +
  geom_point(size = .5)

###Plot_UFP5: Map UFP
Partector_map <- Partector 
ObservAirDST_map <- ObservAirDST
ObservAirDST_map$Part_time <-ObservAirDST_map$DST_time
##Full join
Par_DST_join <- ObservAirDST_map %>% full_join(Partector_map, by="Part_time")
Par_DST_join <-Par_DST_join %>%
  filter(Part_time >= as_datetime("2023-08-14 08:05:00 CET") & Part_time <= as_datetime("2023-08-14 11:00:00 CET") & GPSlat >= 51.0 & GPSlat <= 52.00 & GPSlong >= 4.48 & GPSlong <= 4.49)

POST14_UFP <- qmplot(data = Par_DST_join,  
                     x = GPSlong, y = GPSlat, 
                     geom = "blank", 
                     zoom = 18, 
                     maptype = "toner-lite", 
                     darken = c(0.2, "white")) +
  geom_point(aes(color = number), alpha = 0.8, size = 4.0) + 
  scale_color_viridis(option = "turbo", limits = c(1000,15000)) +
  scale_size("number") + 
  theme_void() + theme(title = element_text(size = 12), legend.position = "bottom", legend.key.width = unit(2, 'cm'), legend.text = element_text(size = 15), legend.title = element_text(size =15)) + 
  labs(title = "Number map 14 August 2023", color = "Number")

POST14_UFP

##Plots for BC
###Plot_BC1: 
ObservAirDST <- ObservAirDST %>% filter(DST_time >= as_datetime("2023-08-14 8:05:00 CET") & DST_time <= as_datetime("2023-08-14 11:00:00 CET"))

BC <- ggplot(data=ObservAirDST, aes(x=DST_time, y=BC, color = BC)) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "BC",
       title = "BC",
       subtitle = "14 August 2023 - POST",
       caption = "Data: DST",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
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
    title = "14 August 2023 - POSTRotterdam",
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
  filter(DST_time >= as_datetime("2023-08-14 08:05:00 CET") & DST_time <= as_datetime("2023-08-14 11:00:00 CET") & GPSlat >= 51.0 & GPSlat <= 52.00 & GPSlong >= 4.47 & GPSlong <= 4.9)
POST14_BC <- qmplot(data = ObservAirDST_filter,  
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
  labs(title = "BC map 14 August 2023", color = "BC") 

POST14_BC

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
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  #xlim(as_datetime("2023-07-14 06:35:00 CET"),as_datetime("2023-07-14 11:30:00 CET"))+
  ylim(0,8)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )


POST_sampling <- UFP2 / BC2

POST_sampling + plot_annotation(title="POST Rotterdam 14-07-2023",
                                subtitle= "UFP & BC")


POST_sampling <- UFP2 / BC2/ PM252 / NO22

POST_sampling + plot_annotation(title="POST Rotterdam 14-07-2023",
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


# "2023-07-14 06:21:00", "2023-07-14 06:49:00"
# "2023-07-14 06:50:00", "2023-07-14 7:50:00"
# "2023-07-14 08:11:00", "2023-07-14 8:37:00"
# "2023-07-14 08:37:00", "2023-07-14 8:57:00"
# "2023-07-14 08:57:00", "2023-07-14 9:22:00"
# "2023-07-14 09:23:00", "2023-07-14 10:14:00"
# "2023-07-14 10:14:00", "2023-07-14 10:24:00"
# "2023-07-14 10:30:00", "2023-07-14 11:56:00"



intervals <- data.frame(
  start = as.POSIXct(c("2023-07-14 06:21:00", "2023-07-14 06:50:00", "2023-07-14 08:11:00", "2023-07-14 08:37:00", "2023-07-14 08:57:00", "2023-07-14 09:23:00", "2023-07-14 10:14:00", "2023-07-14 10:30:00"), tz = "Europe/Amsterdam"),
  end = as.POSIXct(c("2023-07-14 06:49:00", "2023-07-14 7:50:00", "2023-07-14 8:37:00", "2023-07-14 8:57:00","2023-07-14 9:22:00","2023-07-14 10:14:00", "2023-07-14 10:24:00", "2023-07-14 11:56:00"), tz = "Europe/Amsterdam"),
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
write.csv(combined_table, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/Table_20230714_POST.CSV")

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



#write.csv(cs3, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs//CS_20230814_POST.CSV") 
#write.csv(Partector, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/UFP_20230814_POST.CSV")
#write.csv(ObservAirDST, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/BC_20230814_POST.CSV") 
