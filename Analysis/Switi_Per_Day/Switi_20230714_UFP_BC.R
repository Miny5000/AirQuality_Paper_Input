#Script to load datasets, clean & merge - per day 
#MR_20231002
#Readme
#Nb. CS did not work today. 
#Step 1: Load datasets UFP, BC, CS3 + make into dfs + same timestamp (in this case: between 14-07-2023 06:34:29 - 14-07-2023 12:10)
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
# 
# #Step 1 - import and clean data
##UFP 
Partector <- read_delim("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/UFP/2023_07_14/8365_063413.txt", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE, skip = 18) #trims off header lines
 
Partector_start=as_datetime("2023-07-14 06:34:29 CET")
Partector <- Partector %>%
  mutate(Part_time = time + Partector_start)

### Calculate standard deviation
sd_value <- sd(Partector$number)

### Perform a t-test (assuming you want to compare it to a hypothetical mean, e.g., 0)
t_test_result <- t.test(Partector$number, mu = 0)
t_test_result

##BC
ObservAirDST <- read_csv("C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Switi/BC_Julie/Data34_230714_06h19m/Data34_2023-07-14.txt", 
                         col_names = c("TS", "Iref", "Isig", "ATN", "BC", "RH", "Temp", "FR", "Vbat", "GPSlat", "GPSlong"))

ObservAirDST <- ObservAirDST %>%
  mutate(DST_time = as_datetime(TS))

###Clean data 
ObservAirDST <- na.omit(ObservAirDST)
ObservAirDST<- subset(ObservAirDST, BC > 0)

#Step 2 - Analysis per sensor - UFP, BC
## Plots for UFP 
###UFP_a: Whole sampling time 14 July 2023 UFP 
UFP <- ggplot(data=Partector, aes(x=Part_time, y=number, color = number)) +
  geom_line(color = "blue", size = 0.3) +
  theme_minimal()+
  labs(x = "Time", y = "Number",
       title = "Number",
       subtitle = "14 July 2023 - Switi",
       caption = "Data: Partector 2",
       tag = "Fig. #") +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(-1000,180000) +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )
UFP
###UFP_b:whole sampling time period 14 July 2023 UFP - using viridis turbo
ggplot(data = Partector, aes(x = Part_time, y = number, color = number)) +
  geom_line(size = .5) +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "UFP",
    subtitle = "14 July 2023 - Switi",
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

###UFP_c:boxplot all period 14 July 2023 UFP, including mean 
Partector_Filter <- Partector %>% filter(Part_time >= as_datetime("2023-07-14 6:35:00 CET") & Part_time <= as_datetime("2023-07-14 12:10:00 CET"))
mean_working <- mean(Partector$number)
ggplot(data = Partector, aes(x = Part_time, y = number)) +
  geom_boxplot(fill = "darkred", alpha = 0.2) +
  geom_text(x = max(Partector_Filter$Part_time), y = mean_working, label = paste("Mean:", mean_working), vjust = -1, hjust = 1, size = 5, color = "black") +
  theme_minimal() +
  labs(
    x = "Time",
    y = "Number",
    title = "14 July 2023 - Switi Amsterdam",
    subtitle = "Box Plot of Number - Working Crane SK2400-R",
    caption = "Data: Partector",
    tag = "Fig. #"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 16),  # Increased text size to 16
    legend.position = "none"
  )

###UFP_d:Geom_point plot 14 July 2023 UFP
ggplot(Partector, aes(x=Part_time, y=number, colour=diam)) +
  geom_point(size = .5)

###UFP_e: Map UFP
Partector_map <- Partector 
ObservAirDST_map <- ObservAirDST
ObservAirDST_map$Part_time <-ObservAirDST_map$DST_time
##Full join
Par_DST_join <- ObservAirDST_map %>% full_join(Partector_map, by="Part_time")

Par_DST_join <-Par_DST_join %>%
  filter(Part_time >= as_datetime("2023-07-14 06:35:00 CET") & Part_time <= as_datetime("2023-07-14 13:00:00 CET") & GPSlat >= 52.3 & GPSlat <= 52.45 & GPSlong >= 4.75 & GPSlong <= 4.98)

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
    title = "14 July 2023 - Switi",
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
  filter(DST_time >= as_datetime("2023-07-14 06:35:00 CET") & DST_time <= as_datetime("2023-07-14 12:10:00 CET") & GPSlat >= 52.3 & GPSlat <= 52.45 & GPSlong >= 4.75 & GPSlong <= 4.98)

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
  labs(title = "BC map 14 July 2023", color = "BC") 

Swi21_BC


#Step 4: Overview Plots
Switi_sampling <- UFP / BC
Switi_sampling + plot_annotation(title="Switi Amsterdam 14-07-2023",
                                 subtitle= "UFP & BC")

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
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1, size = 8)) +
  theme(legend.position = "none") +
  geom_rug() +
  ylim(0,6)  +
  scale_x_datetime(
    date_breaks = "10 mins",
    date_labels = "%H:%M"
  )


Switi_sampling <- UFP2 / BC2
Switi_sampling + plot_annotation(title="Switi Amsterdam 14-07-2023",
                                 subtitle= "UFP & BC")

#Step 5. Write dfs to csv.
#write.csv(Partector, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/UFP_20230714_Switi.CSV") 
#write.csv(ObservAirDST, "C:/Users/Miny Rajiv/OneDrive - AMS-Institute/Data/Analysis/Cleaned_dfs/BC_20230714_Switi.CSV") 


