
library(dplyr)
library(readr)

###LOAD THE DATA AND MAKE THE DATAFRAME OF DATA FROM EACH SITE### 

#Load Index Site Data
IndexData <- read_csv("CLEAN_eDNA_Index.csv") ## Index DATA
names(IndexData)

#Load HIFarm Data
HIFarmData<- read_csv("HIFarm_Sonde.csv")
HIFarm<- HIFarmData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>% #rename some columns
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"Altitude m",-"pH mV",-"Time",-"Vertical Position m")#+#adding code to remove some columns of data
  #mutate(`Site` = "HI Farm") #add back in site name
  
names(HIFarm)

#Load HIDistant Data
HIDistData<- read_csv("HIDistant_Sonde.csv")
##HIDistData is only Hurricane FAR, HIDist has both FAR and DISTANT...why??
HIDist<- HIDistData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>%
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"Altitude m",-"pH mV",-"Time",-"Vertical Position m")#adding code to remove some columns of data
names(HIDist)

#Load NHFarm Data
NHFarmData<- read_csv("NHFarm_Sonde.csv")
NHFarm<- NHFarmData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>%
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"pH mV",-"Altitude m",-"Time (HH:mm:ss)",-"Vertical Position m")#adding code to remove some columns of data
names(NHFarm)


#Load NHDistant Data
NHDistData<- read_csv("NHDistant_Sonde.csv")
NHDist<- NHDistData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>%
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"pH mV",-"Altitude m",-"Time (HH:mm:ss)",-"Vertical Position m")#adding code to remove some columns of data
names(NHDist)

#Load STFarm Data
STFarmData<- read_csv("STFarm_Sonde.csv")
STFarm<- STFarmData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>%
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"pH mV",-"Altitude m",-"Time (HH:mm:ss)",-"Vertical Position m")#adding code to remove some columns of data
names(STFarm)

#Load STDistant Data
STDistData<- read_csv("STDistant_Sonde.csv")
STDist<- STDistData %>% #use package dplyr to rename
  rename("Salinity"="Sal psu") %>% 
  rename("Chl5"="Chlorophyll RFU...5")%>%
  rename("Chl6"="Chlorophyll RFU...6")%>% 
  select(-"Wiper Position volt",-"Barometer mmHg",-"GPS Longitude °",-"GPS Latitude °",
         -"Cable Pwr V",-"Battery V",-"pH mV",-"Altitude m",-"Time (HH:mm:ss)",-"Vertical Position m")#adding code to remove some columns of data
names(STDist)

#Dataframe of all data from all sites
AllAQData <- bind_rows(HIFarm, HIDist, NHFarm, NHDist, STFarm, STDist)
AllAQData 

# Dot plot with colors
All<-ggplot(data=AllAQData, aes(x = Site, y = Salinity, color = Site)) +
  geom_point(alpha=0.2) +
  theme_classic()+
  stat_summary(aes(x=Site,y=Salinity),fun=mean,geom = "point",color="red",size=4)+
  labs(x = "Time", y = "Mean Salinity at Depth", title = "Mean Salinity at Sites") +
  scale_color_discrete(name = "Site")+# Customize legend title
  ylim(28,35)
All

####SELECT OUT THE MEAN DATA FROM NET DEPTHS AT EACH SITE FOR A GIVEN SAMPLING DATE###

## Define a function to calculate the mean while excluding NAs
calculate_mean_without_na <- function(x) {
  mean(x, na.rm = TRUE)}

Index <- IndexData |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #AMH change to calculate means without NAs
  mutate(`Site` = "Index") #add back in site name
Index

#HIFarm mean data from net depths 
HIFarmNet <- HIFarm |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #AMH change to calculate means without NAs
  mutate(`Site` = "HI Farm") #add back in site name
HIFarmNet

#HIDist mean data from net depths 
HIDistNet <- HIDist |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "HI Distant") #add back in site name
HIDistNet


#NH Farm mean data from net depths
NHFarmNet <- NHFarm |>  #data youre feeding it
  filter(Depth >= 5 & Depth <= 7) |> #filter to only have net depths at NH
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "NH Farm") #add back in site name
NHFarmNet

#NHDist mean data from net depths 
NHDistNet <- NHDist |>  #data youre feeding it
  filter(Depth >= 5 & Depth <= 7) |> #filter to only have net depths at NHDist
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "NH Distant") #add back in site name
NHDistNet

#ST Farm mean data from net depths
STFarmNet <- STFarm |>  #data youre feeding it
  filter(Depth >= 8 & Depth <= 10) |> #filter to only have net depths at NH
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "ST Farm") #add back in site name
STFarmNet

#STDist mean data from net depths 
STDistNet <- STDist |>  #data youre feeding it
  filter(Depth >= 8 & Depth <= 10) |> #filter to only have net depths at NHDist
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "ST Distant") #add back in site name
STDistNet

# Combine the dataframes into one large dataframe of net-level means
NetLevelMeans <- bind_rows(HIFarmNet, HIDistNet, NHFarmNet, NHDistNet, STFarmNet, STDistNet)
NetLevelMeans #Full list of all means of variables measured at each site at each date

# Print the first few rows of the combined dataframe
head(NetLevelMeans)

###EXPORT new dataframe with net-level means to excel###
install.packages("writexl")
library(writexl)
write_xlsx(NetLevelMeans, "/Users/phoebejekielek/Documents/GitHub/scallop_gsi/Sonde_Net_Means.csv")

###MAKE SOME FIGURES WITH NET-LEVEL DATA###
boxplot(Depth~Site,data=NetLevelMeans,
        main = "Average Depth at Each Site", 
        xlab = "Site", 
        ylab = "Average Depth (m)",
        col = c("lightblue", "lightgreen", "lightyellow"), # Set colors for the boxes
        border = "black",  # Set color for the border lines
        notch = FALSE#,      # Add notches to the boxplot
        #notchwidth = 0.5# Set width of the notches
)

boxplot(pH~Site,data=NetLevelMeans,
        main = "Average pH at Each Site", 
        xlab = "Site", 
        ylab = "Average pH",
        #col = c("lightblue", "lightgreen", "lightyellow"), # Set colors for the boxes
        border = "black",  # Set color for the border lines
        notch = FALSE#,      # Add notches to the boxplot
        #notchwidth = 0.5# Set width of the notches
)


boxplot(ODO mg/L~Site,data=combined_mean_data,
        main = "Average Dissolved Oxygen at Each Site", 
        xlab = "Site", 
        ylab = "Average pH",
        #col = c("lightblue", "lightgreen", "lightyellow"), # Set colors for the boxes
        border = "black",  # Set color for the border lines
        notch = FALSE#,      # Add notches to the boxplot
        #notchwidth = 0.5# Set width of the notches
)










####SELECT OUT THE BOTTOM TWO METERS OF DATA AT EACH SITE FOR A GIVEN SAMPLING DATE####
library(readr)
library(dplyr)

HIFarmBot<-HIFarm %>%
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "HI Farm")%>% #add back in site name
  ungroup()
head(HIFarmBot)

HIDistBot<-HIDist %>%
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "HI Distant")%>% #add back in site name
  ungroup()
head(HIDistBot)

STDistBot<- STDist |>  #data youre feeding it
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "ST Distant")%>% #add back in site name
  ungroup()
head(STDistBot)

STFarmBot<- STFarm |>  #data youre feeding it
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "ST Farm")%>% #add back in site name
  ungroup()
head(STFarmBot)

NHDistBot<- NHDist |>  #data youre feeding it
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "NH Distant")%>% #add back in site name
  ungroup()
head(NHDistBot)

NHFarmBot<- NHFarm |>  #data youre feeding it
  group_by(Date) %>% #grouping by date so that it will then calculate the 2m for each date
  filter(Depth>= (max(Depth)-2)) %>% #find the max depth at each site and subtract 2 to get the data for bottom 2m at each date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means of all variables without NAs
  mutate(`Site` = "NH Farm")%>% #add back in site name
  ungroup()
head(NHFarmBot)

# Combine the dataframes into one large dataframe of bottom 2m means
BotMeansAQ <- bind_rows(HIFarmBot, HIDistBot, NHFarmBot, NHDistBot, STFarmBot, STDistBot)
head(BotMeansAQ) #Full list of all means of variables measured at each site at each date


###EXPORT new dataframe with bottom means to excel###
install.packages("writexl")
library(writexl)
write_xlsx(BotMeansAQ, "/Users/phoebejekielek/Documents/GitHub/scallop_gsi/Sonde_Bottom_Means_AQ.csv")



library(ggplot2)
##Plot variable mean values by site
ggplot(BotMeansAQ, aes(x=Date, y = "Temp °C"))+
  geom_line()+
  labs(x = "Date", y = "Mean Value", title = "Mean pH Over Time")

###MAKE SOME FIGURES WITH NET-LEVEL DATA###
boxplot(Depth~Site,data=BotMeansAQ,
        main = "Average Depth at Each Site", 
        xlab = "Site", 
        ylab = "Average Depth (m)",
        col = c("lightblue", "lightgreen", "lightyellow"), # Set colors for the boxes
        border = "black",  # Set color for the border lines
        notch = FALSE#,      # Add notches to the boxplot
        #notchwidth = 0.5# Set width of the notches
)
names(BotMeansAQ)

boxplot(pH~Site,data=BotMeansAQ,
        main = "Average pH at Each Site", 
        xlab = "Site", 
        ylab = "Average pH",
        #col = c("lightblue", "lightgreen", "lightyellow"), # Set colors for the boxes
        border = "black",  # Set color for the border lines
        notch = FALSE#,      # Add notches to the boxplot
        #notchwidth = 0.5# Set width of the notches
)



# Dot plot with colors
ppl<-ggplot(data=BotMeansAQ, aes(x = Site, y = Salinity, color = Site)) +
  geom_point(alpha=0.2) +
  theme_classic()+
  labs(x = "Time", y = "Mean Salinity at Depth", title = "Time Series of Mean Salinity from Different Sites") +
  scale_color_discrete(name = "Site")+# Customize legend title
  ylim(30,35)
ppl



















