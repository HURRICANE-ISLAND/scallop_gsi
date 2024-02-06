
library(dplyr)
library(readr)



##This was Rene helping me get into my data...she is a gem

###LOAD THE DATA AND MAKE THE DATAFRAME OF DATA FROM EACH SITE### 

#Index Site Data
IndexData <- read_csv("CLEAN_eDNA_Index.csv") ## Index DATA
names(IndexData)

#Load HIFarm Data
HIFarmData<- read_csv("HIFarm_Sonde.csv")
names(HIFarmData)

#Load HIDistant Data
HIDistData<- read_csv("HIDistant_Sonde.csv")
names(HIDistData)

#Load NHFarm Data
NHFarmData<- read_csv("NHFarm_Sonde.csv")
names(NHFarmData)

#Load NHDistant Data
NHDistData<- read_csv("NHDistant_Sonde.csv")
names(NHDistData)

#Load STFarm Data
STFarmData<- read_csv("STFarm_Sonde.csv")
names(STFarmData)

#Load STDistant Data
STDistData<- read_csv("STDistant_Sonde.csv")
names(STDistData)



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
HIFarm <- HIFarmData |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #AMH change to calculate means without NAs
  mutate(`Site` = "HI Farm") #add back in site name
HIFarm

#HIDist mean data from net depths 
HIDist <- HIDistData |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "HI Distant") #add back in site name
HIDist


#NH Farm mean data from net depths
NHFarm <- NHFarmData |>  #data youre feeding it
  filter(Depth >= 5 & Depth <= 7) |> #filter to only have net depths at NH
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "NH Farm") #add back in site name
NHFarm

#NHDist mean data from net depths 
NHDist <- NHDistData |>  #data youre feeding it
  filter(Depth >= 5 & Depth <= 7) |> #filter to only have net depths at NHDist
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "NH Distant") #add back in site name
NHDist

#ST Farm mean data from net depths
STFarm <- STFarmData |>  #data youre feeding it
  filter(Depth >= 8 & Depth <= 10) |> #filter to only have net depths at NH
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "ST Farm") #add back in site name
STFarm

#STDist mean data from net depths 
STDist <- STDistData |>  #data youre feeding it
  filter(Depth >= 8 & Depth <= 10) |> #filter to only have net depths at NHDist
  group_by(Date) |> #group by date
  summarise_all(calculate_mean_without_na) %>% #change to calculate means without NAs
  mutate(`Site` = "ST Distant") #add back in site name
STDist

# Combine the dataframes into one large dataframe
combined_mean_data <- bind_rows(HIFarm, HIDist, NHFarm, NHDist, STFarm, STDist)
combined_mean_data #Full list of all means of variables measured at each site at each date
write.csv(combined_data,file = "Sonde_env_Means.csv")

# Print the first few rows of the combined dataframe
head(combined_mean_data)

###EXPORT new dataframe with means to excel###
install.packages("writexl")
library(writexl)
write_xlsx(combined_mean_data, "/Users/phoebejekielek/Documents/GitHub/scallop_gsi/combined_data.xlsx")

##Plot variable mean values by site
ggplot(combined_mean_data, aes(x=Date, y = Temp °C))+
  geom_line()+
  labs(x = "Date", y = "Mean Value", title = "Mean pH Over Time")






##To find the maximum depth at each site to then determine how to select for just the bottom two meters
BenthicDepth <- PheobeData |>  #data youre feeding it
  group_by(Date) |> #group by date
  summarise(max(Depth)) #return max depth of each days visit













August <- PheobeData |> filter(Date == "8/25/2020") ##subsetting by day

NetDepth <- August |> filter(Depth >= 4 & Depth <= 6) ##choosing depth
MeanNetDepth <- NetDepth |> dplyr::select(-Date, -"Site Name") |> summarise_all(mean)

MeanNetDepth <- MeanNetDepth |> mutate(Site = "Index") |> mutate(Date = "8/25/2020") |> relocate(c(Date, Site))




ALL_MeanNetDepth <- MeanNetDepth # only did this once at the beginning

############

##If I wanted to just select out a certain month, this chunk can be copied/pasted and change the date and month
Sept <- PheobeData |> filter(Date == "9/24/2020") ##subsetting by day
NetDepth <- Sept |> filter(Depth >= 4 & Depth <= 6) ##choosing depth
MeanNetDepth <- NetDepth |> dplyr::select(-Date, -"Site Name") |> summarise_all(mean) #remove date/site to avoid warnings, summarize all values
MeanNetDepth <- MeanNetDepth |> mutate(Site = "Index") |> mutate(Date = "9/24/2020") |> relocate(c(Date, Site)) #add back in date and site, move to beginning


ALL_MeanNetDepth <- rbind(ALL_MeanNetDepth, MeanNetDepth) # adding new row to the dataframe with all info


AugSept <- PheobeData |> filter(Date == "9/24/2020" | Date == "8/25/2020" | Date == "10/28/2020") ##subsetting by day









