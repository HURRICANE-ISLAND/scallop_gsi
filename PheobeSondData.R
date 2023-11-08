
library(dplyr)
library(readr)



##This was Rene helping me get into my data...she is a gem
##make the dataframe of data for the given site
IndexData <- read_csv("CLEAN_eDNA_Index.csv") ## Index DATA
names(IndexData)

####To select out the mean data from net depths for each sampling date at a given site
Index <- IndexData |>  #data youre feeding it
  filter(Depth >= 4 & Depth <= 6) |> #filter to only have certain depths
  group_by(Date) |> #group by date
  summarise_all(mean) |> #summarize mean each column by date
  mutate(`Site` = "Index") #add back in site name

Index

##To find the maximum depth at each site to then determine how to select for just the bottomm two meters
BenthicDepth <- PheobeData |>  #data youre feeding it
  group_by(Date) |> #group by date
  summarise(max(Depth)) #return max depth of each days visit

##To combine data mean from each site into one large dataframe organized by date and site
ALL_SITES <- rbind(Index, Othersites1, Othersites2, etc..)











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









