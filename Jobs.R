library(tidyr)
library(dplyr)
library(writexl)
library(lehdr)
library(tidycensus)
library(sf)
library(tidyverse)
library(tigris)

setwd("H:/dashboards - working copy")

current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data = data.frame() #create empty dataframe to dump data into


#for every year in the list, run the census API call and add it to the empty dataframe. 

for(i in years){
  df = grab_lodes(state = "wa",
    year = i, lodes_type = "wac", agg_geo = "tract", use_cache = TRUE)  
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}



rm(df) #remove df, which was just used to construct final dataframe. 



#by wage
wages = pivot_longer(data, cols = c(CE01, CE02, CE03), names_to = "wage_category", values_to = "number_jobs") #pivot longer

wages = subset(wages, select = c(w_tract, wage_category, number_jobs, Year)) #just take the columns we need


write_xlsx(x = wages, "Jobs by wages.xlsx", col_names = TRUE)

#by education

educ = pivot_longer(data, cols = c(CD01, CD02, CD03, CD04), names_to = "educ_category", values_to = "number_jobs") #pivot longer


educ = subset(educ, select = c(w_tract, educ_category, number_jobs, Year)) #just take the columns we need


write_xlsx(x = educ, "Jobs by educ.xlsx", col_names = TRUE)
