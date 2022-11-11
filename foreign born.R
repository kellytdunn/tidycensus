setwd("H:/dashboards - working copy/R Code") #set this to the folder where you want this document and its output to live.
library(tidycensus)
library(dplyr)
library(stringr)
library(janitor)
library(writexl)

current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data = data.frame() #create empty dataframe to dump data into

# key = (get your own free API key from https://api.census.gov/data/key_signup.html)

#for every year in the sequence, run the census API call and add it to the empty dataframe. Ignores years where data does not yet exist. You can ignore "API endpoint not found" errors

for(i in years){
  df = get_acs(geography = "tract", table = "B05006", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = key)
  df$Year = i
  try((data = rbind(data, df)), silent = TRUE)
}


rm(df)


#loads var list for every var in the census and then cleans it up
var_list <- load_variables(2020, "acs5", cache = TRUE) %>% 
  select(-concept) %>%
  mutate(label = str_remove(label, "Estimate")) %>% 
  mutate(label = str_remove(label, "!!Total:!!Europe:!!")) %>%   
  mutate(label = str_remove(label, "!!Total:!!Americas:!!")) %>%
  mutate(label = str_remove(label, "!!Total:!!Asia:!!")) %>%
  mutate(label = str_remove(label, "!!Total:!!Africa:!!")) %>%
  mutate(label = str_remove(label, "!!Total:!!Oceania:!!")) %>% 
  mutate(label = str_remove(label, "!!Total:!!Oceania:!!")) %>% 
  mutate(label = str_remove(label, "South Central Asia:!!")) %>% 
  mutate(label = str_remove(label, "South Eastern Asia:!!")) %>% 
  mutate(label = str_remove(label, "Australia and New Zealand Subregion:!!")) %>% 
  mutate(label = str_remove(label, "Eastern Africa:!!")) %>% 
  mutate(label = str_remove(label, "Eastern Asia:!!")) %>% 
  mutate(label = str_remove(label, "Eastern Europe:!!")) %>% 
  mutate(label = str_remove(label, "Latin America:!!")) %>% 
  mutate(label = str_remove(label, "Caribbean:!!")) %>% 
  mutate(label = str_remove(label, "Central America:!!")) %>% 
  mutate(label = str_remove(label, "South America:!!")) %>% 
  mutate(label = str_remove(label, "Middle Africa:!!")) %>% 
  mutate(label = str_remove(label, "Northern Africa:!!")) %>% 
  mutate(label = str_remove(label, "Northern Europe:!!")) %>% 
  mutate(label = str_remove(label, "Southern Europe:!!")) %>% 
  mutate(label = str_remove(label, "Southern Africa:!!")) %>% 
  mutate(label = str_remove(label, "Western Africa:!!")) %>% 
  mutate(label = str_remove(label, "Western Asia:!!")) %>% 
  mutate(label = str_remove(label, "Western Europe:!!")) %>% 
  mutate(label = str_remove(label, "Northern America:!!")) %>% 
  mutate(label = str_remove(label, "China:!!")) %>% 
  mutate(label = str_remove(label, fixed("United Kingdom (inc. Crown Dependencies):!!"))) %>% 
  mutate(label = str_remove(label, "!!"))




#one df of language regions
#one df of countries?

#continue cleaning
#India, Mexico, China, Vietnam, Philippines, Korea, Ukraine, Ethiopia, Japan, Russia, Taiwan, Germany, Cambodia
#kenya, hong kong, iran, somalia, thailand
#Spanish-speaking Latin America, 

#left join var names to df.
foreign_born = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%
  select(-variable) %>% 
  janitor::clean_names() 

foreign_born = select(foreign_born, -c(geography, moe))

foreign_born = rename(foreign_born, `country` = `label`, tract_name = name)

#remove summary rows that are not useful here
remove <- c('Europe', 'Asia', 'Africa', 'Oceania', 'America', 'Crown Dependencies', 'China:', 'Caribbean')

#remove rows that contain any string in the vector in the team column
foreign_born = foreign_born[!grepl(paste(remove, collapse='|'), foreign_born$country),]


foreign_born = foreign_born %>% 
  mutate(country = str_replace(country, "excluding Hong Kong and Taiwan", "mainland")) %>% 
  mutate(country = str_replace(country, "PortugalAzores Islands", "Portugal - Azores")) %>% 
  mutate(country = str_replace(country, "United Kingdom, excluding England and Scotland", "Other United Kingdom or Dependencies"))


write_xlsx(foreign_born, "../Foreign Born.xlsx") #export to excel. File will be one level above the folder named above.

