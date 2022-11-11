#this script will output an excel file ready for import into Power BI

library(readxl)
library(tidyr)
library(dplyr)
library(writexl)
library(stringr)
library(tidycensus)

getwd()
setwd('H:/dashboards - working copy/R Code')

# key = (get your own free API key from https://api.census.gov/data/key_signup.html)
current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data = data.frame() #create empty dataframe to dump data into


#for every year in the list, run the census API call and add it to the empty dataframe. 

for(i in years){
  df = get_acs(geography = "tract", table = "B25044", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = key) #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 



#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!Total:!!")) %>% 
  mutate(label = str_remove(label, "!!"))

#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%
  select(-variable) %>% 
  janitor::clean_names() #what does this do?


data = select(data, -c(geography, moe))

#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, `Total:` = `EstimateTotal:`, Tract = name, `Owner occupied`= `Owner occupied:`, `Renter occupied` = `Renter occupied:`)

#pivot wider
data = data %>% pivot_wider(names_from = label, values_from = estimate)

#new variable for number of vehicles (regardless of tenure)
data$`No vehicle available` = (data$`Renter occupied:No vehicle available` + data$`Owner occupied:No vehicle available`)
data$`1 vehicle available` = (data$`Renter occupied:1 vehicle available` + data$`Owner occupied:1 vehicle available`)
data$`2 + vehicles available` = (data$`Renter occupied:2 vehicles available` + data$`Renter occupied:3 vehicles available` + data$`Renter occupied:4 vehicles available` +
                                   data$`Renter occupied:5 or more vehicles available` + data$`Owner occupied:2 vehicles available` + data$`Owner occupied:3 vehicles available` +
                                   data$`Owner occupied:4 vehicles available` + data$`Owner occupied:5 or more vehicles available`)


head(data)

#keep only relevant columns and assign to new df
veh_tenure = (select(data, c(`Owner occupied`, `Renter occupied`, `No vehicle available`, `1 vehicle available`, `2 + vehicles available`, GEOID, Tract, Year, `Total:`)))

head(veh_tenure)



write_xlsx(x = veh_tenure, "../zero vehicle and tenure2.xlsx", col_names = TRUE)



