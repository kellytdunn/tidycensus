library(readxl)
library(tidyr)
library(dplyr)
library(writexl)
library(stringr)
library(tidycensus)

getwd()
setwd('H:/dashboards - working copy/R Code')

current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data = data.frame() #create empty dataframe to dump data into


#for every year in the list, run the census API call and add it to the empty dataframe. 

for(i in years){
  df = get_acs(geography = "tract", table = "B01001", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44') #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 




#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Total:!!")) %>% 
  mutate(label = str_remove(label, "!!"))


#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%  #join based on column "variable" in data and "name" in var_list
  select(-variable) %>% 
  janitor::clean_names() 


data = select(data, -c(moe, geography))

data = data %>% mutate(label = str_replace(label, ":", " "))


#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, Tract = name)


#pivot wider 
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)

data = rename(data, `Total:` = `Total `, `Male total` = `Male `,`Female total`= `Female `)



write_xlsx(x = data, "../Age and Sex.xlsx", col_names = TRUE) #will overwrite any existing file of the same name

#old code for reference - works but years need to be updated manually
#age_20 = get_acs(geography = "tract", table = "B01001", cache_table = TRUE, year = 2020, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44')
#age_19 = get_acs(geography = "tract", table = "B01001", cache_table = TRUE, year = 2019, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44')
#age_20$Year = 2020
#age_19$Year = 2019
#data = rbind(age_20, age_19)


