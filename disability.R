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

# key = (get your own free API key from https://api.census.gov/data/key_signup.html)

#for every year in the list, run the census API call and add it to the empty dataframe. 

for(i in years){
  df = get_acs(geography = "tract", table = "S1810", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = key) #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 


#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Total!!")) %>% 
  mutate(label = str_remove(label, "Total civilian noninstitutionalized population!!")) %>% 
  mutate(label = str_remove(label, "DISABILITY TYPE BY DETAILED AGE!!")) %>% 
  mutate(label = str_remove(label, "RACE AND HISPANIC OR LATINO ORIGIN!!"))


#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%  #join based on column "variable" in data and "name" in var_list
  select(-variable) %>% 
  janitor::clean_names() 


data = select(data, -c(moe))

data = data %>% mutate(label = str_replace_all(label, "!!", ":"))


#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, Tract = name)



#pivot wider 
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)


data = select(data, c(`Total civilian noninstitutionalized population`, `With a disability:With a cognitive difficulty`, `With a disability:With a hearing difficulty`,
                      `With a disability:With a vision difficulty`, `With a disability:With an ambulatory difficulty`, `With a disability:With a self-care difficulty`,
                      `With a disability:With an independent living difficulty`, `With a disability:Total civilian noninstitutionalized population`, GEOID, Tract, Year))

data$`Total with a disability` = data$`With a disability:Total civilian noninstitutionalized population`

colnames(data)<-gsub("With a disability:","",colnames(data))


write_xlsx(x = data, "../disability.xlsx", col_names = TRUE) #will overwrite any existing file of the same name



