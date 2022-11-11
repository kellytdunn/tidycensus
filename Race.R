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
  df = get_acs(geography = "tract", table = "S0601", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = key) #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 




#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Estimate")) %>% 
  mutate(label = str_remove(label, "Total!!")) %>% 
  mutate(label = str_remove(label, "Total population!!"))


#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%  #join based on column "variable" in data and "name" in var_list
  select(-variable) %>% 
  janitor::clean_names() #what does this do?


data = select(data, -c(moe))

data = data %>% mutate(label = str_replace_all(label, "!!", ":"))


#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, Tract.name = name)

#simplify label values, which will become columns
data = data %>% 
  mutate(label = str_remove(label, "RACE AND HISPANIC OR LATINO ORIGIN:")) %>% 
  mutate(label = str_remove(label, "One race:")) %>% 
  mutate(label = str_remove(label, "LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH:")) %>% 
  mutate(label = str_remove(label, "Population 5 years and over:")) %>% 
  mutate(label = str_remove(label, "EDUCATIONAL ATTAINMENT:")) %>% 
  mutate(label = str_remove(label, "Population 25 years and over:")) %>% 
  mutate(label = str_remove(label, "SEX:")) %>% 
  mutate(label = str_remove(label, "INDIVIDUALS' INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS):")) %>% 
  mutate(label = str_remove(label, "Population 15 years and over:")) %>% 
  mutate(label = str_remove(label, "POVERTY STATUS IN THE PAST 12 MONTHS:")) %>%
  mutate(label = str_remove(label, "Population for whom poverty status is determined:")) %>% 
  mutate(label = str_remove(label, "Speak language other than English:"))


table(data$label)


#pivot wider 
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)

#reduce to most relevant columns
race = select(data, -contains('Native;'), -contains('MARITAL STATUS:'), -contains('PERCENT ALLOCATED'), -contains('INCOME'))
race = select(race, -c('AGE:Median age (years)', 'Male', 'Female', 'One race'))

#convert percentages to numbers
race$`Under 5 years` =round((race$`Total population`/100)* race$`AGE:Under 5 years`,0)
race$`5 to 17 years` =round((race$`Total population`/100)* race$`AGE:5 to 17 years`,0)
race$`18 to 24 years` =round((race$`Total population`/100)* race$`AGE:18 to 24 years`,0)
race$`25 to 44 years` =round((race$`Total population`/100)* race$`AGE:25 to 44 years`,0)
race$`45 to 54 years` =round((race$`Total population`/100)* race$`AGE:45 to 54 years`,0)
race$`55 to 64 years` =round((race$`Total population`/100)* race$`AGE:55 to 64 years`,0)
race$`65 to 74 years` =round((race$`Total population`/100)* race$`AGE:65 to 74 years`,0)
race$`75 years and over` =round((race$`Total population`/100)* race$`AGE:75 years and over`,0)

#these overwrite the old column, so don't run without first re-generating "race"
race$`White` = round((race$`Total population`/100)* race$`White`,0)
race$`Black or African American` = round((race$`Total population`/100) * race$`Black or African American`,0)
race$`American Indian and Alaska Native`= round((race$`Total population`/100)*race$`American Indian and Alaska Native`,0)
race$Asian = round((race$`Total population`/100)*race$Asian,0)
race$`Native Hawaiian and Other Pacific Islander` = round((race$`Total population`/100)*race$`Native Hawaiian and Other Pacific Islander`,0)
race$`Some other race` = round((race$`Total population`/100)*race$`Some other race`,0)
race$`Two or more races` = round((race$`Total population`/100)*race$`Two or more races`,0)
race$`Hispanic or Latino origin (of any race)` = round((race$`Total population`/100) * race$`Hispanic or Latino origin (of any race)`,0)
race$`White alone, not Hispanic or Latino` = round((race$`Total population`/100) * race$`White alone, not Hispanic or Latino`, 0)

#the numbers don't look right.

#reduce again. see if we can get away with just these ones for now
race = select(race, (c('White', 'Black or African American', 'American Indian and Alaska Native', 'Asian', 'Native Hawaiian and Other Pacific Islander', 'Some other race', 'Two or more races','GEOID', 
                             'Tract.name', 'Year', 'Total population', 'Hispanic or Latino origin (of any race)', 'White alone, not Hispanic or Latino')))

#make sure year carries forward

#pivot down by race
race = pivot_longer(race, cols = c('White', 'Black or African American', 'American Indian and Alaska Native', 'Asian', 'Native Hawaiian and Other Pacific Islander', 'Some other race', 'Two or more races'), names_to = "race_name", values_to = "race_population") #pivot longer



write_xlsx(x = race, "../Race.xlsx", col_names = TRUE) #will overwrite any existing file of the same name



