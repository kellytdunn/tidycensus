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
  df = get_acs(geography = "tract", table = "S1501", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44') #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 



#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "EstimateTotal!!")) %>% 
  mutate(label = str_remove(label, "!!")) %>% 
  mutate(label = str_remove(label, "EstimateTotal:"))



#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%  #join based on column "variable" in data and "name" in var_list
  select(-variable) %>% 
  janitor::clean_names() #what does this do?


data = select(data, -c(moe))

data = data %>% mutate(label = str_replace_all(label, "!!", ":"))

#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, Year = year, GEOID = geoid, Tract = name)

#pivot wider for calculations; will change back later
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)

#rename columns
data = rename(data, `Less than 9th grade` = `EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:Less than 9th grade`)
data = rename(data, `9th - 12th grade`= `EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:9th to 12th grade, no diploma`)
data = rename(data, `High school graduate or GED` = `EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:High school graduate (includes equivalency)`)
data = rename(data, `Some_college` = `EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:Some college, no degree`)
data = rename(data, `AA degree` =`EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:Associate's degree`)
data = rename(data, `Bachelors degree` =`EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:Bachelor's degree`)
data = rename(data, `Graduate or professional degree` =`EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over:Graduate or professional degree`)
data = rename(data, `Population 25 years and over` = `EstimateTotal:AGE BY EDUCATIONAL ATTAINMENT:Population 25 years and over`)

#summarize some columns
data$`Less than high school` = data$`Less than 9th grade` + data$`9th - 12th grade`
data$`Some college` = data$Some_college + data$`AA degree`



#reduce to most relevant columns
data = (select(data, c(`Population 25 years and over`, `Tract`, `GEOID`, `Year`, `Less than high school`, `High school graduate or GED`, `Some college`, `Bachelors degree`, `Graduate or professional degree` )))



#pivot back
data = pivot_longer(data, cols = c('Less than high school', 'High school graduate or GED', 'Some college', 'Bachelors degree', 'Graduate or professional degree'), names_to = "education_level", values_to = "EduCat_pop") #pivot longer



write_xlsx(x = data, "../Education.xlsx", col_names = TRUE)



