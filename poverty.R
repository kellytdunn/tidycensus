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
  df = get_acs(geography = "tract", table = "B17026", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44') #call API
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

#pivot wider
data = data %>% pivot_wider(names_from = label, values_from = estimate)

#match exact column names of what's imported to Power BI already, to avoid breaking queries
data = rename(data, `Total pop` = `EstimateTotal:`, Year = year, GEOID = geoid)



#new variables
data$`Number under 200 FPL` = (data$`Under .50`+ data$`.50 to .74` + data$`.75 to .99` + data$`1.00 to 1.24` + data$`1.25 to 1.49` + data$`1.50 to 1.74` + data$`1.75 to 1.84` +
                                 data$`1.85 to 1.99`)
data$`Pct under 200 FPL` = data$`Number under 200 FPL`/data$`Total pop`                                 
                                 

write_xlsx(x = data, "../all tracts - poverty data.xlsx", col_names = TRUE)



