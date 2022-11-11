
setwd("H:/dashboards - working copy/R Code") #set this to the folder where you want this document and its output to live.
library(tidycensus)
library(dplyr)
library(stringr)
library(janitor)
library(writexl)
library(tidyr)

current_year = format(Sys.Date(), "%Y") #define current year
years = seq.int(2019, current_year, by = 1) #list all years from 2019 to present
data2 = data.frame() #create empty dataframe to dump data into
# key = (get your own free API key from https://api.census.gov/data/key_signup.html)

#for every year in the sequence, run the census API call and add it to the empty dataframe. Ignores years where data does not yet exist. You can ignore "API endpoint not found" errors

for(i in years){
  df = get_acs(geography = "tract", table = "S2801", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = key)
  df$Year = i
  try((data2 = rbind(data2, df)), silent = TRUE)
}


rm(df)

data = data2 #so I don't have to re-run API call every time I edit the code

#loads var list for every var in the census and then cleans it up
var_list <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  select(-concept) %>%
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Total!!")) %>% 
  mutate(label = str_remove(label, "Total households!!"))
           



#left join var names to df.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%
  select(-variable) %>% 
  janitor::clean_names() 

data = select(data, -c(moe))
rm(var_list)


data = data %>% mutate(label = str_replace_all(label, "!!", ":"))


#pivot wider for summaries
data = data %>% 
  pivot_wider(names_from = label, values_from = estimate)

#rename most useful columns

data = rename(data, `No computer` = `TYPES OF COMPUTER:No computer`, `Has computer or smartphone`= `TYPES OF COMPUTER:Has one or more types of computing devices:`,
`Only computer is a smartphone` = `TYPES OF COMPUTER:Has one or more types of computing devices::Smartphone:Smartphone with no other type of computing device`,
`Has internet or data plan` = `TYPE OF INTERNET SUBSCRIPTIONS:With an Internet subscription:`, `No internet` = `TYPE OF INTERNET SUBSCRIPTIONS:Without an Internet subscription`,
`Only internet is via a smartphone` = `TYPE OF INTERNET SUBSCRIPTIONS:With an Internet subscription::Broadband of any type:Cellular data plan:Cellular data plan with no other type of Internet subscription`)

#reduce to columns I want
data = select(data, c('geoid', 'name', 'year', 'Total households', 'No computer', 'Has computer or smartphone', 'Only computer is a smartphone', 'Has internet or data plan', 'No internet', 'Only internet is via a smartphone'))



#match exact column names of what's imported to Power BI already, for consistency
data = rename(data, Year = year, GEOID = geoid, Tract = name)

write_xlsx(data, "../Internet.xlsx") #export to excel. File will be one level above the folder named above.

