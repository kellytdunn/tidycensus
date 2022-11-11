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
  df = get_acs(geography = "tract", table = "S1901", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44') #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 


#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5/subject", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!")) %>% 
  mutate(label = str_remove(label, "Estimate"))
  

table(var_list$label)

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


#reduce to most relevant columns
income = (select(data, c(`Households:Total`, `Tract`, `GEOID`, `Year`, `Households:Median income (dollars)`, `Households:Total:Less than $10,000`, `Households:Total:$10,000 to $14,999`,
                       `Households:Total:$15,000 to $24,999`, `Households:Total:$25,000 to $34,999`, `Households:Total:$35,000 to $49,999`, `Households:Total:$50,000 to $74,999`, 
                       `Households:Total:$75,000 to $99,999`, `Households:Total:$100,000 to $149,999`, `Households:Total:$150,000 to $199,999`, `Households:Total:$200,000 or more`)))

#data is in percentages. change to values.
income$`Less than $10,000` = round((income$`Households:Total`/100) * income$`Households:Total:Less than $10,000`,0)
income$`$10,000 to $14,999` = round((income$`Households:Total`/100) * income$`Households:Total:$10,000 to $14,999`,0)
income$`$15,000 to $24,999` = round((income$`Households:Total`/100) * income$`Households:Total:$15,000 to $24,999`,0)
income$`$25,000 to $34,999` = round((income$`Households:Total`/100) * income$`Households:Total:$25,000 to $34,999`,0)
income$`$35,000 to $49,999` = round((income$`Households:Total`/100) * income$`Households:Total:$35,000 to $49,999`,0)
income$`$50,000 to $74,999` = round((income$`Households:Total`/100) * income$`Households:Total:$50,000 to $74,999`,0)
income$`$75,000 to $99,999` = round((income$`Households:Total`/100) * income$`Households:Total:$75,000 to $99,999`,0)
income$`$100,000 to $149,999` = round((income$`Households:Total`/100) * income$`Households:Total:$100,000 to $149,999`,0)
income$`$150,000 to $199,999` = round((income$`Households:Total`/100) * income$`Households:Total:$150,000 to $199,999`,0)
income$`$200,000 or more` = round((income$`Households:Total`/100) * income$`Households:Total:$200,000 or more`,0)


#reduce again
income = (select(income, -c(`Households:Total:Less than $10,000`, `Households:Total:$10,000 to $14,999`,
                         `Households:Total:$15,000 to $24,999`, `Households:Total:$25,000 to $34,999`, `Households:Total:$35,000 to $49,999`, `Households:Total:$50,000 to $74,999`, 
                         `Households:Total:$75,000 to $99,999`, `Households:Total:$100,000 to $149,999`, `Households:Total:$150,000 to $199,999`, `Households:Total:$200,000 or more`)))

#pivot longer
income = pivot_longer(income, cols = c('Less than $10,000', '$10,000 to $14,999', '$15,000 to $24,999', 
                                     '$25,000 to $34,999', '$35,000 to $49,999', '$50,000 to $74,999','$75,000 to $99,999', 
                                     '$100,000 to $149,999', '$150,000 to $199,999', '$200,000 or more'), names_to = "Income Category", values_to = "IncomeCat_HH") #pivot longer

income = income %>% rename(TotHH_tract = `Households:Total`, `Median income` = `Households:Median income (dollars)`)


write_xlsx(x = income, "../Income.xlsx", col_names = TRUE)



