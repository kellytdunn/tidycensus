#This script will output a table ready for import into power bi

library(readxl)
library(dplyr)
library(tidyr)
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
  df = get_acs(geography = "tract", table = "C16001", cache_table = TRUE, year = i, state = 53, county = c(33, 61, 53), key = '2eb74e928c52482f6acbc1d75f4c2a7d066f0d44') #call API
  df$Year = i #add year column
  try((data = rbind(data, df)), silent = TRUE) #add to larger dataframe. ignore years where data doesn't yet exist. 
}


rm(df) #remove df, which was just used to construct final dataframe. 


#access list of variables in words not codes, and clean up names.
var_list <- load_variables(2020, "acs5", cache = TRUE) %>% 
  select(-concept) %>% 
  mutate(label = str_remove(label, "Estimate!!Total:!!")) %>% 
  mutate(label = str_remove(label, "!!")) %>% 
  mutate(across('label', str_replace, 'Speak English "very well"', 'bilingual')) %>% 
  mutate(across('label', str_replace, 'Speak English less than "very well"', 'LEP'))
    
#join that list of variables to the original dataframe.
data = data %>% 
  left_join(var_list, by = c("variable" = "name")) %>%
  select(-variable) %>% 
  janitor::clean_names() #what does this do?

data = select(data, -c(geography, moe))

data = rename(data, language = label, tract_name = name)


head(data)

#pivot wider for calculations; will change back later
data = data %>% 
  pivot_wider(names_from = language, values_from = estimate)


#create column for anybody who speaks English well, whether or not they speak another language (ie, non-LEP people)
data$All_English = (data$`Speak only English` + data$`Spanish:bilingual` + data$`French, Haitian, or Cajun:bilingual` + data$`German or other West Germanic languages:bilingual` + 
                                             data$`Russian, Polish, or other Slavic languages:bilingual` + data$`Other Indo-European languages:bilingual` + data$`Korean:bilingual` +
                                             data$`Chinese (incl. Mandarin, Cantonese):bilingual` + data$`Vietnamese:bilingual` + data$`Tagalog (incl. Filipino):bilingual` + 
                                             data$`Other Asian and Pacific Island languages:bilingual` + data$`Arabic:bilingual` + data$`Other and unspecified languages:bilingual`)


head(data)

#keep only relevant columns and assign to new df
language = (select(data, c(`All_English`, `Spanish:LEP`, `French, Haitian, or Cajun:LEP`, `German or other West Germanic languages:LEP`, 
                             `Russian, Polish, or other Slavic languages:LEP`, `Other Indo-European languages:LEP`, `Korean:LEP`,
                             `Chinese (incl. Mandarin, Cantonese):LEP`, `Vietnamese:LEP`, `Tagalog (incl. Filipino):LEP`, 
                             `Other Asian and Pacific Island languages:LEP`, `Arabic:LEP`, `Other and unspecified languages:LEP`, geoid, Year, `EstimateTotal:`, `tract_name`)))

#pivot the table to be longer and skinnier so that each language has its own row.

language = pivot_longer(language, cols = c('All_English', 'Spanish:LEP', 'French, Haitian, or Cajun:LEP', 'German or other West Germanic languages:LEP', 
                                       'Russian, Polish, or other Slavic languages:LEP', 'Other Indo-European languages:LEP', 'Korean:LEP', 'Chinese (incl. Mandarin, Cantonese):LEP', 
                                       'Vietnamese:LEP', 'Tagalog (incl. Filipino):LEP', 'Other Asian and Pacific Island languages:LEP', 'Arabic:LEP', 'Other and unspecified languages:LEP'), names_to = "Language", values_to = "Number") #pivot longer


language = data.frame(language) #convert to df

head(language)

language = rename(language, `Total.population` = `EstimateTotal.`)
language = rename(language, `Tract` = `tract_name`)
language = rename(language, `GEOID` = `geoid`)



#replace values
language <- language %>% 
  mutate(Language = str_replace(Language, "Spanish:LEP", "Spanish"), 
         Language = str_replace(Language, "French, Haitian, or Cajun:LEP", "French, Haitian, or Cajun"),
         Language = str_replace(Language, "German or other West Germanic languages:LEP", "German or other West Germanic"),
         Language = str_replace(Language, "Russian, Polish, or other Slavic languages:LEP", "Russian, Polish, or other Slavic languages"), 
         Language = str_replace(Language, "Other Indo-European languages:LEP", "Other Indo-European languages"),
         Language = str_replace(Language, "Korean:LEP", "Korean"), 
         Language = str_replace(Language, "Chinese incl. Mandarin, Cantonese:LEP", "Chinese"), 
         Language = str_replace(Language, "Vietnamese:LEP", "Vietnamese"), 
         Language = str_replace(Language, "Tagalog incl. Filipino:LEP", "Tagalog/Filipino"),
         Language = str_replace(Language, "Other Asian and Pacific Island languages:LEP", "Other Asian/PI languages"), 
         Language = str_replace(Language, "Arabic:LEP", "Arabic"),
         Language = str_replace(Language, "Other and unspecified languages:LEP", "Other")
         )


table(language$Language)



#export to excel. result is a table of languages spoken by LEP people, and # of people who speak English proficiently (regardless of first language)
getwd()
write_xlsx(x = language, "../language and LEP.xlsx", col_names = TRUE)


