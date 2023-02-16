# Cleaning Script for RSPCA Data
library(tidyverse)
library(janitor)
library(lubridate)
# Brisbane
brisbane_complaints <- read_csv("data/brisbane_complaints.csv") %>% 
  clean_names()


brisbane_complaints <- brisbane_complaints %>% 
  select(-nature, -responsible_office, -city) %>% 
  mutate(date_range = str_remove_all(date_range, ".csv")) %>% 
  mutate(date_range = str_replace_all(date_range, "1st-quarter-2016-17", "Q1 2016")) %>% 
  mutate(date_range = str_replace_all(date_range, "april-june-2016", "Q2 2016")) %>%  
  mutate(date_range = str_replace_all(date_range, "october-to-december-2016", "Q4 2016")) %>% 
  mutate(date_range = str_replace_all(date_range, "january-to-march-2017", "Q1 2017")) %>% 
  mutate(date_range = str_replace_all(date_range, "april-to-june-2017", "Q2 2017")) %>% 
  mutate(date_range = str_replace_all(date_range, "july-to-september-2017", "Q3 2017")) %>% 
  mutate(date_range = str_replace_all(date_range, "october-to-december-2017", "Q4 2017")) %>% 
  mutate(date_range = str_replace_all(date_range, "jan-to-mar-2018", "Q1 2018")) %>% 
  mutate(date_range = str_replace_all(date_range, "apr-to-jun-2018", "Q2 2018")) %>% 
  mutate(date_range = str_replace_all(date_range, "jul-to-sep-2018", "Q3 2018")) %>% 
  mutate(date_range = str_replace_all(date_range, "oct-to-dec-2018", "Q4 2018")) %>% 
  mutate(date_range = str_replace_all(date_range, "jan-mar-2019", "Q1 2019")) %>% 
  mutate(date_range = str_replace_all(date_range, "apr-jun-2019", "Q2 2019")) %>% 
  mutate(date_range = str_replace_all(date_range, "jul-to-sep-2019", "Q3 2019")) %>% 
  mutate(date_range = str_replace_all(date_range, "cars-srsa-open-data-animal-related-complaints-oct-to-dec-2019", "Q4 2019")) %>%
  mutate(date_range = str_replace_all(date_range, "cars-srsa-open-data-animal-related-complaints-jan-to-mar-2020", "Q1 2020")) %>% 
  mutate(date_range = str_replace_all(date_range, "cars-srsa-open-data-animal-related-complaints-apr-to-jun-2020", "Q2 2020")) %>% 
  mutate(date_range = factor(date_range, levels = c("Q1 2016", "Q2 2016", "Q3 2016", "Q4 2016", "Q1 2017", "Q2 2017", "Q3 2017", "Q4 2017", "Q1 2018", "Q2 2018", "Q3 2018", "Q4 2018", "Q1 2019", "Q2 2019", "Q3 2019", "Q4 2019", "Q1 2020", "Q2 2020"))) %>% 
  mutate(suburb = str_to_title(suburb))


brisbane_complaints <- brisbane_complaints %>%
  unite('type', animal_type:category, sep = " - ") %>% 
  mutate(type = str_remove_all(type, " - NA")) %>% 
  mutate(type = str_remove_all(type, " - Dog")) %>% 
  mutate(type = str_replace_all(type, "Other Animal - Not An Attack", "Other Animal")) %>%
  mutate(type = str_replace_all(type, "Dog - Nuisance Animal", "Dog")) %>%
  mutate(type = str_replace_all(type, "Cat - Unregistered", "Cat")) %>%
  mutate(type = str_replace_all(type, "Cat - Odour", "Cat")) %>%
  mutate(type = str_replace_all(type, "Attack - Rabbit", "Attack")) %>%
  mutate(type = str_replace_all(type, "Attack - Fencing Issues", "Attack")) %>%
  mutate(type = str_replace_all(type, "Dog - Surrender", "Dog")) %>%
  mutate(type = str_replace_all(type, "Attack - Rabbit", "Attack")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Feral Goat", "Pest / Feral Animal")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Feral Cat", "Pest / Feral Animal")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Feral Pig", "Pest / Feral Animal")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Pest / Feral Animal", "Pest / Feral Animal")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Deer", "Deer")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Fox", "Fox")) %>%
  mutate(type = str_replace_all(type, "Attack - Dangerous", "Unspecified - Dangerous Attack")) %>%
  # First the columns will be seperated again then "Attack" will be replaced
  mutate(type = str_replace_all(type, "Attack - Attack On An Animal", "Unspecified - Attack On An Animal")) %>% 
  mutate(type = str_replace_all(type, "Cat Trapping", "Cat - Trapping")) %>% 
  mutate(type = str_replace_all(type, "Attack - Attack On A Person", "Unspecified - Attack On A Person")) %>% 
  mutate(type = str_replace_all(type, "Attack - Not An Attack", "Unspecified - Not An Attack")) %>% 
  mutate(type = str_replace_all(type, "Attack - Menacing", "Unspecified - Menacing")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Wild Dog", "Wild Dog")) %>%
  mutate(type = str_replace_all(type, "Other Animal - Rabbit", "Rabbit")) %>%
  # mutate(type = str_replace_all(type, "Attack", "Unspecified - Attack")) %>%
  separate(col = type, into = c("type_of_animal", "complaint"), sep = " - ") %>% 
  mutate(complaint = if_else(type_of_animal == "Attack", "Attack", complaint)) %>% 
  mutate(type_of_animal = str_replace_all(type_of_animal, "Attack", "Unspecified")) %>%
  mutate(complaint = if_else(is.na(complaint), "Other", complaint))


# Animal Complaints
animal_complaints <- read_csv("data/animal_complaints.csv") %>%
  clean_names()

animal_complaints <- animal_complaints %>% 
  mutate(date_received = my(date_received))



# Animal Outcomes
animal_outcomes <- read_csv("data/animal_outcomes.csv") %>% 
  clean_names()

animal_outcomes <- animal_outcomes %>% 
  pivot_longer(act:wa, names_to = "region", values_to = "number_of_occurences") %>% 
  mutate(year = factor(year, levels = c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")))
