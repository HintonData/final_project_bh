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

# create a new column to hold the year
brisbane_complaints$year <- as.integer(substring(as.character(brisbane_complaints$date_range), 4))

# create a new column to hold the quarter
brisbane_complaints$quarter_str <- substring(as.character(brisbane_complaints$date_range), 1, 2)

# create a lookup table for the quarters
quarters <- c("Q1", "Q2", "Q3", "Q4")

# map the quarters to their numerical value
brisbane_complaints$quarter <- match(brisbane_complaints$quarter_str, quarters)

# create the date column
brisbane_complaints$date <- yq(paste(brisbane_complaints$year, brisbane_complaints$quarter, sep = "-"))

brisbane_complaints <- brisbane_complaints %>% 
  select(-quarter, -quarter_str, -date_range)


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
  mutate(date_received = my(date_received)) %>% 
  mutate(animal_type = str_to_title(animal_type))



# Animal Outcomes
animal_outcomes <- read_csv("data/animal_outcomes.csv") %>% 
  clean_names()

animal_outcomes <- animal_outcomes %>% 
  pivot_longer(act:wa, names_to = "region", values_to = "number_of_occurences") %>% 
  mutate(year = ymd(paste0(year, "-01-01"))) %>% 
  mutate(outcome = ifelse(outcome == "In Stock" & year <= "2008-01-01", "Currently In Care", outcome)) %>% 
  mutate(region = str_to_upper(region))