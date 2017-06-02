# This code parses data from the CDC's Multiple Cause of Death datafile for FiveThirtyEight's 
# "Gun Death in America" project.
# This code produces clean dataframes of firearm deaths and suicides (firearm and non-firearm).
# Code to further process this data for our interactive graphic can be found in the 'interactive_prep.R' file
# elsewhere in this repo.

# Questions/comments/corrections to ben.casselman@fivethirtyeight.com

# All data is from the CDC's Multiple Cause of Death datafile.
# Data: http://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm#Mortality_Multiple
# Codebook: http://www.cdc.gov/nchs/data/dvs/Record_Layout_2014.pdf

# Most of these calculations can be checked through CDC's two web tools:
# Wonder search: http://wonder.cdc.gov/controller/datarequest/D76
# WISQARS search: http://webappa.cdc.gov/sasweb/ncipc/mortrate10_us.html (1999-2014)

library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

# The function below will download and parse each year of data.
# Note that older files may require coding tweaks to adapt to older file structures.
# This will save three files:
# 1. all_deaths_YR.RData: Full deaths file, with minimal cleaning
# 2. gun_deaths_YR.RData: Gun deaths only, with some basic additional variables
# 3. suicides_YR.RData: Suicides (firearm and non)

# NOTE THAT EACH FILE IS approx. 1gb

# Function for downloading and parsing data:
CDC_parser <- function(year, url) {
  
  # Set up files
  all_deaths_name <- paste0("deaths_", substr(year, 3, 4))
  all_deaths_save <- paste0("all_deaths_", substr(year, 3, 4), ".RData")
  gun_name <- paste0("guns_", substr(year, 3, 4))
  gun_save <- paste0("gun_deaths_", substr(year, 3, 4), ".RData")
  suicide_name <- paste0("suicide_", substr(year, 3, 4))
  suicide_save <- paste0("suicide_", substr(year, 3, 4), ".RData")
  
  # First download data. These are fixed-width files.
  # Layout for recent years (need tweaks for earlier year)
  layout <- fwf_widths(c(19,1,40,2,1,1,2,2,1,4,1,2,2,2,2,1,1,1,16,4,1,1,1,1,34,1,1,4,3,1,3,3,2,1,281,1,2,1,1,1,1,33,3,1,1),
                       col_names = c("drop1", "res_status", "drop2", "education_89", "education_03", "education_flag", "month", 
                                     "drop3", "sex", "detail_age", "age_flag", "age_recode", "age_recode2", "age_group", 
                                     "age_infant", "death_place", "marital", "day_of_week", "drop4", "data_year", "at_work", 
                                     "death_manner", "burial", "autopsy", "drop5", "activity", "injury_place", 
                                     "underlying_cause", "cause_recode358", "drop6", "cause_recode113", "cause_recode130", 
                                     "cause_recode39", "drop7", "multiple_causes", "drop8", "race", "race_bridged", "race_flag", 
                                     "race_recode", "race_recode2", "drop9", "hispanic", "drop10", "hispanic_recode"))
  
  temp <- tempfile()
  download.file(url, temp, quiet = T)
  
  # Read in data
  raw_file <- read_fwf(unzip(temp), layout)
  
  # Drop empty fields
  raw_file <- raw_file %>%
    select(-contains("drop"))

  # Save 'all_deaths' file
  assign(eval(all_deaths_name), raw_file)
  save(list = all_deaths_name, file = all_deaths_save)
  
  # Subset suicides
  # Suicide codes: X60 - X 84, U03, Y870
  
  suicide_code <- list()
  for (i in 1:24) {
    suicide_code[[i]] <- paste0("X", i + 59)
  }
  suicide_code[length(suicide_code)+1] <- "U03"
  suicide_code[length(suicide_code)+1] <- "Y870"
  
  # Gun suicides
  # X72 (Intentional self-harm by handgun discharge)
  # X73 (Intentional self-harm by rifle, shotgun and larger firearm discharge)
  # X74 (Intentional self-harm by other and unspecified firearm discharge)
  
  suicide <- raw_file %>%
    filter(underlying_cause %in% suicide_code) %>%
    mutate(gun = ifelse(underlying_cause %in% c("X72", "X73", "X74"), 1, 0),
           year = year)  
  
  assign(eval(suicide_name), suicide)
  save(list = suicide_name, file = suicide_save)
  rm(suicide)
  rm(list = suicide_name)
  
  # Subset firearm deaths
  
  # Firearm death codes
  # Accidental:
  # W32 (Handgun discharge)
  # W33 (Rifle, shotgun and larger firearm discharge)
  # W34 (Discharge from other and unspecified firearms)
  # 
  # Suicide:
  # X72 (Intentional self-harm by handgun discharge)
  # X73 (Intentional self-harm by rifle, shotgun and larger firearm discharge)
  # X74 (Intentional self-harm by other and unspecified firearm discharge)
  # 
  # Homicide:
  # U01.4 (Terrorism involving firearms)
  # X93 (Assault by handgun discharge)
  # X94 (Assault by rifle, shotgun and larger firearm discharge)
  # X95 (Assault by other and unspecified firearm discharge)
  # 
  # Undetermined intent:
  # Y22 (Handgun discharge, undetermined intent)
  # Y23 (Rifle, shotgun and larger firearm discharge, undetermined intent)
  # Y24 (Other and unspecified firearm discharge, undetermined intent)
  # 
  # Legal intervention (Note that we code legal intervention deaths as homicides)
  # Y35.0 (Legal intervention involving firearm discharge)
  
  guns <- raw_file %>%
    filter(underlying_cause %in% c("W32", "W33", "W34", "X72", "X73", "X74", "U014", "X93", "X94", "X95", "Y22", "Y23", "Y24", "Y350"))
  
  rm(raw_file)
  
  # Add categorical variable for intent, weapon, plus dummy for police shootings
  guns <- guns %>%
    mutate(intent = ifelse(underlying_cause %in% c("W32", "W33", "W34"), "Accidental",
                           ifelse(underlying_cause %in% c("X72", "X73", "X74"), "Suicide",
                                  ifelse(underlying_cause %in% c("*U01.4", "X93", "X94", "X95", "Y350"), "Homicide",
                                         ifelse(underlying_cause %in% c("Y22", "Y23", "Y24"), "Undetermined", NA)))),
           police = ifelse(underlying_cause == "Y350", 1, 0),
           weapon = ifelse(underlying_cause %in% c("W32", "X72", "X93", "Y22"), "Handgun",
                           ifelse(underlying_cause %in% c("W33", "X73", "X94", "Y23"), "Rifle etc",
                                  "Other/unknown")),
           year = year) # Dummy for young men (15-34)
  
  # Create a cleaner age variable. Every age under 1 year will be coded as "0"
  guns <- guns %>%
    mutate(age = ifelse(substr(detail_age, 1, 1) == "1", as.numeric(substr(detail_age, 2, 4)), # Year
                        ifelse(detail_age == 9999, NA, 0)),
           age = ifelse(age == 999, NA, age))
  
  assign(eval(gun_name), guns)
  save(list = gun_name, file = gun_save)
  rm(guns)
  rm(list = gun_name)
  
}

# Enter year and url (urls are inconsistent, so easier to enter them directly)
year <- 2013
url <- "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DVS/mortality/mort2013us.zip"

# Now run the function for each year you want:
CDC_parser(year, url)


#########################################################################################################################

# The code below processes the data for FiveThirtyEight's Gun Deaths in America project

# For the project, we used the three most recent years available: 2012-14
# We'll combine these into a single data frame.
# In keeping with CDC practice, we'll eliminate deaths of non-U.S. residents

load("gun_deaths_14.RData")
load("gun_deaths_13.RData")
load("gun_deaths_12.RData")

all_guns <- rbind(guns_12, guns_13, guns_14)
all_guns <- all_guns %>%
  filter(res_status != 4)

# Create new categorical variables for place of injury, educational status, and race/ethnicity.
# For race/ethnicity, we used five non-overlapping categories: 
# Hispanic, non-Hispanic white, non-Hispanic black, non-Hispanic Asian/Pacific Islander, non-Hispanic Native American/Native Alaskan

all_guns <- all_guns %>%
  mutate(place = factor(injury_place, labels = c("Home", "Residential institution", "School/instiution", "Sports", "Street", 
                                                 "Trade/service area", "Industrial/construction", "Farm", "Other specified", 
                                                 "Other unspecified")),
         education = ifelse(education_flag == 1, 
                            cut(as.numeric(education_03), breaks = c(0, 2, 3, 5, 8, 9)),
                            cut(as.numeric(education_89), breaks = c(0, 11, 12, 15, 17, 99))),
         education = factor(education, labels = c("Less than HS", "HS/GED", "Some college", "BA+", NA)),
         race = ifelse(hispanic > 199 & hispanic <996, "Hispanic",
                       ifelse(race == "01", "White",
                              ifelse(race == "02", "Black",
                                     ifelse(as.numeric(race) >= 4 & as.numeric(race) <= 78, "Asian/Pacific Islander","Native American/Native Alaskan")))),
         race = ifelse(is.na(race), "Unknown", race)) %>%
  select(year, month, intent, police, sex, age, race, hispanic, place, education)

# This is the main data frame FiveThirtyEight used in its analysis.
# For example:
# Gun suicides by year:
all_guns %>%
  filter(intent == "Suicide") %>%
  group_by(year) %>%
  summarize(suicides = length(year))

# Gun homicides of young men (15-34) by year:
all_guns %>%
  filter(intent == "Homicide", age >= 15, age < 35, sex == "M") %>%
  group_by(year) %>%
  summarize(homicides = length(year))

save(all_guns, file = "all_guns.RData")
write.csv(all_guns, file = "full_data.csv")
