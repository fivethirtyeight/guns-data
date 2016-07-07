# This code processes data from the CDC's Multiple Cause of Death datafile to prepare it for FiveThirtyEight's 
# "Gun Death in America" project.
# This code is designed to work in conjunction with 'CDC_parser.R' elsewhere in this repo. To match FiveThirtyEight's
# published data, use years 2012-2014.

# Questions/comments/corrections to ben.casselman@fivethirtyeight.com
# Interactive designed and built by Reuben Fischer-Baum and Matthew Conlen.

# All deaths data is from the CDC's Multiple Cause of Death datafile.
# Data: http://www.cdc.gov/nchs/data_access/VitalStatsOnline.htm#Mortality_Multiple
# Codebook: http://www.cdc.gov/nchs/data/dvs/Record_Layout_2014.pdf
# Population data from the American Community Survey via IPUMS: https://usa.ipums.org/usa/cite.shtml

library(readr)
library(dplyr)

load("all_guns.RData") # Assumes you have already run CDC_parser code.

# The interactive allows readers to filter by intent, sex, age and race.
# The interactive presents gun deaths in a "typical" year -- the average number of deaths in each category over three years.

# Add variable for age group:
guns_for_interactive <- all_guns %>%
  mutate(age_group = cut(age, breaks = c(-1,14,34,64,107), labels = c("0-14", "15-34", "35-64", "65+")),
         age_group = ifelse(is.na(age), "Unknown", age_group),
         age_group = factor(age_group, labels = c("0-14", "15-34", "35-64", "65+", "Unknown")))
select(year, intent, sex, age_group, race)


# Now we will set up another data frame with the actual encoding.

# The notion here is that each possible permutation has its own code, with an assigned code.
# There are four attributes: Intent, Sex, Age, Race
# The codes are as follows:
# Intent (position 1):
# A Not selected
# B Suicide
# C Homicide
# D Accident
# E Unknown

# Sex (position 2):
# A Not selected
# B Female
# C Male

# Age (position 3):
# A Not selected
# B Under 15
# C 15-34
# D 35-64
# E 65+
# F Unknown

# Race (position 4):
# A Not selected
# B Non-Hispanic white
# C Non-Hispanic black
# D Hispanic
# E Non-Hispanic Asian
# F Non-Hispanic other

# So 'DBDB' would be a 35-64-year-old white male victim of an accident

# There are 540 possible combinations. We'll list all of them:
codes <- list()

v1 <- c("A", "B", "C", "D", "E")      # intent
v2 <- c("A", "B", "C")  # sex
v3 <- c("A", "B", "C", "D", "E", "F")  # age
v4 <- c("A", "B", "C", "D", "E", "F")  # race

for (i1 in 1:length(v1)){
  for (i2 in 1:length(v2)){
    for (i3 in 1:length(v3)){
      for (i4 in 1:length(v4)){
        codes[[length(codes)+1]] <- paste0(v1[i1],v2[i2],v3[i3],v4[i4])
      }
    }
  }
}

# The "encoding" data frame will match the codes to the totals.
# For ease of checking, we'll also translate the codes.
encoding <- data.frame(code = I(codes), Intent = NA, Gender = NA, Age = NA, Race = NA, Deaths = NA)

# Create functions to turn the codes back into English.
converter.intent <- function(code) {
  A <- "None selected"
  B <- "Suicide"
  C <- "Homicide"
  D <- "Accident"
  E <- "Unknown"
  var <- substr(code, 1, 1)
  eval(as.name(var))
}

converter.sex <- function(code) {
  A <- "None selected"
  B <- "Female"
  C <- "Male"
  var <- substr(code, 2, 2)
  eval(as.name(var))
}

converter.age <- function(code) {
  A <- "None selected"
  B <- "Under 15"
  C <- "15 - 34"
  D <- "35 - 64"
  E <- "65+"
  var <- substr(code, 3, 3)
  eval(as.name(var))
}

converter.race <- function(code) {
  A <- "None selected"
  B <- "White"
  C <- "Black"
  D <- "Hispanic"
  E <- "Asian/Pacific Islander"
  F <- "Native American"
  var <- substr(code, 4, 4)
  eval(as.name(var))
}

encoding$Intent <- mapply(function(x) converter.intent(x), encoding_check$Code)
encoding$Gender <- mapply(function(x) converter.sex(x), encoding_check$Code)
encoding$Age <- mapply(function(x) converter.age(x), encoding_check$Code)
encoding$Race <- mapply(function(x) converter.race(x), encoding_check$Code)

# Now we'll calculate the actual numbers.

# Recode guns_for_interactive data frame with all numeric codes -- makes for easier matching
working <- guns_for_interactive %>%
  mutate(intent = as.numeric(factor(intent)),
         sex = as.numeric(factor(sex)),
         age_group = as.numeric(factor(age_group)),
         race = as.numeric(factor(race, levels = c("White", "Black", "Hispanic", "Asian/Pacific Islander", "Other"))))

# We'll match the letters in the codes to the numbers in the data:
A <- c(1, 2, 3, 4, 5)
B <- 1
C <- 2
D <- 3
E <- 4
F <- 5

# This function calculates the NUMBER of deaths in a category, given the character string.
calculator <- function(code){
  a <-  working %>%
    filter(intent %in% eval(as.name(substr(code, 1, 1))),
           sex %in% eval(as.name(substr(code, 2, 2))),
           age_group %in% eval(as.name(substr(code, 3, 3))),
           race %in% eval(as.name(substr(code, 4, 4)))) %>%
    nrow(.)
  round(a/3,0)
}

# Now run the calculation
encoding$Deaths <- mapply(function(x) calculator(x), encoding$code)


# In order to calculate death rates per 100,000 people, we'll need populations from the American Community Survey.
# This data comes from IPUMS: https://usa.ipums.org/usa/cite.shtml
# We'll use the same three years (2012-14). We need the following variables:
# SEX, AGE, RACE and HISPAN, plus the weighting variable PERWT

# Download the data from IMPUS directly. Then proceed.

ACS <- read_csv("guns_ipums.gz") # Or change the file name as needed.

# Need to match categories to CDC data.
ACS <- ACS %>%
  select(PERWT, SEX, AGE, RACE, HISPAN) %>%
  mutate(sex = ifelse(SEX == 1, 2, 1),
         age_group = as.numeric(cut(AGE, breaks = c(-1, 14, 34, 64, 100))),
         race = ifelse(HISPAN > 0 & HISPAN < 9, 3,
                       ifelse(RACE == 1, 1,
                              ifelse(RACE == 2, 2,
                                     ifelse(RACE %in% c(4,5,6), 4,5)))))

# This function calculates the NUMBER of people in a category, given the character string.
calculator.pop <- function(code){
  a <-  ACS %>% select(sex, age_group, race, PERWT) %>%
    filter(sex %in% eval(as.name(substr(code, 2, 2))),
           age_group %in% eval(as.name(substr(code, 3, 3))),
           race %in% eval(as.name(substr(code, 4, 4)))) %>%
    summarize(sum(PERWT)) %>%
    as.numeric(.)
  round(a/3,0)
}

# Now run the calculation
encoding$Population <- NA
encoding$Population <- mapply(function(x) calculator.pop(x), encoding$code)

# Calculate rate
encoding <- encoding %>%
  mutate(Rate = round((Deaths/Population)*100000, 1))

write.csv(encoding, file = "interactive_data.csv")
