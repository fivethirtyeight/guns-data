# This code parses data from the FBI's Supplementary Homicide Reports for FiveThirtyEight's 
# "Gun Death in America" project.
# Data via the National Archive of Criminal Justice Data: https://www.icpsr.umich.edu/icpsrweb/content/NACJD/guides/ucr.html

# Questions/comments/corrections to ben.casselman@fivethirtyeight.com
# Thanks to Carl Bialik for his assistance wtih coding, debugging and analysis.

require(dplyr)
require(readr)

# Download data as zipped tsv files here: https://www.icpsr.umich.edu/icpsrweb/content/NACJD/guides/ucr.html
# Codebooks are included with downloads.
# The code below should work without modifications for the years 2006 to 2014. File layouts vary somewhat in earlier years,
# and code will need to be adjusted accordingly.
# This code will output a clean dataframe. Un-comment line near end of function to also save the data.

# Save tsv files to working directory as 'SHR_YEAR.tsv'

year <- 2010 # user entry

# Un-comment and run the line of code below AFTER generating the function below
# SHR_2010 <- SHR_parser(year) # or whatever you want to call your data frame

SHR_parser <- function(year) {
  
  # Columns don't read in right if you don't pre-assign column types.
  coltypes <- paste0("cicniiiiiicciiicicc", paste0(rep("c", times = 133), collapse = ""))
  
  filename <- paste0("SHR_", year, ".tsv") # Change this if you're using a different naming convention
  
  # Read in file as a tsv
  raw_file <- read_tsv(eval(filename), col_types = coltypes)
  
  # Assign names for JUST the incident section. The victim/offender sections will follow.
  names(raw_file)[1:18] <- c("id_code", "state_code", "ORI_code", "group", "geo_division", "year", "pop", "county", 
                             "MSA", "MSA_indic", "agency_name", "state_name", "offense_month", "last_update", "action_type",
                             "offense_type", "incident_number", "situation")
  
  # Assign unique id to each incident so we can join them later.
  raw_file$unique_id <- 1:nrow(raw_file)
  
  # From here, we read each victim and offender individually. We preserve the
  # unique ids so we can join them up again when we want.
  
  # First split off the first victim
  working <- raw_file %>%
    select(1:18, 19:22, unique_id) %>%
    mutate(vic_off = "vic",
           vic_off_num = 1)
  names(working)[19:22] <- c("age", "sex", "race", "ethnicity")
  working$age[working$age == "00" | working$age == "0"] <- NA # We're coding children less than one year old as age "0"
  working$age[working$age == "NB" | working$age == "BB"] <- 0
  working$age <- as.numeric(working$age)
    
  # Same for the first offender
  working2 <- raw_file %>%
    select(1:18, 23:30, unique_id) %>%
    mutate(vic_off = "off",
           vic_off_num = 1)
  names(working2)[19:26] <- c("age", "sex", "race", "ethnicity", "weapon", "relat", "circ", "sub_circ")
  working2$age[working2$age == "00" | working2$age == "0"] <- NA
  working2$age[working2$age == "NB" | working2$age == "BB"] <- 0
  working2$age <- as.numeric(working2$age)
  
  # Now join those together. Note that using bind_rows, we can bind the data frames despite unequal number of columns.
  working <- bind_rows(working, working2)
  
  # Now we do essentially the same thing, but in bulk using a for loop.
  # Each loop takes the incident information (plus unique id) but takes a different
  # victim/offender location (victim 2, victim 3, etc).
  
  # Victim records
  for (i in 1:10){
    start <- 33 + (i-1)*4
    end <- start + 3
    working2 <- raw_file %>%
      select(1:18, start:end, unique_id) %>%
      mutate(vic_off = "vic",
             vic_off_num = i+1)
    names(working2)[19:22] <- c("age", "sex", "race", "ethnicity")
    working2$age[working2$age == "00" | working2$age == "0"] <- NA
    working2$age[working2$age == "NB" | working2$age == "BB"] <- 0
    working2$age <- as.numeric(working2$age)
    working2 <- working2 %>%
      filter(!(is.na(sex) & is.na(age) & is.na(race) & is.na(ethnicity)))
    working <- bind_rows(working, working2)
  }
  
  # Offender records
  for (i in 1:10){
    start <- 73 + (i-1)*8
    end <- start + 7
    working2 <- raw_file %>%
      select(1:18, start:end, unique_id) %>%
      mutate(vic_off = "off",
             vic_off_num = i+1)
    names(working2)[19:26] <- c("age", "sex", "race", "ethnicity", "weapon", "relat", "circ", "sub_circ")
    working2$age[working2$age == "00" | working2$age == "0"] <- NA
    working2$age[working2$age == "NB" | working2$age == "BB"] <- 0
    working2$age <- as.numeric(working2$age)
    working2 <- working2 %>%
      filter(!(is.na(sex) & is.na(age) & is.na(race) & is.na(ethnicity) & is.na(weapon) & is.na(relat) & is.na(circ) & is.na(sub_circ)))
    working <- bind_rows(working, working2)
  }
  
  # Next we identify offenders who used a gun. (Note that no weapon is coded for victims.)
  # Firearm codes:
  # 11 - firearm type not stated
  # 12 - handgun
  # 13 - rifle
  # 14 - shotgun
  # 15 - other gun
  working <- working %>%
    mutate(gun = ifelse(weapon %in% c(11, 12, 13, 14, 15), 1, 0)) # Identify if a gun was used
    
  # Next we identify gun incidents. These are *incidents* in which a gun was used.
  # Note that we're coding these at the incident level -- this variable doesn't tell us which
  # offender USED the gun (the 'gun' variable does that). We just want to know whether an incident was a "gun incident."
  # This will apply to both victims and offenders. (As a result, victims will be coded as "gun victims" whether or not
  # they were personally shot.)
  working <- working %>%
    filter(vic_off == "off") %>%
    mutate(gun_used = ifelse(weapon %in% c(11, 12, 13, 14, 15), 1, 0)) %>% # Identify if a gun was used
    group_by(unique_id) %>% 
    select(unique_id, gun_used) %>%
    summarize(gun_used = sum(gun_used)) %>%
    mutate(gun_used = ifelse(gun_used > 0, 1, 0)) %>% # Reduce this to a binary -- we only care about one gun per incident
    left_join(working, ., by = "unique_id") # join back to main record
  
  # Same basic approach for "justifiable homicide." This will produce three dummy variables:
  # One for justifiable homicides by law enforcement; one for justifiable homicides by civilians; and one for
  # total justifiable homicide (regardless of type). Note that these are coded at the incident level -- we don't
  # specify WHICH offender was justified (although we could do so with minor coding adjustments). It is not possible,
  # from the public SHR data, to identify which victims in multi-victim incidents died through justifiable homicide.
  working <- working %>%
    filter(vic_off == "off", !is.na(circ)) %>%
    mutate(police = ifelse(circ == 80, 1, 0),
           other_just = ifelse(circ ==  81, 1, 0),
           just = ifelse(police == 1 | other_just == 1, 1, 0)) %>%
    group_by(unique_id) %>%
    summarize(police = sum(police), other_just = sum(other_just), just = sum(just)) %>%
    mutate(police = ifelse(police > 0, 1, 0),
           other_just = ifelse(other_just > 0, 1, 0),
           just = ifelse(just > 0, 1, 0)) %>%
    select(unique_id, just, police, other_just) %>%
    left_join(working, ., by = "unique_id")
  
  # save.file(working, file = paste0("SHR_", year, ".RData")) # Un-comment this line to save file
  
  working # output the final product
  
}

