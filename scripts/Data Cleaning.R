# Load libraries 
library(tidyverse)
library(readxl)

# Load data
data <- read_excel("data/FIOBPD_API222.xlsx")

# Select Columns
data <- data %>%
  select(contact_date, sex, race, age, ethnicity, deceased, was_frisked, contact_officer, zip, circumstance, basis, starts_with("vehicle_"), 
         key_situations, weather, contact_reason)


# Cutoff date for RMS consistency after Sept 29 2019: ######################################
data <- data %>% 
  filter(contact_date >= "2019-09-29")

# Manipulate vehicle column
data <- data %>%
  select(!vehicle_make)

data <- data %>%
  mutate(across(starts_with("vehicle_"), ~na_if(., "NULL")))

data <- data %>%
  mutate(involved = if_else(rowSums(!is.na(select(data, starts_with("vehicle_")))) > 0, TRUE, FALSE))

data <- data %>%
  select(-starts_with("vehicle_"))

data <- data %>%
  rename("vehicle_involved" = involved)

# Outcome variable manipulation




# About 8:51PM on Monday, 05/30/22, Offic
# how do we want to deal with NULLs and NAs and imputation? eg., was_frisked of NULL or NA -- do we assume this is a 0 or 1, or drop these?