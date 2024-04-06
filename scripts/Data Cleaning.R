# Load libraries 
library(tidyverse)
library(readxl)
library(gridExtra)
library(sf)
library(tidycensus)



# Load data ####################
data <- read_excel("data/FIOBPD_API222.xlsx")



# Select Columns ####################
data <- data %>%
  select(contact_date, sex, race, age, ethnicity,
         # deceased, #removing because it's almost entirely 0s seen below
         was_frisked, contact_officer, zip, circumstance, basis, starts_with("vehicle_"), 
         key_situations, weather, contact_reason)




# Cutoff date for RMS consistency after Sept 29 2019 ####################
data <- data %>% 
  filter(contact_date >= "2019-09-29")



# Manipulate vehicle column ####################
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

# how do we want to deal with NULLs and NAs and imputation? eg., was_frisked of NULL or NA -- do we assume this is a 0 or 1, or drop these?



# Outcome variable manipulation ####################



# Contact date into date and time ####################
data <- data %>%
  mutate(date = substr(contact_date, 1, 10),
         time = substr(contact_date, 12, 19)) %>% 
  mutate(time = ifelse(time == "", NA, time)) %>%
  select(date, time, everything())



# Other cleaning ####################
data <- data %>% 
  mutate(race = ifelse(race == "NULL", NA, race),
         age = ifelse(age == "NULL", NA, age),
         sex = ifelse(age == "NULL", NA, sex),
         was_frisked = ifelse(was_frisked == "NULL", NA, was_frisked),
         basis = ifelse(basis == "NULL", NA, basis),
         circumstance = ifelse(circumstance == "NULL", NA, circumstance))



# Exploratory Analysis ####################
## FIOs and Frisk Rates by Race ======
race_count_frisk_rate <- data %>% 
  group_by(race) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
            )

## Age counts ======
# Filter out rows where age is missing
data_age_filtered <- data[!is.na(data$age), ]

# Convert age to numeric (assuming it's stored as character)
data_age_filtered$age <- as.numeric(data_age_filtered$age)

# Count occurrences for each age
age_counts <- data_age_filtered %>%
  count(age)

# Plot
ages <- ggplot(age_counts, aes(x = age, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "FIO Counts by Age", x = "Age", y = "Count") +
  scale_x_continuous(breaks = seq(0, max(data_age_filtered$age), by = 10), minor_breaks = seq(0, max(data_age_filtered$age), by = 5)) +
  scale_y_continuous(breaks = seq(0, max(age_counts$n), by = 200), minor_breaks = seq(0, max(age_counts$n), by = 100)) +
  theme_minimal()

ages

# Save plot as an image
ggsave("images/fio_counts_by_age.png", plot = ages, width = 8, height = 6, dpi = 300, bg = "white")



## Vehicles ======
vehicles <- data %>% 
  group_by(vehicle_involved) %>% 
  summarise(count = n())



## Sex ======
sex <- data %>% 
  group_by(sex) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )



## Zip Codes ======
#create boston zip codes dataset using data from 2015-2019 5-year ACS
acs_vars <- c(total_pop = "B01003_001E")

#create boston zip codes dataset using data from 2015-2019 5-year ACS
boston_zips <- get_acs(geography = "zcta",
                       variables = acs_vars,
                       state = "MA",
                       output = "wide",
                       geometry = TRUE,
                       year = 2019) %>% 
  st_transform("WGS84")

#Check it works 
ggplot(data = boston_zips) +
  geom_sf() 

fio_zips <- merge(data, boston_zips, by.x = "zip", by.y = "GEOID", all.x = TRUE)


# Create summary table of FIO incidents by zip code
fio_summary <- fio_zips %>%
  group_by(zip) %>%
  summarize(total_fios = n(),
            geometry = geometry)  # Count the number of FIO incidents per zip code

# Plot the map shading by the number of FIO incidents per zip code
ggplot(data = fio_summary, aes(fill = total_fios)) +
  geom_sf(data = fio_zips) +
  scale_fill_gradient(name = "FIO Count", low = "lightblue", high = "darkblue") +
  labs(title = "FIO Incidents by Zip Code")



## Date and time ======



# ## Deceased ======
# deceased <- data %>% 
#   group_by(deceased) %>% 
#   summarise(count = n()) # this is almost entirely 0s so I think we should remove


## Circumstance & Basis ======
basis <- data %>% 
  group_by(basis) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )

circumstance <- data %>% 
  group_by(circumstance) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )

circumstance_basis <- data %>% 
  group_by(circumstance, basis) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )

race_basis <- data %>% 
  group_by(basis, race) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )

race_circumstance <- data %>% 
  group_by(circumstance, race) %>% 
  summarise(count = n(),
            frisk_rate = round(mean(as.numeric(was_frisked), na.rm = T), 3)
  )
