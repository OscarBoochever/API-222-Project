# Load libraries 
library(tidyverse)
library(readxl)
library(gridExtra)
library(sf)
library(tidycensus)
library(ggspatial)
library(prettymapr)
library(lubridate)



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

# DROP ALL NULLS/NAs
#race NA as Not Reported
#impute age and another column to impute age
# key situations binary 1 or 0 if there is one listed
# drop zip code NAs
# zip code label map colored by frisk rate instead of total FIOs


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



# Outcome variable manipulation ####################
data <- data %>% 
  filter(!is.na(was_frisked)) %>% 
  select(!contact_reason)

write_csv(data, "FIO_clean_removed_frisked_NAs.csv")

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
# Create boston zip codes dataset using data from 2015-2019 5-year ACS
acs_vars <- c(total_pop = "B01003_001E")

# Create boston zip codes dataset using data from 2015-2019 5-year ACS
boston_zips <- get_acs(geography = "zcta",
                       variables = acs_vars,
                       state = "MA",
                       output = "wide",
                       geometry = TRUE,
                       year = 2019) %>% 
  st_transform("WGS84")

# Merge data
fio_zips <- merge(data, boston_zips, by.x = "zip", by.y = "GEOID", all.x = TRUE)

# Create summary table of FIO incidents by zip code
fio_summary <- fio_zips %>%
  group_by(geometry, zip) %>%
  summarize(total_fios = n(),
            frisk_rate = sum(as.numeric(was_frisked))/total_fios) %>% 
  st_as_sf() # Count the number of FIO incidents per zip code

# Plot the map shading by the number of FIO incidents per zip code
mass_fio_map <- ggplot() +
  geom_sf(data = boston_zips, color = "grey", fill = NA) +  # Plot all zip code boundaries
  geom_sf(data = fio_summary, aes(fill = total_fios), color = "black") +  # Overlay FIO incidents
  scale_fill_gradient(name = "FIO Count", low = "lightblue", high = "darkblue") +
  labs(title = "FIO Incidents by Zip Code", fill = "FIO Count") +
  theme_minimal()

# Filter the fio_summary dataset for zip codes with at least 2 FIO incidents
filtered_fio_summary <- fio_summary %>%
  filter(total_fios >= 4)

# Plain map, blue zips
fio_zip_map <- ggplot() +
  geom_sf(data = filtered_fio_summary, aes(fill = total_fios), color = "black") +  # Overlay FIO incidents
  # geom_sf(data = boston_zips, color = "grey", fill = NA) +  # Plot filtered zip code boundaries for Boston area
  scale_fill_gradient(name = "FIO Count", low = "lightblue", high = "darkblue") +
  labs(title = "FIO Incidents by Zip Code (Boston Area)", fill = "FIO Count") +
  theme_minimal()

# Map with background
fio_zip_map_background <- ggplot(data = filtered_fio_summary) +
  annotation_map_tile(zoomin = 0,
                      type = "cartolight") +
  geom_sf(data = filtered_fio_summary, aes(fill = total_fios, alpha = 0.9), color = "black") +  # Overlay FIO incidents with transparency
  scale_fill_gradient(name = "FIO Count", low = "lightyellow", high = "darkred") +  # Change color gradient
  labs(title = "FIO Incidents by Zip Code (Boston Area)", fill = "FIO Count") +
  guides(alpha = "none") +
  theme_minimal()

# Get the top 5-10 zip codes with the highest FIO counts
top_zip <- filtered_fio_summary %>%
  arrange(desc(total_fios)) %>%
  slice(1:10)

# Get the centroids of the top 5-10 zip codes
top_zip_centroids <- st_centroid(top_zip)

# Extract x and y coordinates from centroids
top_zip_centroids <- cbind(top_zip_centroids, st_coordinates(top_zip_centroids))

# Labeled Map
fio_zip_label_map <- ggplot(data = filtered_fio_summary) +
  annotation_map_tile(zoomin = 0,
                      type = "cartolight") +
  geom_sf(data = filtered_fio_summary, aes(fill = total_fios), color = "black", alpha = 0.5) +  # Overlay FIO incidents with transparency
  scale_fill_gradient(name = "FIO Count", low = "lightyellow", high = "darkred") +  # Change color gradient
  labs(title = "FIO Incidents by Zip Code (Boston Area)", fill = "FIO Count") +
  geom_text(data = top_zip_centroids, aes(label = zip, x = X, y = Y), size = 1, color = "white", check_overlap = TRUE) +  # Add labels for top zip codes
  guides(alpha = "none") +  # Remove the alpha legend
  theme_minimal()

# Labeled Map Colored by Frisk Rate
frisk_zip_label_map <- ggplot(data = filtered_fio_summary) +
  annotation_map_tile(zoomin = 0,
                      type = "cartolight") +
  geom_sf(data = filtered_fio_summary, aes(fill = frisk_rate), color = "black", alpha = 0.8) +  # Overlay FIO incidents with transparency
  scale_fill_gradient(name = "Frisk Rate", low = "white", high = "darkred") +  # Change color gradient
  labs(title = "Frisk Rates by Zip Code (Boston Area)", fill = "FIO Count") +
  geom_text(data = top_zip_centroids, aes(label = zip, x = X, y = Y), size = 1, color = "white", check_overlap = TRUE) +  # Add labels for top zip codes
  guides(alpha = "none") +  # Remove the alpha legend
  theme_minimal()

# Define the breaks and colors for the gradient
breaks <- c(0, 0.57, 1)
colors <- c("white", "purple", "black")

# Create the plot with the custom gradient scale
frisk_zip_label_map <- ggplot(data = filtered_fio_summary) +
  annotation_map_tile(zoomin = 0,
                      type = "cartolight") +
  geom_sf(data = filtered_fio_summary, aes(fill = frisk_rate), color = "black", alpha = 1) +
  scale_fill_gradientn(name = "Frisk Rate", colors = colors, values = scales::rescale(breaks), breaks = breaks) +
  labs(title = "Frisk Rates by Zip Code (Boston Area)", fill = "FIO Count") +
  geom_text(data = top_zip_centroids, aes(label = zip, x = X, y = Y), size = 1, color = "white", check_overlap = TRUE) +
  guides(alpha = "none") +
  theme_minimal()

# Define the breaks and colors for the gradient
breaks <- c(0, 0.16, 0.58, 1)
colors <- c("white", "lightcyan", "navy", "black")

# Create the plot with the custom gradient scale
frisk_zip_label_map <- ggplot(data = filtered_fio_summary) +
  annotation_map_tile(zoomin = 0,
                      type = "cartolight") +
  geom_sf(data = filtered_fio_summary, aes(fill = frisk_rate), color = "black", alpha = 0.8) +
  scale_fill_gradientn(name = "Frisk Rate", colors = colors, values = scales::rescale(breaks), breaks = breaks) +
  labs(title = "Frisk Rates by Zip Code (Boston Area)", fill = "FIO Count") +
  geom_text(data = top_zip_centroids, aes(label = zip, x = X, y = Y), size = 1, color = "white", check_overlap = TRUE) +
  guides(alpha = "none") +
  theme_minimal()

frisk_zip_label_map
# COLOR BY PROPORTION FRISKED

# Exporting mass_fio_map
ggsave(filename = "images/mass_fio_map.png", plot = mass_fio_map, width = 8, height = 6, dpi = 300, bg = "white")
# Exporting fio_zip_map
ggsave(filename = "images/fio_zip_map.png", plot = fio_zip_map, width = 8, height = 6, dpi = 300, bg = "white")
# Exporting fio_zip_map_background
ggsave(filename = "images/fio_zip_map_background.png", plot = fio_zip_map_background, width = 8, height = 6, dpi = 300, bg = "white")
# Exporting fio_zip_label_map
ggsave(filename = "images/fio_zip_label_map.png", plot = fio_zip_label_map, width = 8, height = 6, dpi = 300, bg = "white")
ggsave(filename = "images/frisk_rate_zip_label_map.png", plot = frisk_zip_label_map, width = 8, height = 6, dpi = 300, bg = "white")







## Date and time ======
# Aggregate data by day
data_daily <- data %>%
  group_by(date) %>%
  summarize(total_incidents = n())
# 
# # Plot of FIOs by day as a column chart
# ggplot(data_daily, aes(x = date, y = total_incidents)) +
#   geom_col() +
#   labs(title = "FIO Incidents by Day (Column Chart)",
#        x = "Date",
#        y = "Number of FIO Incidents") +
#   theme_minimal()

# Plot of FIOs by day as a line chart
fio_day_line <- ggplot(data_daily, aes(x = date, y = total_incidents)) +
  geom_line() +
  labs(title = "FIO Incidents by Day (Line Chart)",
       x = "Date",
       y = "Number of FIO Incidents") +
  theme_minimal()


# Convert date column to Date format
data$date <- as.Date(data$date)

# Round down each date to the nearest week start
data <- mutate(data, week_start = floor_date(date, unit = "week"))

# Aggregate data by week start
data_weekly <- data %>%
  group_by(week_start) %>%
  summarize(total_incidents = n())

# Plot of FIOs by week
fio_week_line <- ggplot(data_weekly, aes(x = week_start, y = total_incidents)) +
  geom_line() +
  labs(title = "FIO Incidents by Week",
       x = "Week",
       y = "Number of FIO Incidents") +
  theme_minimal()

# Round down each date to the nearest month start
data <- mutate(data, month_start = floor_date(date, unit = "month"))

# Aggregate data by month start
data_monthly <- data %>%
  group_by(month_start) %>%
  summarize(total_incidents = n())

# Filter out the first month's data
data_monthly_filtered <- data_monthly %>%
  filter(month_start > min(month_start))

# Plot of FIOs by month with y-axis starting from 0 and quarterly minor ticks on x-axis
fio_month_col <- ggplot(data_monthly_filtered, aes(x = month_start, y = total_incidents)) +
  geom_col() +
  labs(title = "FIO Incidents by Month",
       x = "Month",
       y = "Number of FIO Incidents") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # Adjust y-axis range to start from 0
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +  # Set quarterly minor ticks on x-axis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Aggregate data by quarter
data_quarterly <- data %>%
  mutate(quarter = quarter(date),
         year = year(date),
         qy = year + quarter/4) %>%
  group_by(quarter, year, qy) %>%
  filter(qy > 2020) %>% 
  summarize(total_incidents = n())

# Plot of FIOs by quarter
fio_quarter_col <- ggplot(data_quarterly, aes(x = qy, y = total_incidents)) +
  geom_col() +
  labs(title = "FIO Incidents by Quarter",
       x = "Quarter",
       y = "Number of FIO Incidents") +
  scale_x_continuous(breaks = data_quarterly$qy,  # Set breaks to every quarter
                     labels = paste("Q", data_quarterly$quarter, "-", data_quarterly$year, sep = ""),  # Set labels to quarters and years
                     minor_breaks = NULL) +  # Remove minor ticks
  theme_minimal()


# Filter out NA values in the 'time' column
data_time_filtered <- data %>% 
  filter(!is.na(time))

# Convert 'time' column to POSIXct format with specified format
data_time_filtered$time <- as.POSIXct(data_time_filtered$time, format = "%H:%M:%S")

# Extract hour from the 'time' column
data_time_filtered <- data_time_filtered %>%
  mutate(hour = lubridate::hour(time))

# Plot of FIOs by hour
fio_hour_col <- ggplot(data_time_filtered, aes(x = hour)) +
  geom_bar(fill = "lightblue") +
  labs(title = "FIO Incidents by Hour",
       x = "Hour of the Day",
       y = "Number of FIO Incidents") +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +  # Set breaks every 3 hours
  theme_minimal()


# Plot of FIOs by time
ggplot(data_time_filtered, aes(x = time)) +
  geom_bar(fill = "lightblue") +
  labs(title = "FIO Incidents by Time",
       x = "Time",
       y = "Number of FIO Incidents") +
  theme_minimal()


# Save the named plots as files into the images folder with a white background
ggsave("images/fio_day_line.png", fio_day_line, bg = "white")
ggsave("images/fio_week_line.png", fio_week_line, bg = "white")
ggsave("images/fio_month_col.png", fio_month_col, bg = "white")
ggsave("images/fio_quarter_col.png", fio_quarter_col, bg = "white")
ggsave("images/fio_hour_col.png", fio_hour_col, bg = "white")







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


data %>% 
  group_by(key_situations) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
