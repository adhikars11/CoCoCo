# Load required libraries
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(tidyr)     # For reshaping data
library(stringr)   # For string manipulation

# Step 1: Set working directory
setwd("/Users/shishamadhikari/Desktop/Research/Co-Co-Co/empirics/data")

# Step 2: Load datasets
pwt_data <- read_excel("pwt1001.xlsx", sheet = 3) 
chinn_ito <- read_excel("kaopen.xls") %>%
  select(country_name, year, kaopen) %>%
  rename(country = country_name)

hhi_data <- read_excel("country-hhi.xlsx", sheet = 2)
patent_data <- read_excel("greenpatents.xlsx", sheet = 2)

# Step 3: Clean and reshape datasets

## 3.1 Select relevant variables from Penn World Table
pwt_clean <- pwt_data %>%
  select(
    countrycode, country, year, rgdpe, rgdpo, pop, emp, hc, 
    ccon, cgdpe, ck, ctfp, labsh, delta, xr, pl_con
  ) %>%
  rename(ccode = countrycode)  # Rename for consistency

## 3.2 Reshape HHI data from wide to long format
hhi_clean <- hhi_data %>%
  rename(country = `Country Name`) %>%
  pivot_longer(cols = `1988`:`2022`, 
               names_to = "year", 
               values_to = "hhi_index") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(hhi_index))  # Remove rows with missing HHI values

## 3.3 Summarize total patents at country-year level
patent_summary <- patent_data %>%
  group_by(Country, Year) %>%
  summarise(total_patents = sum(`Filed Patents`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(country = Country, year = Year)

## 3.4 Summarize enabling technology patents at country-year level
enabling_patents <- patent_data %>%
  filter(Technology == "Enabling Technologies") %>%
  group_by(Country, Year) %>%
  summarise(enabling_tech_patents = sum(`Filed Patents`, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(country = Country, year = Year)

# Step 4: Check for country name inconsistencies using anti_join
## Create unique data frames for comparison
pwt_countries <- pwt_clean %>% distinct(country)
hhi_countries <- hhi_clean %>% distinct(country)
patent_countries <- patent_summary %>% distinct(country)

## Find mismatched countries
missing_in_hhi <- anti_join(pwt_countries, hhi_countries, by = "country")
missing_in_patent <- anti_join(patent_countries, pwt_countries, by = "country")

# Print results
print("Countries in PWT but missing in HHI data:")
print(missing_in_hhi)

print("Countries in PWT but missing in Patent data:")
print(missing_in_patent)

# Step 5: Merge datasets
final_data <- pwt_clean %>%
  inner_join(chinn_ito, by = c("country", "year")) %>%
  left_join(hhi_clean, by = c("country", "year")) %>%
  left_join(patent_summary, by = c("country", "year")) %>%
  mutate(total_patents = ifelse(is.na(total_patents), 0, total_patents)) %>%
  left_join(enabling_patents, by = c("country", "year")) %>%
  mutate(enabling_tech_patents = ifelse(is.na(enabling_tech_patents), 0, enabling_tech_patents))

# Step 6: Save the final dataset
write.csv(final_data, "final_data.csv", row.names = FALSE)

# Step 7: Inspect the final data
print(head(final_data))
print(summary(final_data))