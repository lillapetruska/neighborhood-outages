# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Oct. 6, 2020                
# Last revised: Oct. 16, 2020                 
# Project: MS&E 226 Final Project       
# Subproject: Data Cleaning 
# Re: Prepare ACS data 
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script prepares block group level ACS data to join with outages

# Inputs:
# NA (use tidycensus package)

# Outputs:
# Cleaned ACS data

# Update log: 
# 10/6/20 - reading in block group-level 2018 ACS variables: race/ethnicity, 
# income, tenure (owner/renter), college-educated, poverty, vacancy rates, 
# food stamps/SNAP
# Race (proportions) - 8 columns (although some might need to be dropped due to 
# insufficient data)
# Income - should we use median income, AMI groupings, or raw $10,000 intervals?
# College-educated (proportion) - 1 column, could be converted to binary based 
# on 50% threshold
# Tenure (proportion) - 2 columns (really 1), could be converted to 1 binary
# based on 50% ownership/rentership threshold
# Poverty (proportion) - 1 column, could be converted to binary
# Vacancy (proportion) - 1 column, could be converted to binary
# Food stamps/SNAP (proportion) - 1 column, could be converted to binary

# 10/16/20 - switching over to tracts (more accurate demographics, can account 
# for larger outages). dropping poverty (way too correlated to income measures).
# dropped SNAP variable as well.

# 10/22/20 - switched from tract median income to county median income to 
# determine income groupings.

# Setup -------------------------------------------------------------------
# Packages: 
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
options(tigris_use_cache = TRUE)

# Directories: 
homedir <- "E:/neighborhood-outages/"
workdir <- "raw_data/"
savedir <- "cleaned_data/"
setwd(homedir)

# Import data: 

# Parameters:

# Main Script -------------------------------------------------------------

# Load available variables for 2018 ACS
acs_vars <- load_variables(year = 2018, dataset = "acs5", cache = TRUE)

# Read in ACS data
## ACS race data (2018)
acs_race <-
  get_acs(
    geography = "tract",
    table = "B03002",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>% 
  select(GEOID, label, estimate) %>% 
  mutate(
    # convert names to better form
    label = str_to_lower(str_replace_all(label, "!!|\\s+", "_")),
    # short estimate to est
    label = str_replace_all(label, "estimate", "est"),
    # remove unnecessary character strings
    label = str_remove_all(label, "est_total_not_hispanic_or_latino_|_alone")
   ) %>% 
  # remove total non-hispanic and hispanic by race
  filter(!str_detect(label, "est_total_not_hispanic_or_latino|or_latino_")) %>% 
  # pivot table
  pivot_wider(
    names_from = label,
    values_from = estimate
  ) %>% 
  # rename multiracial category
  rename(multi_racial = two_or_more_races) %>%
  # select out separated multiracial categories
  select(-starts_with("two_or_more")) %>% 
  # create proportions by tract
  mutate(
    across(
      where(is.double),
      ~ . / est_total,
      .names = "prop_{.col}"
    )
  ) %>% 
  # select for just tract ID, total population, and proportions
  select(GEOID, starts_with("prop")) %>%
  select(-prop_est_total) %>% 
  rename(prop_latino = prop_est_total_hispanic_or_latino)

## ACS income data (2018)
### Read in median income by tract
acs_median_income <-
  get_acs(
    geography = "county",
    variables = "B06011_001",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  select(GEOID, ami = estimate) 

## ACS income data (2018)
acs_income <-
  get_acs(
    geography = "tract",
    table = "B19001",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>% 
  select(GEOID, label, estimate) %>% 
  mutate(
    # convert names to better form
    label = str_to_lower(str_replace_all(label, "!!|\\s+", "_")),
    # short estimate to est
    label = str_replace_all(label, "estimate", "est"),
    est_total = if_else(label == "est_total", estimate, NA_real_)
  ) %>% 
  group_by(GEOID) %>% 
  fill(everything(), .direction = "down") %>% 
  ungroup() %>% 
  filter(label != "est_total") %>% 
  mutate(
    label = str_extract(label, "\\$\\d+,\\d+$"),
    label = str_remove_all(label, "\\$|[:punct:]"),
    label = if_else(is.na(label), 500000, as.double(label)),
    county = str_extract(GEOID, "^\\d{5}")
  ) %>% 
  rename(upper_income_limit = label) %>% 
  left_join(acs_median_income, by = c("county" = "GEOID")) %>% 
  # construct the income categories
  mutate(
    income_category =
      case_when(
        upper_income_limit <= (.30 * ami)       ~ "eli",
        upper_income_limit <= (.50 * ami) & 
          upper_income_limit > (.30 * ami)      ~ "vli",
        upper_income_limit <= (.80 * ami) & 
          upper_income_limit > (.50 * ami)      ~ "li",
        upper_income_limit <= (1.20 * ami) & 
          upper_income_limit > (.80 * ami)      ~ "mi",
        upper_income_limit > (1.20 * ami)       ~ "hi",
        TRUE                                    ~ NA_character_
      )
  ) %>%
  select(GEOID, estimate, income_category, est_total) %>% 
  group_by(GEOID, income_category) %>% 
  mutate(estimate = sum(estimate, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # change shape of data
  pivot_wider(names_from = income_category, values_from = estimate) %>% 
  # create proportions by tract
  mutate(
    across(
      eli:vli,
      ~ . / est_total,
      .names = "prop_{.col}"
    )
  ) %>% 
  # select for just tract ID, total population, and proportions
  select(GEOID, starts_with("prop")) %>% 
  mutate(across(where(is.double), ~ replace_na(.x, replace = 0)))

rm(acs_median_income)

## ACS tenure data (2018)
acs_tenure <-
  get_acs(
    geography = "tract",
    table = "B25003",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>% 
  select(GEOID, label, estimate) %>% 
  mutate(
    # convert names to better form
    label = str_to_lower(str_replace_all(label, "!!|\\s+", "_")),
    # short estimate to est
    label = str_replace_all(label, "estimate", "est"),
    est_total = if_else(label == "est_total", estimate, NA_real_)
  ) %>% 
  group_by(GEOID) %>% 
  fill(everything(), .direction = "down") %>% 
  ungroup() %>% 
  filter(label != "est_total") %>% 
  mutate(label = str_remove_all(label, "est_total_|_occupied")) %>% 
  pivot_wider(names_from = label, values_from = estimate) %>% 
  # create proportions by tract
  mutate(
    across(
      owner:renter,
      ~ . / est_total,
      .names = "prop_{.col}"
    )
  ) 

## ACS college-educated data (2018) 
acs_education <-
  get_acs(
    geography = "tract",
    table = "B15003",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>% 
  select(GEOID, label, estimate) %>% 
  mutate(
    # convert names to better form
    label = str_to_lower(str_replace_all(label, "!!|\\s+", "_")),
    # short estimate to est
    label = str_replace_all(label, "estimate", "est"),
    est_total = if_else(label == "est_total", estimate, NA_real_)
  ) %>% 
  group_by(GEOID) %>% 
  fill(everything(), .direction = "down") %>% 
  ungroup() %>% 
  filter(label != "est_total") %>% 
  mutate(
    label = str_remove_all(label, "est_total_"),
    label =
      case_when(
        str_detect(
          label, "associate|bachelor|master|professional|doctorate"
        )                                                           ~ "college",
        str_detect(
          label, "high_school|ged_or_|some_college"             
        )                                                       ~ "high_school",
        TRUE                                                    ~ "less_than_hs"
      )
  ) %>% 
  group_by(GEOID, label) %>% 
  mutate(estimate = sum(estimate, na.rm = TRUE)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  pivot_wider(names_from = label, values_from = estimate) %>% 
  # create proportions by tract
  mutate(
    across(
      college:less_than_hs,
      ~ . / est_total,
      .names = "prop_{.col}"
    )
  ) %>% 
  # select for just tract ID, total population, and proportions
  select(GEOID, starts_with("prop"))

## ACS vacancy data (2018) 
acs_vacancy <-
  get_acs(
    geography = "tract",
    table = "B25004",
    year = 2018,
    cache_table = TRUE,
    state = "CA"
  ) %>% 
  left_join(acs_vars, by = c("variable" = "name")) %>% 
  select(GEOID, label, estimate) %>% 
  mutate(
    # convert names to better form
    label = str_to_lower(str_replace_all(label, "!!|\\s+", "_")),
    # short estimate to est
    label = str_replace_all(label, "estimate", "est")
  ) %>% 
  filter(
    label %in% 
      c(
        "est_total_for_rent", "est_total_rented,_not_occupied", 
        "est_total_for_sale_only", "est_total_sold,_not_occupied"
      )
  ) %>% 
  mutate(label = str_remove_all(label, "est_total_|,")) %>% 
  pivot_wider(names_from = label, values_from = estimate) %>% 
  left_join(acs_tenure, by = "GEOID") %>% 
  transmute(
    GEOID = GEOID,
    rental_vacancy_rate = for_rent / (renter + rented_not_occupied + for_rent),
    owner_vacancy_rate = for_sale_only / (owner + sold_not_occupied + for_sale_only)
  )

### Update tenure
acs_tenure <- 
  acs_tenure %>%  
  # select for just tract ID, total population, and proportions
  select(GEOID, starts_with("prop"))

# Merge all ACS data
acs_merged <-
  acs_race %>% 
  left_join(acs_income, by = "GEOID") %>% 
  left_join(acs_education, by = "GEOID") %>% 
  left_join(acs_tenure, by = "GEOID") %>% 
  left_join(acs_vacancy, by = "GEOID")

# Save Results ------------------------------------------------------------
write_csv(
  acs_merged,
  file = paste0(homedir, savedir, "acs_clean.csv")
)
