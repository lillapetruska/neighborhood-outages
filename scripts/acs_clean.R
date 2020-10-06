# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Oct. 6, 2020                
# Last revised: Oct. 6, 2020                 
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
    geography = "block group",
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
  # merge multiracial categories
  mutate(
    multi_racial = sum(c_across(starts_with("two_or_more")), na.rm = TRUE)
  ) %>% 
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
  select(GEOID, est_total, starts_with("prop")) %>%
  select(-prop_est_total) %>% 
  rename(prop_latino = prop_est_total_hispanic_or_latino)

## ACS income data (2018)

## ACS tenure data (2018)

## ACS college-educated data (2018) 

## ACS poverty data (2018)

## ACS vacancy data (2018) 

## ACS food stamps/SNAP data (2018)


# Merge all ACS data

# Save Results ------------------------------------------------------------
write_csv(
  path =
)