# -------------------------------------------------------------------------
# Created by: Lilla Petruska
# Date created: Oct. 22, 2020
# Last revised: Oct. 22, 2020
# Project: MS&E 226 Final Project
# Subproject: Data Prep
# Re: Joing ACS and PG&E outages data
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script joins cleaned ACS PG&E outage data collected from
# https://pge-outages.simonwillison.net/pge-outages. Uses the outages_clean
# dataset with acs_clean dataset.

# Inputs:
# Clean PG&E outage data and clean acs data

# Outputs:
# Joined ACS and PG&E Census data

# Update log:
# 10/22/20 - created summary stats for PG&E outaged data by census tract and
# joined this data onto ACS census tract data.
# 10/22/20 - fixed binary flag, converted NA's in outage stats after ACS merge 
# to 0s

# Setup -------------------------------------------------------------------
# Packages:
library(tidyverse)

# Directories: 
homedir <- "E:/neighborhood-outages/"
workdir <- "cleaned_data/"
savedir <- "cleaned_data/"
setwd(homedir)

# Parameters
outages_filepath <- paste0(homedir, workdir, "outages_clean.csv")
acs_filepath <- paste0(homedir, workdir, "acs_clean.csv")

# Import data:
outages <- read_csv(outages_filepath)
acs <- read_csv(acs_filepath)

# Main Script -------------------------------------------------------------

outages_grouped <-
  outages %>%
  group_by(GEOID) %>%
  summarise(
    median_outage_duration_hr = median(outage_duration_hr),
    median_mean_cust_affected = median(mean_cust_affected)
  ) %>% 
  ungroup() %>% 
  mutate(
    above_median_cust_affected = 
      if_else(
        median_mean_cust_affected > median(median_mean_cust_affected), 1, 0
      )
  ) %>% 
  # remove median of mean customers affected column
  select(-median_mean_cust_affected)

acs_outages <-
  acs %>%
  left_join(outages_grouped, by = "GEOID") %>% 
  mutate(
    median_outage_duration_hr = replace_na(median_outage_duration_hr, 0),
    above_median_cust_affected = replace_na(above_median_cust_affected, 0)
  )

# CA census tracts that we don't have outage data for, as they are likely
# not serviced by PG&E.
non_outage_tracts <- 
  setdiff(acs$GEOID, outages_grouped$GEOID) %>% 
  as_tibble() %>% 
  rename(non_pge_tract = value)

rm(acs, outages, outages_grouped)

# Save Results ------------------------------------------------------------
## write merged ACS outages data
write_csv(
  acs_outages,
  file = paste0(homedir, savedir, "acs_outages.csv")
)

## write non-outage tracts
write_csv(
  non_outage_tracts,
  file = paste0(homedir, savedir, "non_outage_tracts.csv")
)