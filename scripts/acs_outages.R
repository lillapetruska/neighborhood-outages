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

# Setup -------------------------------------------------------------------
# Packages:
library(tidyverse)

# Parameters
outages_filepath <- "~/GitHub/neighborhood-outages/cleaned_data/outages_clean.csv"
acs_filepath <- "~/GitHub/neighborhood-outages/cleaned_data/acs_clean.csv"

# Import data:
outages <- read_csv(outages_filepath)
acs <- read_csv(acs_filepath)

# Main Script -------------------------------------------------------------

outages_grouped <-
  outages %>%
  group_by(GEOID) %>%
  summarise(
    median_outage_duration_hrr = median(outage_duration_hr),
    median_mean_cust_affected = median(mean_cust_affected),
    num_cust_affected_flag = sum(cust_affected_flag)
  )

acs_outages <-
  acs %>%
  left_join(outages, by = "GEOID")

# CA census tracts that we don't have outage data for, as they are likely
# not serviced by PG&E.
setdiff(acs$GEOID, outages_grouped$GEOID)
