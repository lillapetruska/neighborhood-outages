# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Oct. 5, 2020                
# Last revised: Oct. 16, 2020                 
# Project: MS&E 226 Final Project       
# Subproject: Data Cleaning 
# Re: Clean and prepare PG&E power outage data  
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans PG&E outage data collected from 
# https://pge-outages.simonwillison.net/pge-outages. Prepares it for joining
# with census data. Uses the outages_expanded dataset with probably_ended set 
# to 1.

# Inputs:
# Raw PG&E outage data

# Outputs:
# Cleaned PG&E Census data

# Update log: 
# 10/6/20 - switched from outage_snapshot to outages_expanded, which computes 
# total duration of outage. Also switched from tract to block group.
# The result produces multiple outages per block group. Dropping date time info.

# 10/16/20 - added binary flag if number of customers affected is greater 
# than average

# 10/22/20 - switched from block groups to tracts

# Setup -------------------------------------------------------------------
# Packages: 
library(tidyverse)
library(sf)
library(tigris)
library(lubridate)
options(tigris_use_cache = TRUE)

# Directories: 
homedir <- "E:/neighborhood-outages/"
workdir <- "raw_data/"
savedir <- "cleaned_data/"
setwd(homedir)

# Import data: 
outages <- read_csv(paste0(homedir, workdir, "outages_expanded.csv"))

# Parameters:

# Main Script -------------------------------------------------------------

# Read in California block groups
ca_tracts <- tracts(state = "CA")

# Filter outages and geocode lat/long to Census tract
outages_filter <-
  outages %>% 
  # Extract possible duration hours, min/max est affected, lat/long
  select(
    possible_duration_hours,
    min_estCustAffected,
    max_estCustAffected,
    latitude,
    longitude
  ) %>%
  # determine mean customers affected
  mutate(
    mean_cust_affected = (min_estCustAffected + max_estCustAffected) / 2
  ) %>% 
  # convert to sf object
  st_as_sf(
    coords = c("longitude", "latitude"), crs = st_crs(ca_tracts)
  ) %>%
  # join to CA block groups
  st_join(ca_tracts) %>% 
  # drop geometry
  st_drop_geometry() %>% 
  # select out unnecessary columns
  select(
    GEOID, outage_duration_hr = possible_duration_hours, mean_cust_affected
  )

# Remove unnecessary objects
rm(ca_tracts, outages)

# Save Results ------------------------------------------------------------
write_csv(
  outages_filter,
  file = paste0(homedir, savedir, "outages_clean.csv")
)
