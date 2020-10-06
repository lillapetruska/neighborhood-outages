# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Oct. 5, 2020                
# Last revised: Oct. 5, 2020                 
# Project: MS&E 226 Final Project       
# Subproject: Data Cleaning 
# Re: Clean and prepare PG&E power outage data  
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script cleans PG&E outage data collected from 
# https://pge-outages.simonwillison.net/pge-outages. Prepares it for joining
# with census data.

# Inputs:
# Raw PG&E outage data

# Outputs:
# Cleaned PG&E Census data

# Update log: 

# Setup -------------------------------------------------------------------
# Packages: 
library(tidyverse)
library(jsonlite)
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
# NOTE: this is just a sample of the outages data (which has >3 million rows)
outages <- read_csv(paste0(homedir, workdir, "outage_snapshots.csv"))

# Parameters:

# Main Script -------------------------------------------------------------

# Read in California tracts
ca_tracts <- tracts(state = "CA")

# Filter outages and geocode lat/long to Census tract
outages_filter <-
  outages %>% 
  # Extract snapshot label (for date), est affected, lat/long
  select(
    snapshot_label,
    estCustAffected, 
    latitude,
    longitude
  ) %>% 
  # change snapshot date object to quarter
  # (can't trust date because it's date of snapshot, not actual outage - but
  # good to approximate which quarter)
  mutate(snapshot_label = quarter(snapshot_label, with_year = TRUE)) %>% 
  # convert to sf object
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(ca_tracts)) %>%
  # join to oakland_tracts
  st_join(ca_tracts) %>% 
  # drop geometry
  st_drop_geometry() %>% 
  # select out unnecessary columns
  select(-c(STATEFP:TRACTCE, NAME:INTPTLON))

# Remove unnecessary objects
rm(ca_tracts, outages)

# Save Results ------------------------------------------------------------
write_csv(
  outages_filter,
  path = paste0(homedir, savedir, "outages_clean.csv")
)