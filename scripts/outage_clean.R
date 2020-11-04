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

# 10/26/20 - added power line shapefiles

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
power_lines <- 
  st_read(paste0(homedir, workdir, "California_Electric_Transmission_Lines.shp"))

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

# # Clean up power lines data
# power_lines_join <-
#   power_lines %>% 
#   # filter to PG&E
#   filter(Owner == "PG&E") %>% 
#   st_as_sf() %>% 
#   st_transform(crs = st_crs(ca_tracts)) %>% 
#   # join to CA block groups
#   st_join(ca_tracts)
# 
# power_lines_intersect <-
#   power_lines %>% 
#   # filter to PG&E
#   filter(Owner == "PG&E") %>% 
#   st_as_sf() %>% 
#   st_transform(crs = st_crs(ca_tracts)) %>% 
#   # join to CA block groups
#   st_intersection(ca_tracts)
# 
# geoid_lengths <-
#   tapply(st_length(power_lines_intersect), power_lines_intersect$GEOID, sum) %>% 
#   as.data.frame() %>% 
#   rownames_to_column(var = "GEOID") %>% 
#   as_tibble() %>% 
#   mutate(line_length_km = `.` / 1000.0) %>% 
#   select(GEOID, line_length_km) %>% 
#   group_by(GEOID) %>% 
#   summarise(line_length_km = sum(line_length_km))
# 
# # Join to outages data
# outages_grouped <-
#   outages_filter %>%
#   group_by(GEOID) %>%
#   summarise(
#     median_outage_duration_hr = median(outage_duration_hr),
#     median_mean_cust_affected = median(mean_cust_affected),
#     num_outages = n()
#   ) %>%
#   ungroup() %>%
#   mutate(
#     above_median_cust_affected =
#       if_else(
#         median_mean_cust_affected > median(median_mean_cust_affected), 1, 0
#       )
#   ) %>%
#   # remove median of mean customers affected column
#   select(-median_mean_cust_affected) %>%  
#   left_join(geoid_lengths, by = "GEOID")

# Remove unnecessary objects
rm(ca_tracts, outages)

# Save Results ------------------------------------------------------------
write_csv(
  outages_filter,
  file = paste0(homedir, savedir, "outages_clean.csv")
)
