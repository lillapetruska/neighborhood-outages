# MS&E 226 Final Project 

This repo contains all the code, data, visualizations, and reports used in making the final project for MS&E 226: Fundamentals of Data Science. This course is offered at Stanford University and taught by Ramesh Johari. The final project aims to create predictive models (both regression and classification) using unique datasets.

We use PG&E electricity outage data (found [here](https://simonwillison.net/2019/Oct/10/pge-outages/)) and 2018 ACS demographic data - identifying which neighborhood characterisitcs (at the block group level) can be used to predict both the duration of the outage and the estimated number of customers affected. Through this project we hoped to both expand our understanding of predictive models and identify any patterns in who outages impact. Given the contentious nature of PG&E's performance over the last few years, it is especially relevant now to understand how their infrastructure impacts Californians and it is imperative that we focus this investigation through the lens of equity.

## Scripts
This folder contains all code used in cleaning, EDA, and report-making.

## cleaned_data
This folder contains all cleaned data, ready to be run through the preditive models and used in EDA.

## raw_data
This folder contains all raw data (just outage data at this point), ready to be cleaned with cleaning scripts.

## Credit
* PG&E data was scraped by Simon Willison (and continues to be scraped every 10 minutes!), and can be found on his [blog](https://simonwillison.net/2019/Oct/10/pge-outages/).
* ACS data was gathered using the `tidycensus` package.
