# Temporal trends in iNat user behaviour
# Code by Tom August & Nikolas Pechlivanis
# Data from Simon Rolph & Pieter Huybrechts
# Work from BioHackathon Barcelona 2021

# Libraries ----
library(ggplot2)
library(scales)
library(recorderMetrics)
library(sp)
library(sf)
library(parallel)
library(COVID19)
library(data.table)
library(lubridate)
library(stringr)

# Source functions ----
source('Toms_code/temporal_trends/lockdown_function.R')

# Get a list of all the countries for which we have data
country_codes<- str_extract(
  list.files(path = 'Getting_Random_Observers/data/countrywise_subset_inat_data/',
             pattern = '.rds$'),
  pattern = '^[[:alpha:]]{2}')

# Loop through countries ----
for(i in country_codes){
  system.time({
    lockdown(country_iso_code = i, 
             parallel = TRUE)
  })
}
