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
library(reshape2)
library(ggthemr)
library(lme4)

# Source functions ----
source('Toms_code/temporal_trends/lockdown_function.R')

# Get a list of all the countries for which we have data
country_codes <- str_extract(
  list.files(path = 'Getting_Random_Observers/data/countrywise_subset_inat_data/',
             pattern = '.rds$'),
  pattern = '^[[:alpha:]]{2}')

# Loop through countries ----
for(i in country_codes){
  cat(paste('\n\nStarting', i))
  t <- system.time({
    lockdown(country_iso_code = i, 
             parallel = TRUE)
  })
  print(t)
}

# Read in all the country data ----
metrics_files <- list.files('Toms_code/temporal_trends/country_outputs', 
                            pattern = 'metrics.csv$', full.names = T)

load_country_metrics <- function(i){
  
  country_code <- regmatches(basename(i), regexpr('^[[:alpha:]]{2}', basename(i)))
  metrics <- read.csv(i)
  metrics$country <- country_code
  return(metrics)
  
}

# Apply the read function and combine data
metrics_l <- lapply(metrics_files, FUN = load_country_metrics)
metrics_df <- do.call(rbind, metrics_l)
metrics_df$country <- as.numeric(as.factor(metrics_df$country))

# Plot the results ----
metrics_long <- reshape2::melt(metrics_df,
                               id.vars = 'status',
                               measure.vars = c("activity_ratio", "active_days",
                                                "median_weekly_devoted_days", 
                                                "periodicity", "periodicity_variation",
                                                "upper_area", "upper_n_poly", 
                                                "ratio"),
                               variable.name = 'variable',
                               value.name = 'value')

# Create a simple box plot
ggthemr::ggthemr('flat dark')

ggplot(metrics_long, aes(x = status, y = value, group = status)) + 
  geom_boxplot(aes(fill = status)) +
  facet_wrap(variable ~ ., scales = 'free', ncol = 3) +
  theme(legend.position = "none")

# Save the plot
ggsave(filename = 'Toms_code/lockdown_impacts_Europe.png')

# Model the results ----
metrics_df$statusIO <- metrics_df$status == 'Lockdown'

m1 <- glm(formula = statusIO ~ activity_ratio + active_days +
            median_weekly_devoted_days + 
            periodicity + periodicity_variation +
            upper_area + upper_n_poly +
            ratio, family = binomial, data = metrics_df)

summary(m1)
write.csv(summary(m1)['coefficients'],file='Toms_code/temporal_trends/model_Europe.csv')

# In lockdown
# Active days increases:
# People record on more days
# 
# median_weekly_devoted_days increases:
# people record more days a week
# 
# periodicity_variation increases:
# the time periods between records becomes more variable
#
# upper_area decreases:
# The area over which people record becomes smaller
# 
# Ratio increases:
# records become more homogeneously distributed (probably because total area
# becomes smaller)

# Look at a mixed effect model ----
# Subset data to coutrys with min 5 observers
# obs_counts <- table(metrics_df$country)
# countries_to_use <- names(obs_counts)[obs_counts >= 10]
# metrics_df <- metrics_df[metrics_df$country %in% countries_to_use, ]

# The model advises rescaling the variables
m2 <- glmer(formula = statusIO ~ activity_ratio + active_days +
            median_weekly_devoted_days +
            periodicity + periodicity_variation +
            upper_area + upper_n_poly +
            ratio + (1|country),
          family = binomial,
          data = metrics_df)
summary(m2)
