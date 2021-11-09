# Temporal trends in iNat user behaviour

# Libraries ----
library(ggplot2)
library(scales)

# Load in the data ----
data_raw <- read.csv('../../2_Generating_iNat_RM_Baseline/data/observations/draft_data.csv')

# Format the data
data_raw$date <- as.Date(data_raw$date)

# Explore the temporal variation ----
ggplot(data_raw,
       aes(x = date)) +
  geom_histogram() +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format("%b-%Y"),
               limits = as.Date(c('2017-01-01','2022-01-01'))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Define time periods ----

# Calculate the metrics ----
