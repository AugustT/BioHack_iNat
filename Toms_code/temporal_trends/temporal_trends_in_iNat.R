# Temporal trends in iNat user behaviour

# Libraries ----
library(ggplot2)
library(scales)

# Load in the data ----
data_raw <- read.csv('../../2_Generating_iNat_RM_Baseline/data/observations/draft_data.csv')

# Format the data
data_raw$date <- as.Date(data_raw$date)

str(data_raw)



# Explore the temporal variation ----
ggplot(data_raw,
       aes(x = date)) +
  geom_histogram()

# Define time periods ----

# Calculate the metrics ----
