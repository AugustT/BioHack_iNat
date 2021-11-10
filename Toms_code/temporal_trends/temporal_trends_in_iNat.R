# Temporal trends in iNat user behaviour

# Libraries ----
library(ggplot2)
library(scales)
library(recorderMetrics)
library(sp)
library(sf)
library(parallel)

# Load in the data ----
data_raw <- readRDS(file = '2_Generating_iNat_RM_Baseline/data/observations/draft_data.rds')

# Explore the temporal variation ----
ggplot(data_raw,
       aes(x = date)) +
  geom_histogram(binwidth = 30) +
  scale_x_date(date_breaks = "1 month", 
               labels = date_format("%b-%Y"),
               limits = as.Date(c('2017-01-01','2022-01-01'))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.background = element_rect(color = 'white'))

ggsave(filename = 'Toms_code/tempoal_figure.pdf')

# Crop to boundng box of mainland Spain
# Simon has cleaned the data so this shouls not be needed anymore
data_raw <- data_raw[data_raw$lat > 35.946850084 & 
                     data_raw$lat < 43.7483377142 & 
                     data_raw$long > -9.39288367353 & 
                     data_raw$long < 3.03948408368, ]

plot(data_raw$long, data_raw$lat)

# Define time periods ----
# I think the way to do this is to look at people yearly
spain_crs <- '+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs'

# First we can create metrics with this data for 1 recorder
pred_a <- predictAxes(data = na.omit(data_raw),
                      recorders = unique(data_raw$recorder)[1],
                      verbose = TRUE,
                      new_crs = spain_crs)

# Turn this into a function that saves an RDS per user
axes_for_one_user <- function(user,
                              data_raw, 
                              dir_out = 'Toms_code/temporal_trends/outputs/',
                              new_crs = spain_crs){

    pred_a <- predictAxes(data = na.omit(data_raw),
                          recorders = user,
                          verbose = TRUE,
                          new_crs = spain_crs)
    saveRDS(object = pred_a, file = file.path(dir_out, user))  
  
}

system.time({
  axes_for_one_user(user = 'ahospers',
                    data_raw = data_raw, 
                    dir_out = 'Toms_code/temporal_trends/outputs/',
                    new_crs = spain_crs)
}) # 20 seconds



# parallelise this
n.cores <- detectCores()

# define cluster and export functons and variables
cl <- makeCluster(n.cores - 1)
clusterEvalQ(cl, library(recorderMetrics))
clusterEvalQ(cl, library(sp))
clusterExport(cl, c("data_raw", "axes_for_one_user", "spain_crs"))

# Run for all recorders in parallel
system.time({
  parLapplyLB(cl = cl,
              X = unique(data_raw$recorder),
              fun = axes_for_one_user, 
              data_raw = data_raw,
              dir_out = 'Toms_code/temporal_trends/outputs/',
              new_crs = spain_crs)
}) # took ~30min

stopCluster(cl)


# NIKO - cut this data to two periods:

# Lockdown period in Spain

# Same period by the year before

# Calculate metrics for all users with min ~50 records in each period 

# See how they changed

# Scale to other countries
# Calculate the metrics ----
