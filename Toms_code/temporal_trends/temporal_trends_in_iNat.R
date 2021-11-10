# Temporal trends in iNat user behaviour

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


# task.1 nikopech - cut this data to two periods -----------------

# get covid19 data --------------------

country_var = "ESP" # iso_alpha_3

data_raw = data_raw[order(data_raw$date), ]
data_raw = setDT(data_raw)

covid19.data = covid19(country = country_var, level = 1)
covid19.data = setDT(covid19.data)

covid19.data = covid19.data[order(covid19.data$date), ]

# use closing

min.date = min(covid19.data$date, na.rm = TRUE)

max.date = max(covid19.data$date, na.rm = TRUE)

data.covid.period = data_raw[which(data_raw$date >= min.date & 
                            data_raw$date <= max.date), ]

data.covid.period$week.id = week(data.covid.period$date)

previous.year = data.covid.period$date - years(1)

data.previous.year = data_raw[which(data_raw$date %in% previous.year), ]

data.previous.year$week.id = week(data.previous.year$date)

# clean environment -----------------

rm(min.date, max.date, previous.year)

data.covid.period$year = str_split(data.covid.period$date, "\\-", simplify = TRUE)[,1]

data.covid.period = data.covid.period[, .N, by = .(year, week.id)]

proportion = data.covid.period[, .(N = sum(N)), by = year]

who = match(data.covid.period$year, proportion$year)

data.covid.period$prop = round(data.covid.period$N / proportion[who, ]$N,
                               digits = 4)

data.previous.year$year = str_split(data.previous.year$date, 
                                    "\\-", simplify = TRUE)[,1]

data.previous.year = data.previous.year[, .N, by = .(year, week.id)]


proportion = data.previous.year[, .(N = sum(N)), by = year]

who = match(data.previous.year$year, proportion$year)

data.previous.year$prop = round(data.previous.year$N / proportion[who, ]$N,
                               digits = 4)

rm(proportion)


data.covid.period$xlabel = paste0(data.covid.period$year, "week#",
                                  data.covid.period$week.id)

data.covid.period$xlabel = factor(data.covid.period$xlabel, 
                                  levels = unique(data.covid.period$xlabel))


lockdown_var = "workplace_closing"

who = which(!is.na(covid19.data[[lockdown_var]]) & 
              covid19.data[[lockdown_var]] != 0)

lockdown_var_data = covid19.data[who, ]

lockdown_var_data = lockdown_var_data[, .(date = min(date)), 
                                      by = `lockdown_var`]

colnames(lockdown_var_data) = c("var", "date")

lockdown_var_data$week.id = week(lockdown_var_data$date)

lockdown_var_data$xlabel = paste0(
  str_split(lockdown_var_data$date, "\\-", simplify = TRUE)[,1],
  "week#", 
  lockdown_var_data$week.id
)

lockdown_var_data$xlabel = factor(lockdown_var_data$xlabel, 
                                  levels = unique(lockdown_var_data$xlabel))

lockdown_var_data$var = as.character(lockdown_var_data$var)

gr1 = ggplot(data = data.covid.period, aes(x = xlabel, y = prop)) +
  
  geom_col() +
  
  geom_vline(data = lockdown_var_data, 
             aes(xintercept = xlabel, color = var)) +
  
  scale_y_continuous(labels = percent,
                     expand = c(0, 0),
                     limits = c(0, max(data.covid.period$prop,
                                      data.previous.year$prop))) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        
        axis.title.x = element_blank(),
        
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        
        panel.grid = element_blank(),
        
        legend.position = "top") + 
  
  labs(y = "Proportion", color = "First workplace closing",
       title = "COVID19 pandemic")


data.previous.year$xlabel = paste0(data.previous.year$year, "week#",
                                   data.previous.year$week.id)

data.previous.year$xlabel = factor(data.previous.year$xlabel, 
                                  levels = unique(data.previous.year$xlabel))

gr2 = ggplot(data = data.previous.year, aes(x = xlabel, y = prop)) +
  
  geom_col() +
  
  scale_y_continuous(labels = percent,
                     expand = c(0, 0),
                     limits = c(0, max(data.covid.period$prop,
                                       data.previous.year$prop))) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, hjust = 1),
        
        panel.grid = element_blank(),
        
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        
        axis.title.x = element_blank()) + 
  
  labs(y = "Proportion", title = "Previous year")


library(patchwork)

gr1 / gr2

# Calculate metrics for all users with min ~50 records in each period 

# See how they changed

# Calculate the metrics ----
