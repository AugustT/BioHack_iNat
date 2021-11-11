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
data_raw <- readRDS(file = '2_Generating_iNat_RM_Baseline/data/observations/by_country/observations_ES_processed.rds')

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


# new plot ----------------------------------------


lockdown_var = "workplace_closing"

who = which(!is.na(covid19.data[[lockdown_var]]) & 
              covid19.data[[lockdown_var]] != 0)

lockdown_var_data = covid19.data[who, ]

lockdown_var_data = lockdown_var_data[order(lockdown_var_data$date), ]

who = which(diff(lockdown_var_data$workplace_closing) != 0)

lockdown_var_data = lockdown_var_data[c(1, who), ]

lockdown_var_data$week.id = week(lockdown_var_data$date)

lockdown_var_data[[lockdown_var]] = as.character(lockdown_var_data[[lockdown_var]])

lockdown_var_data$xlabel = paste0(
  str_split(lockdown_var_data$date, "\\-", simplify = TRUE)[,1],
  "week#", 
  lockdown_var_data$week.id
)

lockdown_var_data$xlabel = factor(lockdown_var_data$xlabel, 
                                  levels = unique(lockdown_var_data$xlabel))

data.covid.period$previous.code = paste0(data.covid.period$year - 1,
                                    "week#", 
                                    data.covid.period$week.id)

who = match(data.covid.period$previous.code, data.previous.year$xlabel)

data.covid.period$prop.previous = data.previous.year[who, ]$prop

data.covid.period$prop.diff = data.covid.period$prop - data.covid.period$prop.previous

data.covid.period$reg = "upregulated"
data.covid.period[which(data.covid.period$prop.diff < 0), ]$reg = "downregulated"
data.covid.period[which(data.covid.period$prop.diff == 0), ]$reg = "downregulated"



ggplot(data = data.covid.period) +
  
  geom_col(aes(x = xlabel, y = prop.diff, fill = reg)) +
  
  geom_vline(data = lockdown_var_data, 
             aes(xintercept = xlabel, 
                 color = workplace_closing)) +
  
  scale_y_continuous(labels = percent) +
  
  theme_minimal() +
  
  theme(legend.position = "bottom",
        
        axis.text.x = element_text(angle = 90)) +
  
  labs(title = "COVID period vs Previous year")



# Calculate metrics for all users with min records in each period 
country_var = "ESP" # iso_alpha_3

data_raw = data_raw[order(data_raw$date), ]
data_raw = setDT(data_raw)

covid19.data = covid19(country = country_var, level = 1)
covid19.data = setDT(covid19.data)

covid19.data = covid19.data[order(covid19.data$date), ]

plot(covid19.data$date, covid19.data$stay_home_restrictions)
plot(covid19.data$date, covid19.data$workplace_closing)
plot(covid19.data$date, covid19.data$transport_closing)

lockdown_var = "workplace_closing"
records_per_period = 10

who = which(!is.na(covid19.data[[lockdown_var]]) & 
              covid19.data[[lockdown_var]] > 2)

lockdown.date <- as.Date(na.omit(covid19.data$date[covid19.data[[lockdown_var]] > 2]))

data_lockdown <- data_raw[data_raw$date %in% lockdown.date,]
data_control <- data_raw[data_raw$date %in% (lockdown.date - 366),]

users_lockdown <- sort(table(data_lockdown$recorder))
users_lockdown <- names(users_lockdown[users_lockdown > records_per_period])

users_control <- sort(table(data_control$recorder))
users_control <- names(users_control[users_control > records_per_period])

# users with at least 20 data points in both time periods
users_of_interest <- users_lockdown[users_lockdown %in% users_control]
length(users_of_interest)

# Calculate the metrics ----

# See how they changed
spain_crs <- '+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs'

metrics_user <- function(user, data, year = NULL, new_crs){
  
  if(!is.null(year)){
    
    data <- data[year(data$date) == year, ]
    
  }
  
  rec.activity <- activityRatio(data = data,
                                recorder_name = user,
                                recorder_col = 'recorder',
                                date_col = 'date',
                                summer_days = NULL)
  
  rec.spatial <- spatialBehaviour(recorder_name = user, 
                                           data = data, 
                                           y_col = 'lat', 
                                           x_col = 'long',
                                           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                           new_crs = new_crs, 
                                           recorder_col = 'recorder')
  
  rec.weeklyDevotedDays <- weeklyDevotedDays(recorder_name = user,
                                             data = data,
                                             recorder_col = 'recorder',
                                             date_col = 'date')
  
  rec.periodicity <- periodicity(recorder_name = user,
                                 data = data,
                                 date_col = 'date',
                                 recorder_col = 'recorder',
                                 day_limit = 10)
  
  out <- data.frame(recorder = user,
                    activity_ratio = rec.activity$activity_ratio,
                    active_days = rec.activity$active_days,
                    median_weekly_devoted_days = rec.weeklyDevotedDays$median_weekly_devoted_days,
                    periodicity = rec.periodicity$periodicity,
                    periodicity_variation = rec.periodicity$periodicity_variation,
                    upper_area = rec.spatial$upper_area,
                    upper_n_poly = rec.spatial$upper_n_poly,
                    ratio = rec.spatial$ratio)
}

# Get user areas for both time periods
metrics_lockdown <- do.call(rbind, lapply(users_of_interest, FUN = metrics_user, data = data_lockdown, new_crs = spain_crs))
metrics_lockdown$status <- 'Lockdown'
metrics_control <- do.call(rbind, lapply(users_of_interest, FUN = metrics_user, data = data_control, new_crs = spain_crs))
metrics_control$status <- 'Control'

metrics_wide <- rbind(metrics_control, metrics_lockdown)

metrics_long <- reshape2::melt(metrics_wide,
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

ggsave(filename = 'Toms_code/lockdown_impacts_spain.pdf')

metrics_wide$statusIO <- metrics_wide$status == 'lockdown'
m1 <- glm(formula = statusIO ~ activity_ratio + active_days +
          median_weekly_devoted_days + 
          periodicity + periodicity_variation +
          upper_area + upper_n_poly +
          ratio, family = binomial, data = metrics_wide)

summary(m1)

# UK metrics ----
country_var = "GB" # iso_alpha_3

data_uk <- readRDS('Toms_code/temporal_trends/UK_inat.rds')
data_uk$date <- as.Date(data_uk$observed_on)
names(data_uk) <- c("observation_uuid", "recorder", "lat", "long", 
                    "positional_accuracy", "taxon_id", "quality_grade",
                    "observed_on", 'date')
data_uk <- data_uk[, c("recorder", "lat", "long", "taxon_id", 'date')]
data_uk = data_uk[order(data_uk$date), ]
data_uk = setDT(data_uk)

covid19.data = covid19(country = country_var, level = 1)
covid19.data = setDT(covid19.data)

covid19.data = covid19.data[order(covid19.data$date), ]

# plot(covid19.data$date, covid19.data$stay_home_restrictions)
# plot(covid19.data$date, covid19.data$workplace_closing)
# plot(covid19.data$date, covid19.data$transport_closing)

lockdown_var = "workplace_closing"
records_per_period = 10

who = which(!is.na(covid19.data[[lockdown_var]]) & 
              covid19.data[[lockdown_var]] > 2)

lockdown.date <- as.Date(na.omit(covid19.data$date[covid19.data[[lockdown_var]] > 2]))

data_lockdown <- data_uk[data_uk$date %in% lockdown.date,]
data_control <- data_uk[data_uk$date %in% (lockdown.date - 366),]

users_lockdown <- sort(table(data_lockdown$recorder))
users_lockdown <- names(users_lockdown[users_lockdown > records_per_period])

users_control <- sort(table(data_control$recorder))
users_control <- names(users_control[users_control > records_per_period])

# users with at least 20 data points in both time periods
users_of_interest <- as.numeric(users_lockdown[users_lockdown %in% users_control])
length(users_of_interest)

uk_crs <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

# Get user areas for both time periods
metrics_lockdown <- do.call(rbind, lapply(users_of_interest, FUN = metrics_user, data = data_lockdown, new_crs = uk_crs))
metrics_lockdown$status <- 'Lockdown'
metrics_control <- do.call(rbind, lapply(users_of_interest, FUN = metrics_user, data = data_control, new_crs = uk_crs))
metrics_control$status <- 'Control'

metrics_wide <- rbind(metrics_control, metrics_lockdown)

metrics_long <- reshape2::melt(metrics_wide,
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

ggsave(filename = 'Toms_code/lockdown_impacts_UK.pdf')

metrics_wide$statusIO <- metrics_wide$status == 'Lockdown'
m1 <- glm(formula = statusIO ~ activity_ratio + active_days +
            median_weekly_devoted_days + 
            periodicity + periodicity_variation +
            upper_area + upper_n_poly +
            ratio, family = binomial, data = metrics_wide)

summary(m1)


# Do a temporal analysis across years ----
# restrict to the time period where inat has a decent number of observations
data_temp <- data_raw[year(data_raw$date) >= 2015]
users_n_year <- tapply(year(data_temp$date), data_temp$recorder, FUN = function(x){length(unique(x))})
sort(users_n_year, decreasing = TRUE)
hist(users_n_year)

users_5years <- names(users_n_year[users_n_year >5])

# Set up file to store data
unlink('Toms_code/temporal_trends/metrics_by_year.csv')

for(i in users_5years){
  
  cat(paste('\nUser', i, '#', grep(i, users_5years), 'of', length(users_5years), '...'))
  
  for(j in 2015:2021){
    
    cat(paste(j, ' '))
    
    if(i %in% data_temp$recorder[year(data_temp$date) == j]){
      
      metrics_ji <- metrics_user(user = i,
                                 data = data_temp,
                                 year = j)
      
      metrics_ji$year <- j
      
      write.table(x = metrics_ji,
                  file = 'Toms_code/temporal_trends/metrics_by_year.csv',
                  sep = ',', 
                  append = TRUE, 
                  col.names = FALSE, 
                  row.names = FALSE)
    }
    
  }
  
}

read.csv('Toms_code/temporal_trends/metrics_by_year.csv',
         col.names = c("recorder", "activity_ratio", "active_days", "median_weekly_devoted_days", 
                       "periodicity", "periodicity_variation", "upper_area", "upper_n_poly", 
                       "ratio", "year"))

