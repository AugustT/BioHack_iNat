# Lockdown anaylsis function

lockdown <- function(country_iso_code = "ES",
                     data_dir = 'Getting_Random_Observers/data/countrywise_subset_inat_data/',
                     out_dir = 'Toms_code/temporal_trends/country_outputs',
                     lockdown_var = "workplace_closing",
                     lockdown_values = 3,
                     min_active_days = 10,
                     proj4 = "+proj=lcc +lat_1=52.66666666666666 +lat_2=54.33333333333334 +lat_0=48 +lon_0=10 +x_0=815000 +y_0=0 +ellps=intl +towgs84=-87,-98,-121,0,0,0,0 +units=m +no_defs",
                     parallel = FALSE,
                     verbose = TRUE){
  
  if(verbose) cat('\n\nLoading observation data\n')
  
  # Load in the data for the country
  data <- readRDS(file.path(data_dir, 
                            paste(country_iso_code,
                                  'inat.rds',
                                  sep = '_')))
  
  # Create a date formatted column with a different name
  data$date <- as.Date(data$observed_on)
  
  # Rename the columns
  names(data) <- c("observation_uuid", "recorder", "lat", "long", 
                      "positional_accuracy", "taxon_id", "quality_grade",
                      "observed_on", "country", "date")
  
  # Only keep the columns we need
  data <- data[, c("recorder", "lat", "long", "taxon_id", 'date')]
  
  # Order by date
  data = data[order(data$date), ]
  
  # Covert to a Data Table
  data = setDT(data)
  
  if(verbose) cat('Loading covid data\n')
  
  # Get the covid data for the country we are working on
  covid19.data = covid19(country = country_iso_code,
                         level = 1)
  
  # Convert to data table
  covid19.data = setDT(covid19.data)
  
  # Order by date
  covid19.data = covid19.data[order(covid19.data$date), ]
  
  # Identify the dates that meet the lockdown conditions
  lockdown.date <- as.Date(na.omit(covid19.data$date[covid19.data[[lockdown_var]] %in% lockdown_values]))
  
  # Cut data the the lockdown and control period (1 year before)
  data_lockdown <- data[data$date %in% lockdown.date,]
  data_control <- data[data$date %in% (lockdown.date - 366),]
  
  # Get the names of users with enough observations in 
  # lockdown and control periods
  users_lockdown <- tapply(data_lockdown$date,
                           data_lockdown$recorder,
                           FUN = function(x) length(unique(x)))
  users_lockdown <- names(users_lockdown[users_lockdown >= min_obs_per_period])
  users_control <- tapply(data_control$date,
                          data_control$recorder,
                          FUN = function(x) length(unique(x)))
  users_control <- names(users_control[users_control >= min_obs_per_period])
  
  # Get those people in both lists
  users_of_interest <- users_lockdown[users_lockdown %in% users_control]
  
  if(verbose) cat(paste(length(users_of_interest), 'users to analyse\n'))
  
  # only run if there are people to run it on!
  if(length(users_of_interest) < 1){
    
    warning(paste('No people meet the criterea in', country_iso_code))
    
  } else {
    
    # Do parallel stuff
    if(parallel){
      
      if(verbose) cat('Running metrics in parallel\n')
      
      # Set up cluster
      cl <- makeCluster(detectCores() - 1)
      on.exit({stopCluster(cl)})
      clusterEvalQ(cl, library(recorderMetrics))
      clusterEvalQ(cl, library(sp))
      clusterExport(cl, c("data", "metrics_user"))
      
      # Run lockdown metrics
      if(verbose) cat('Running lockdown metrics\n')
      
      metrics_lockdown <- do.call(rbind,
                                  parLapply(cl = cl,
                                            X = users_of_interest,
                                            fun = metrics_user,
                                            data = as.data.frame(data_lockdown),
                                            new_crs = proj4))
      # Add status lockdown
      metrics_lockdown$status <- 'Lockdown'
      
      # Run control metrics
      if(verbose) cat('Running control metrics\n')
      
      metrics_control <- do.call(rbind, 
                                 parLapply(cl = cl,
                                           X = users_of_interest, 
                                           fun = metrics_user, 
                                           data = as.data.frame(data_control), 
                                           new_crs = proj4))
      
      # Add status control
      metrics_control$status <- 'Control'
      
      # Close the cluster
      stopCluster(cl)
      
      # Do serial stuff
    } else{
      
      # Run lockdown metrics
      if(verbose) cat('Running metrics in series\n')
      if(verbose) cat('Running lockdown metrics\n')
      
      metrics_lockdown <- do.call(rbind,
                                  lapply(users_of_interest,
                                         FUN = metrics_user,
                                         data = data_lockdown,
                                         new_crs = proj4))
      # Add status lockdown
      metrics_lockdown$status <- 'Lockdown'
      
      # Run control metrics
      if(verbose) cat('Running control metrics\n')
      
      metrics_control <- do.call(rbind, 
                                 lapply(users_of_interest, 
                                        FUN = metrics_user, 
                                        data = data_control, 
                                        new_crs = proj4))
      
      # Add status control
      metrics_control$status <- 'Control'
      
    }
    
    # Combine the 2 datasets for each period into one table
    metrics_wide <- rbind(metrics_control, metrics_lockdown)
    
    # Save this data incase we need it again!
    write.csv(metrics_wide,
              file = file.path(out_dir,
                               paste(country_iso_code,
                                     'metrics.csv',
                                     sep = "_")), 
              row.names = FALSE)

    # Convert the data into a long format for plotting with GGplot
    if(verbose) cat('Plotting metrics\n')
    
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
    # Set theme
    ggthemr::ggthemr('flat dark')
    
    # Create plot
    p <- ggplot(metrics_long, aes(x = status, y = value, group = status)) + 
      geom_boxplot(aes(fill = status)) +
      facet_wrap(variable ~ ., scales = 'free', ncol = 3) +
      theme(legend.position = "none")
    
    # Save the plot
    png(file.path(out_dir,
                  paste(country_iso_code, 
                        'plot.png', 
                        sep = "_")))
    print(p)
    dev.off()
    
    # Run a binomial model on the same data
    # Add a response variable for lockdown or not 
    metrics_wide$statusIO <- metrics_wide$status == 'Lockdown'
    
    # Run the model
    if(verbose) cat('Running Model\n')
    
    m1 <- glm(formula = statusIO ~ activity_ratio + active_days +
                median_weekly_devoted_days + 
                periodicity + periodicity_variation +
                upper_area + upper_n_poly +
                ratio,
              family = binomial,
              data = metrics_wide)
    
    # Save out the summary
    write.csv(summary(m1)['coefficients'],
              file = file.path(out_dir, 
                               paste(country_iso_code,
                                     'model.csv',
                                     sep = "_")))
    
    if(verbose) cat('Done\n')
    
  }
  
}


# A function that gets the metrics for one user
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
