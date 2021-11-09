# Creating a random subset of inat users from Spain, who collect plants, atleast 100 observations, after 2016


# load libraries ----------------------------------------------------------
library(dplyr)
library(data.table)


# load input data ---------------------------------------------------------

inat_in <- fread("data/0047435-210914110416597.csv",
      encoding = "UTF-8",
      select = c('gbifID', 'occurrenceID', 'recordedBy')
        )


# filter input ------------------------------------------------------------


## tally observations -----------------------------------------------------
active_recorders <- 
  inat_in[, .(count = .N),by = recordedBy] %>% 
  filter(count>99) %>% 
  pull(recordedBy)


# apply filter ------------------------------------------------------------

# we have already filtered on spain and post 2016 in the export

filter(inat_in, recordedBy %in% active_recorders) %>% 
  pull(recordedBy) %>% 
  unique() %>% 
  writeLines(paste0(
    "data/spain_100obs_observers_",
    format(Sys.time(), "%F_%H-%M"),
    ".txt")
  )
