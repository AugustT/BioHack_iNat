# locally read iNaturalist dataset, filter out users by country by certain
# criteria, and make countrywise lists.

# NOTE CRITERIA
# more then one hundred observations
# only plants
# 1000 random per country


# load dataset ------------------------------------------------------------

library(dplyr)
library(data.table)


# let's set some variables for reading this big occurrence file

# file_path <- "F://CETAF/occurrence.txt"
file_path <- file.path("data", "inat_observations.csv")

number_of_columns <-
  ncol(fread(
    # file = file_path,
    cmd = paste('head -n 500', file_path),
    # quote = "",
    encoding = "UTF-8"
    # nrows = 100
  ))

number_of_rows <- system(sprintf("wc -l %s",file_path),intern = TRUE) %>% 
  stringr::str_extract("[0-9]+") %>% 
  as.integer() - 1 # remove header


column_names <- fread(
  # file = file_path,
  cmd = paste('head -n 500', file_path),
  # quote = "",
  encoding = "UTF-8"
  # nrows = 100
) %>% colnames

colselect <-
  c(
    "occurrenceID",
    # "recordedBy",
    "recordedByID",
    "taxonID",
    "taxonRank",
    "kingdom",
    "eventDate",
    "decimalLatitude",
    "decimalLongitude",
    "countryCode"
  )

col_index <-
  purrr::map_int(colselect, function(x)
    grep(x, column_names, value = FALSE, fixed = TRUE))

## function to chunked read a csv ------------------------------------------
read_occ_chunked <-
  function(chunk_start,
           chunk_size,
           file_path = file_path,
           out_path,
           column_names = column_names,
           col_index = col_index,
           number_of_columns = number_of_columns) {
    
    lines_in_memory <-
      readr::read_lines(
        file_path,
        skip = chunk_start - 0.5 * chunk_size,
        n_max = chunk_size,
        progress = FALSE
      )
    
    out <- fread(
      text = paste0(lines_in_memory,collapse = "\n"),
      quote = "",
      encoding = "UTF-8",
      col.names = column_names[col_index],
      header = FALSE,
      colClasses = replicate(number_of_columns, "character"),
      showProgress = FALSE,
      select = col_index
    )
    
    fwrite(out[, colselect],
           file = out_path,
           append = TRUE,
           showProgress = FALSE)
    
  }


## read inat occ -----------------------------------------------------------

# 
# read_occ_chunked(chunk_start = 0,
#                  chunk_size = 10000,
#                  out_path = "data/inat_observations_colsel.csv")


chunk_start <- 0
chunk_size <- 1e6

for (chunk_start in seq(from = chunk_start,
                        to = number_of_rows + chunk_size,
                        by = chunk_size)) {
  
  print(paste(chunk_start,"/",number_of_rows + chunk_size))
  
  lines_in_memory <-
    readr::read_lines(file_path,
                      skip = chunk_start - 0.5 * chunk_size,
                      n_max = chunk_size,
                      progress = FALSE)
  out <- fread(
    text = paste0(lines_in_memory,collapse = "\n"),
    # quote = "",
    encoding = "UTF-8",
    col.names = column_names[col_index],
    header = FALSE,
    colClasses = replicate(number_of_columns, "character"),
    showProgress = FALSE,
    select = col_index
  )
  
  fwrite(out[, ..colselect],
         file = "data/inat_observations_colsel.csv",
         append = TRUE,
         showProgress = FALSE)
  
}


# filtering down the inat obs ---------------------------------------------



## extract recorders that have at least 100 observations -------------------
# also filter on plant observations only

recorders_tally <-
  fread(
    "data/inat_observations_colsel.csv",
    encoding = "UTF-8",
    select = c("recordedByID", "occurrenceID", "eventDate", "kingdom")
  )[kingdom == "Plantae",] %>% 
  # mutate(eventDate = lubridate::ymd_hms(eventDate)) %>% 
  filter(as.integer(substr(eventDate,1,4)) > 2015) %>% 
  select(recordedBy,occurrenceID) %>% 
  .[, .(count = .N), by = recordedByID]

active_recorders <- filter(recorders_tally,count > 99) %>% pull(recordedByID)


# filter down the big file to only post 2016, only active observers -------
# only plants


inat_filtered <- 
  fread("data/inat_observations_colsel.csv",
      encoding = "UTF-8") %>% 
  filter(kingdom == "Plantae")
  mutate(eventDate = lubridate::ymd_hms(eventDate)) %>% 
  filter(lubridate::year(eventDate) > 2015) %>% 
  filter(recordedByID %in% active_recorders)

dplyr::glimpse(inat_filtered)

fwrite(inat_filtered,"data/inat_obs_filtered.csv.gz")

      
