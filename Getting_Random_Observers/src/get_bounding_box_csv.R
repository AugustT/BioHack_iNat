# Script to transform bounding box information from a python script from github
# to a csv

# source: https://gist.github.com/graydon/11198540

library(stringr)
library(dplyr)


# read input file
bb_txt <- readLines("BioHack_iNat/Getting_Random_Observers/data/bounding_boxes.txt")

# extract iso country code
cc_iso2c <- stringr::str_extract_all(bb_txt, pattern = "[A-Z]{2}") %>% unlist()

# extract coordinates of bounding box
bb_coord <- stringr::str_extract_all(bb_txt, pattern = "(?<=', \\().+(?=\\)\\))") %>%
  unlist() %>%
  readr::read_csv(col_names = c("long_min", "lat_min", "long_max", "lat_max"))

# output file
bind_cols(tibble(iso2c = cc_iso2c), bb_coord) %>%
  mutate(country_name = countrycode::countrycode(iso2c, origin = "iso2c", destination = "country.name")) %>%
  mutate(continent = countrycode::countrycode(iso2c, origin ="iso2c", destination = "continent")) %>% 
  data.table::fwrite("BioHack_iNat/Getting_Random_Observers/data/bounding_boxes.csv")
