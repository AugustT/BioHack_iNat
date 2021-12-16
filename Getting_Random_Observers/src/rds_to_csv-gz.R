# Script to read databricks output RDS files, and convert them to compressed
# csv's for archiving

rds_files <-
  list.files(
    file.path(
      "BioHack_iNat",
      "Getting_Random_Observers",
      "data",
      "countrywise_subset_inat_data",
      "afr"
    ),
    pattern = "rds$",
    full.names = TRUE
  )

already_present <-
  list.files(
    file.path(
      "BioHack_iNat",
      "Getting_Random_Observers",
      "data",
      "countrywise_subset_inat_data",
      "afr"
    ),
    pattern = "csv.gz$",
    full.names = TRUE
  ) %>% tools::file_path_sans_ext() %>% tools::file_path_sans_ext() %>% paste0(".rds")



purrr::map(setdiff(rds_files,already_present),
           ~ data.table::fwrite(
             readRDS(.x),
             sprintf("%s.csv.gz", tools::file_path_sans_ext(.x))
           ))

