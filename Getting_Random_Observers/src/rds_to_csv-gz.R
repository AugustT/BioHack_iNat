# Script to read databricks output RDS files, and convert them to compressed
# csv's for archiving

rds_files <-
  list.files(
    file.path(
      "BioHack_iNat",
      "Getting_Random_Observers",
      "data",
      "countrywise_subset_inat_data"
    ),
    pattern = "rds$",
    full.names = TRUE
  )

purrr::map(rds_files,
           ~ data.table::fwrite(
             readRDS(.x),
             sprintf("%s.csv.gz", tools::file_path_sans_ext(.x))
           ))

