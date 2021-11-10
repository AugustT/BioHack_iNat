# BioHack_iNat
Biohackaton 2021 - Team investigating recorder behaviour on iNaturalist

- [BioHack_iNat](#biohack_inat)

## Getting metrics for the mean Spanish plant collector

### Building a list of observers

The goal is the create metrics for a list of 'mean' observers. We created a gbif export for research grade inaturalist observations, in spain, since 2016. Then filtered on observers with atleast 100 observations since then. 

The export is here: GBIF.org (09 November 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.svwnrp 

This results in 461 observers meeting this criteria.

### Getting a data dump from inat

Make a directory called `data_inat` in the main repo directory and use aws command line tool (https://aws.amazon.com/cli/) to download data:

```
mkdir data_inat
cd data_inat
aws s3 cp s3://inaturalist-open-data/observations.csv.gz observations.csv.gz --no-sign-request
```

The `data_inat` directory is git ignored so will not be committed/pushed. This data is currently not used in the workflow.

### Getting all inat observations for these users

This part of the process is run in different code chunks in https://github.com/AugustT/BioHack_iNat/tree/main/2_Generating_iNat_RM_Baseline

From the 461 observers from GBIF - we don't actually have their usernames, we have a `recordedBy` column which is a mix of names and usernames. Therefore we need to resolve these to iNaturalist usernames. We can do this by using the endpoint: https://api.inaturalist.org/v1/docs/#!/Users/get_users_autocomplete 

If multiple users are found by this method we currently just drop those users. This leaves us with 319 users.

Next we make a query to the iNaturalist API https://api.inaturalist.org/v1/docs/#!/Observations/get_observations but it is wrapped in the `get_inat_obs_user()` function in R package `rinat` https://cran.r-project.org/web/packages/rinat/index.html. However in order to filter by location and species I have edited the function to access extra bit of the query, which for plants in Spain is `"&taxon_id=47126&place_id=6774"`.

This query is run for all users with a maximum number of records as 5000 - some observers do have over 5000 records so this is just an arbitrary limit whilst we're hacking. This results is then saved as a dataframe as a `.rds` object in https://github.com/AugustT/BioHack_iNat/tree/main/2_Generating_iNat_RM_Baseline/data/user_obvs

These are then all loaded in and combined into one dataframe using
```
all_observations_raw <- paste0("2_Generating_iNat_RM_Baseline/data/user_obvs/",list.files(path = "2_Generating_iNat_RM_Baseline/data/user_obvs", pattern = ".rds")) %>%
  map(readRDS) %>% 
  bind_rows()
```

### Preparing data for recordMetrics

Appropriate columns are selected:
 * `id` - the observation id
 * `recorder` - the recorder username
 * `species` - the species scientific name
 * `date` - the date in YYYY-MM-DD format and converted to date using `as.Date()`
 * `long` - decimal longitude
 * `lat`- decimal latitude
 * `quality_grade`  - not essential for recorder metrics but possible useful

We also derive these columns
 * `species_level` - is the record identified to species level? (determined in a hacky was by checking if there's a space in the `species` column
 * `country` - country determined by using function `coords2country()` function defined in `coords2country.R`, then used to remove any records that are not from the target country
 * `km_sq` - the km square (stored as character) derived by converting points to spanish national grid and rounding to nearest 1000 metres

This file is then saved to `2_Generating_iNat_RM_Baseline/data/observations/baseline_observations_spain_plants.rds` and is ready for plugging into recorderMetrics package.

### Calculating metrics
