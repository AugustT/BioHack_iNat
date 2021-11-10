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

The data directory is git ignored so will not be committed/pushed

### Getting all inat observations for these users

### Calculating metrics
