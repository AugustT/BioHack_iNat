# Combine iNat data and covid data to get the status of COVID and lockdowns
# at the time of every record
# Niko Pechlivanis

rm(list = ls())

# load libraries ----------------------------

library(data.table)
library(stringr)
library(COVID19)


# read dataset --------------

data = fread(
    "Getting_Random_Observers/data/0047435-210914110416597.csv",
    encoding ="UTF-8"
) 



# remove recurrent whitespaces ---------------------

for(i in colnames(data)) {
    
    if(is.character( data[[i]] )) {
        
        data[[i]]  = str_squish(data[[i]])
        
    }
    
}


any(duplicated(data$gbifID))

# dates --------------------------------

data$date.cl = str_split(data$dateIdentified, "\\ ", simplify = TRUE)[,1]

data$date.cl = as.Date(data$date.cl)
# 
# data$year = str_split(data$date.cl, "\\-", simplify = TRUE)[,1]
# data$month = str_split(data$date.cl, "\\-", simplify = TRUE)[,2]
# data$day = str_split(data$date.cl, "\\-", simplify = TRUE)[,3]
# 
# 
# data$year = as.numeric(data$year)
# data$month = as.numeric(data$month)
# data$day = as.numeric(data$day)

data = data[order(data$date.cl), ]

# filtering ---------------------------------------------------------------


# Pieter's code

recorder_counts = data[, .N, by = recordedBy]

data.sub = data[which(data$recordedBy %in% recorder_counts[which(N >= 100), ]$recordedBy), ]


# remove duplicates -------------------------------------

data.sub = data.sub[which(!is.na(data.sub$date.cl)), ]


# retrieve covid19 data for Spain -----------------------------

covid19.data = covid19(country = "Spain", level = 2)

covid19.data = setDT(covid19.data)


# filter based on covid19 data availability ----------------------------------

min(covid19.data$date)
max(covid19.data$date)


data.sub = data.sub[which(data.sub$date.cl >= min(covid19.data$date) & 
                              data.sub$date.cl <= max(covid19.data$date)), ]



# match datasets based on state and date -----------

data.sub.cl = list()

for(i in unique(data.sub$stateProvince)) {
    
    
    message(i)
    
    
    x = data.sub[which(data.sub$stateProvince == i), ]
    
    who = which(str_detect(covid19.data$administrative_area_level_2, i))
    
    tmp = covid19.data[who, ]
    
    who = match(x$date.cl, tmp$date)
    
    y = cbind(x, tmp[who, 3:ncol(tmp)])
    
    
    data.sub.cl[[i]] = y
    
}


data.sub.cl = rbindlist(data.sub.cl)


# clean environment -----------------------------

rm(data, data.sub, recorder_counts, 
   tmp, x, y, i, who)


fwrite(
    
    data.sub.cl[sample(1:nrow(data.sub.cl), 50), ],
    file = "nikopech_code/sampleDataset-covid19Info.tsv",
    
    row.names = FALSE,
    quote = FALSE,
    sep = "\t"
    
)
