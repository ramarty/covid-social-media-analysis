# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("pacman")) install.packages("pacman")

pacman::p_load(gtrendsR, tidyverse, parallel, pbmcapply, ggplot2, scales, doBy,
               jsonlite, stringr, raster, stringi, lubridate, purrr, lexiconPT,
               tidytext, quanteda, qdap, SentimentAnalysis, sentimentr, ggmap,
               tm, tokenizers, wordcloud, ggwordcloud, ggpubr, hrbrthemes,
               geojsonsf, sf, broom, readtext)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Keys -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read txt that contains my Google API
key <- readtext(file.path(github_file_path, "DataWork", "twitter", "passwords.txt"))
key <- key$text
    
register_google(key = key)
has_google_key() # Should return TRUE

# Temp directory
temp_dir <- file.path(dropbox_file_path, "Data", "twitter", "RawData", "geo")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

location_brazil <- readRDS(file.path(dropbox_file_path, "Data", "twitter", "FinalData", "brazil_tweets", "rds", "brazil_tweets_appended_clean.Rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Remove duplicates 
location_brazil <- location_brazil[!duplicated(location_brazil$location), ]

# To lower case
location_brazil <- location_brazil %>% 
    mutate(location = tolower(location)) %>% 
    filter(!is.na(location))

# Clean locations that include leading white space, emojis and include brasil
# Note: This needs to be cleaned: There are some locations that contain strings that will create conflict in the query
location_brazil <- location_brazil %>% 
    mutate(location = gsub("[^[:alnum:][:space:]]","", location),
           location = gsub("brasil|brazil", "", location),
           location = gsub("^\\s+|\\s+$", "", location),
           location = paste0(location, ", brasil"))

# Define a range of address just to try to loop : This should be removed if it is working
location_brazil<- location_brazil[1:300,]

# Get only the addresses
addresses = location_brazil$location

#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
    #use the gecode function to query google servers
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    #now extract the bits that we need from the returned list
    answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
    answer$status <- geo_reply$status
    #if we are over the query limit - want to pause for an hour
    while(geo_reply$status == "OVER_QUERY_LIMIT"){
        print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
        time <- Sys.time()
        print(as.character(time))
        Sys.sleep(60*60)
        geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
        answer$status <- geo_reply$status
    }
    #return Na's if we didn't get a match:
    if (geo_reply$status != "OK"){
        return(answer)
    }   
    #else, extract what we need from the Google server reply into a dataframe:
    answer$lat <- geo_reply$results[[1]]$geometry$location$lat
    answer$long <- geo_reply$results[[1]]$geometry$location$lng   
    if (length(geo_reply$results[[1]]$types) > 0){
        answer$accuracy <- geo_reply$results[[1]]$types[[1]]
    }
    answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
    answer$formatted_address <- geo_reply$results[[1]]$formatted_address
    return(answer)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GGMap Address extracton ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#initialise a dataframe to hold the results
geocoded <- data.frame()

# find out where to start in the address list (if the script was interrupted before):
startindex <- 1

#if a temp file exists - load it up and count the rows!
tempfilename <- file.path(temp_dir, "temp_geocoded.rds")

if (file.exists(tempfilename)){
    print("Found temp file - resuming from index:")
    geocoded <- readRDS(tempfilename)
    startindex <- nrow(geocoded)
    print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
    print(paste("Working on index", ii, "of", length(addresses)))
    #query the google geocoder - this will pause here if we are over the limit.
    result = getGeoDetails(addresses[ii]) 
    print(result$status)     
    result$index <- ii
    #append the answer to the results file.
    geocoded <- rbind(geocoded, result)
    #save temporary results as we are going along
    saveRDS(geocoded, tempfilename)
}

