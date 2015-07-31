#!/usr/bin/env Rscript

## Pull data from solardat.uoregon.edu and import into a sqlite database.
## Before writing to database each dataframe (table) is saved in an rds file.
## All data from a station is downloaded, reformatted, combined and saved
## before moving on to the next station.

## Can turn logging off if running from the command line and use your own
## redirects, or no log file. Otherwise, a file download_log.txt will be
## generated with logging information.
## Turning on retry will cause failed links from a station to be retried once
## after the rest of the station's data is downloaded.
## Rscript [options] import.R [database name] [logging] [retry]

require(compiler); enableJIT(3)
require(data.table, quietly = TRUE, warn.conflicts = FALSE)
require(RSQLite, quietly = TRUE, warn.conflicts = FALSE)

source("process.R")

convert_to_dict <- function(pair){
    ## Create a named list from a list with value,key pairs

    pair = strsplit(pair, ",")[[1]]
    value = pair[1]
    key = pair[2]
    names(value) = key
    return(value)
}

get_and_format <- function(link){
    ## Read online data and put into a suitable format for a sqlite db
    ## Wrapper for data processing functions from process.R

    dat = tryCatch({ retrieve(link) }, error = function(e) { NA })

    # if dat is a dataframe, is.na will raise a warning (using only first
    # condition)
    if (suppressWarnings(is.na(dat))) {
        cat(as.character(Sys.time()), "Failed to open:", link, "\n")
        return(NA)
    } else {
        # Try to space out requests a bit to avoid hitting uoregon server too
        # often.
        cat(as.character(Sys.time()), "Read:", link, "\n")
        Sys.sleep(3)
    }

    dat = reformat_data(x = dat$df, cols = dat$header)
    dat = set_NA(dat, flags = c(13, 99))
    dat = rm_flag_cols(dat)
    dat$Month = as.character(dat$Month)
    return(dat)
}

retry_failures <- function(lst, links){
    ## Retry links once

    failed = is.na(lst)
    cat("Retrying failed pages\n")
    retries = lapply(links[failed], function(link) get_and_format(link))
    lst[failed] = retries

    failed_twice = is.na(lst)
    return(lst[!failed_twice])
}

remove_failures <- function(lst, ...){
    ## Remove failures (NA values) from list.
    ## additional arguments are not used, but are present so the same arguemnts
    ## can be passed as to retry_failures

    failed = is.na(lst)
    return(lst[!failed])
}

dbWriteTableMsg <- function(conn, x, name, ...){
    ## Wrapper for dbWriteTable. Catches errors and prints message.

    msg = paste0("Error writing ", name, " to database. Skipping...\n")
    tryCatch({
        dbWriteTable(conn = conn, name = name, value = x, ...)
    }, error = function(e) {
        cat(msg)
        FALSE
    })
}

## Take command line arguments or set defaults
argv = commandArgs(trailingOnly = TRUE)

if (length(argv) == 0) {
    # defaults
    dbname = "solardata.db"
    logging = TRUE
    retry = FALSE
} else {
    dbname = argv[1]
    logging = as.logical(toupper(argv[2]))
    retry = as.logical(toupper(argv[3]))
}

on_failure = if (retry) retry_failures else remove_failures

## Read in dictionary of station names and station abbreviations
# $bash makestationdict.sh > stationdict.txt
stations = readLines("stationdict.txt")
stations = unlist(lapply(stations, convert_to_dict))
stations = sort(stations) # alphabetic order

## Read data links for stations that are in the dictionary and in the links
## file
links = readLines("solardatlinks.txt")
station_links = lapply(names(stations), find_links, links = links)
names(station_links) = stations
station_links = Filter(function(a) length(a) != 0, station_links)

if (logging)
    sink("download_log.txt")

for (i in 1:length(station_links)){
    station = names(station_links)[i]
    cat("Starting", station, "with", length(station_links[[i]]), "links\n")
    station_data = lapply(station_links[[i]], function(link) get_and_format(link))

    # Remove failures or retry
    station_data = on_failure(lst = station_data, links = station_links)

    station_data = as.data.frame(rbindlist(station_data, fill = TRUE))
    saveRDS(station_data, file = paste0(station, ".rds"))

    db = dbConnect(SQLite(), dbname)
    tablename = gsub("[^a-zA-Z]", "", station) # format for sqlite
    successful_write = dbWriteTableMsg(conn = db,
                                       x = station_data,
                                       name = tablename)
    disconnected = dbDisconnect(db)

    cat("\n")
    rm(station_data); gc()
}

cat("Finished at", as.character(Sys.time()), "\n")

if (logging)
    sink(NULL)

