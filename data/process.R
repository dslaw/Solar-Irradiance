## Process University of Oregon solar irradiance data

find_links <- function(station, year = NULL, month = NULL, links){
    ## Returns links to archival data by station.
    ## Options to narrow the search by year and month.
    ## The station argument is NOT formatted.

    format_number <- function(x){
        if (is.null(x))
            return("[0-9]{2}")

        string = as.character(x)
        chars = strsplit(string, "")[[1]]
        n = length(chars)

        if (n == 1) {
            last_two_digits = paste0("0", chars)
        } else {
            last_two_digits = paste0(chars[n-1], chars[n])
        }
        return(last_two_digits)
    }

    if (!is.character(station))
        stop()

    year = format_number(year)
    month = format_number(month)
    unknown = "[A-Z]{2}" # Don't know what this sequence denotes

    parameters = lapply(c(station, unknown, year, month), toupper)
    pattern = paste0(parameters, collapse = "")
    return(links[grep(pattern, links)])
}

which_cols <- function(x){
    ## Create a vector of columns to keep based on a single data frame.
    ## Vector to be passed to colClasses in read.table, read.delim etc
    ## Keep only:
    ## year-day (first column)
    ## time (second column)
    ## global irradiance data (header flag between 1000 and 1970)
    ## data quality flag columns that correspond to global irradiance data

    header = unlist(x)
    data_cols = header[3:length(header)]
    keep = ifelse(data_cols < 2000 & data_cols !=0, TRUE, FALSE)
    flags = c(FALSE, keep[-length(keep)]) # offset by one to get flags
    keep = ifelse(flags, TRUE, keep) # update to include flags and data
    keep = c(TRUE, TRUE, keep) # include year-day and time columns
    return(keep)
}

retrieve <- function(url, ...){
    ## Load University of Oregon solar data.
    ## Uses fread from data.table if available, otherwise read.delim.
    ## Optional arguments can be passed to read.delim (if using).

    if (!require(data.table)) {
        x = read.delim(url, header = FALSE, ...)
    } else {
        x = fread(url, showProgress = FALSE, verbose = FALSE)
    }

    header = unlist(x[1, ])
    # drop header/metadata and convert from data.table to data.frame
    x = as.data.frame(x[-1, ])
    cols = which_cols(header) # remove non-global irradiance data
    x = x[, cols]
    header = header[cols]
    return(list(df = x, header = header))
}

which_month <- function(x, year = 2000){
    ## Return the month given the day of year (x) and year.

    year = as.numeric(year)
    start_of_year = paste0(year - 1, "-12-31") # Count starts at 0
    date_inst = as.Date(x, origin = start_of_year)
    return(months(date_inst))
}

day_of_month <- function(x, year = 2000){
    ## Return day of month based on day of year.

    year = as.numeric(year)
    start_of_year = paste0(year - 1, "-12-31")
    datetime = as.Date(x, origin = start_of_year)
    datetime_POSIX = as.POSIXlt(datetime, tz = "UTC")
    return(datetime_POSIX$mday)
}

reformat_data <- function(x, cols){
    ## Create a new dataframe with added variables Year and Month (string). Add
    ## column and row names.
    ## x is the dataframe to convert, cols is the associated header.

    # Isolate solar irradiance data
    dayofyear = x[, 1]
    hour = x[, 2]
    x[, 2] = NULL # raises an error if both are removed at once
    x[, 1] = NULL

    # first entry in cols is the site location code
    year = unname(cols[2])
    header = c("Year",
               "Month",
               "DayOfMonth",
               "DayOfYear",
               "Interval",
               "Hour",
               unname(cols[3:length(cols)]))
    new_df = data.frame(Year = rep(year, nrow(x)),
                        Month = which_month(dayofyear, year = year),
                        DayOfMonth = day_of_month(dayofyear, year = year),
                        DayOfYear = dayofyear,
                        Interval = rep(which_interval(hour), nrow(x)),
                        Hour = hour,
                        x)
    colnames(new_df) = header
    new_df[, "Month"] = as.character(new_df[, "Month"])
    return(new_df)
}

which_interval <- function(x){
    ## Return the interval between discrete points at which the data was
    ## collected. Assumes that interval is consistent for the entire data set.
    ## Takes the relevant column in a dataframe as input i.e. x = df$Hour

    # Because only the interval between minutes is necessary, hours are
    # removed, as are any points that fall on the hour. The latter is to
    # account for the fact that there are 60 minutes in an hour rather than
    # 100.

    hours = unique(x)
    minutes = unique(hours %% 100) # Remove hours
    minutes = minutes[minutes != 0] # Remove points on the hour
    interval = unique(diff(minutes))
    if (length(minutes) == 0)
       interval = 60
    return(interval)

    #votes = table(diff(x))
    #interval = names(which.max(votes))
    #if (interval == "100")
    #    interval = "60"
    #return(as.numeric(interval))
}

set_NA <- function(x, flags = c(13, 99)){
    ## Replace fields that correspond to a quality control flag in the next
    ## column with NA

    flag_cols = which(colnames(x) %in% "0")

    if (length(flag_cols) == 0) {
        warning("No quality control flag columns found")
        return(x)
    }

    # coerce to dataframe in case there is only a single column
    flagged = apply(as.data.frame(x[ flag_cols]), 2, function(field)
                    ifelse(field %in% flags, TRUE, FALSE))
    x[, (flag_cols - 1)][flagged] = NA
    return(x)
}

rm_flag_cols <- function(x){
    ## Remove data quality flag columns from the dataframe

    flag_cols = which(colnames(x) %in% "0")

    if (length(flag_cols) == 0)
        return(x)
    else
        return(x[, -flag_cols])
}

