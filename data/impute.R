## Handle negative and NA values

impute <- function(x, which.col){
    ## Impute negative or otherwise missing (NA) values using data from other
    ## sensors
    ## This may return non integers

    cols = which.col:ncol(x)

    if (length(cols) < 2)
        stop('Insufficient data to perform imputation')

    # Set negative values to NA
    x[, cols] = lapply(x[, cols], function(a)
                       ifelse(a < 0 | is.na(a), NA, a))

    # Perform imputation on selected column
    to_impute = which(is.na(x[[which.col]]))
    imputed = apply(x[to_impute, cols], 1, function(a)
                      median(a, na.rm = TRUE))

    x[to_impute, which.col] = imputed
    return(x)
}

rm_day <- function(x){
    ## If one or more rows contains a NA value in a day, remove that day from
    ## the dataframe.

    NA_indices = as.logical(rowSums(is.na(x)))
    days_with_NA = x[NA_indices, ]
    days_with_NA = unique(days_with_NA[, c("Year", "Month", "DayOfMonth")])

    year = ifelse(x[, "Year"] %in% days_with_NA[, "Year"], TRUE, FALSE)
    month = ifelse(x[, "Month"] %in% days_with_NA[, "Month"], TRUE, FALSE)
    dayofmonth = ifelse(x[, "DayOfMonth"] %in% days_with_NA[, "DayOfMonth"],
                        TRUE, FALSE)
    prune = mapply(function(x1, x2, x3) x1 & x2 & x3,
                   year, month, dayofmonth)
    return(x[!prune, ])
}

