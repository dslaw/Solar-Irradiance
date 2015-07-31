## Get the links to Archival solar data from solardat.uoregon.edu

require(stringr)
require(RCurl)
require(XML)

linksfile = "solardatlinks.txt"
endyear = "2014"
endmonth = "12"

## Scrape station names from Select Archival files page
selectarchivalurl = "http://solardat.uoregon.edu/SelectArchival.html"
html = htmlTreeParse(selectarchivalurl, useInternalNodes = TRUE)
xpath = '//table/tr/td/input[@type="CHECKBOX"]/following::td[1]'
stations = xpathSApply(html, xpath, xmlValue)

## Construct POST parameters list
POST_parameters = list("CalledFrom" = "MONTH_BLOCK",
                       "EndMonth" = endmonth,
                       "EndYear"= endyear,
                       "StartMonth"= "01",
                       "StartYear"= "1975",
                       "SubmitButton"= "Select files"
                       )
station_params = as.list(rep("on", length(stations)))
names(station_params) = stations
POST_parameters = c(POST_parameters, station_params)

## Submit url with POST parameters for all stations filled out
archivaldataurl = "http://solardat.uoregon.edu/cgi-bin/ShowArchivalFiles.cgi"
archivaldata = postForm(archivaldataurl, style = "post",
                        .params = POST_parameters)

## Scrape links to data
archival_html = unlist(strsplit(archivaldata, "\n"))
pattern = "download/Archive/(.+?).txt"
filenames = str_match(archival_html, pattern)
filenames = Filter(Negate(is.na), filenames[, 2])

# Format file names into links
links = sapply(filenames, function(filename) {
                   link = "http://solardat.uoregon.edu/download/Archive/";
                   paste0(link, filename, ".txt") })
links = unname(links)

# Write links to file
f = file(linksfile, "w")
writeLines(links, con = f)
flush(f)
close(f)

