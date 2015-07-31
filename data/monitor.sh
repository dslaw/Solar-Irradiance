#!/usr/bin/env bash

## Provides a summary of a log file output from import.R
## Can be run at any point during execution of import.R or after completion

file=$1

# Links that R failed to retrieve
failed=$(cat $file |
    egrep -o 'Failed (.+)' |
    cut -d: -f2,3 |
    sed s/' '/''/)

# Number of failures
numfails=$(cat $file |
    egrep -o 'Failed (.+)' |
    wc -l)

# Number of links read
successful=$(cat $file |
    egrep -o 'Read: (.+)' |
    wc -l)

# Number of stations read
numstations=$(cat $file |
    egrep -o '/[A-Z]{2}' |
    uniq |
    wc -l)

# Start time
started=$(head -n 2 $file |
    egrep -o '[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}')

# End time / last time
ended=$(tail -n 1 $file |
    egrep -o '[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}')

# Output
echo Started at: $started, last time at: $ended
echo Number of stations: $numstations
echo Number of links successfully read: $successful
echo "Failed links: ($numfails total)"
printf "%s\n" $failed

