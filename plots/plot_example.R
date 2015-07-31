require(clearskies)
require(data.table)
require(ggplot2)

source("../data/impute.R")

y = readRDS("../data/Eugene.rds")

ghi_col = which(colnames(y) == "1000")
y = impute(y, ghi_col)
y = y[, 1:ghi_col]
colnames(y)[ghi_col] = "Ghi"

y = data.table(y)
setkey(y, Year, DayOfYear)

# May 29, 2012
x = y[ list(2012) ]
x = x[ x[["Month"]] == "May" & x[["DayOfMonth"]] == 29, ]

loc = locations[ locations[["Site"]] == "EU", ] # Eugene
model = clear_sky("Ineichen", x = unique(x), y = loc, data = x[["Ghi"]])
model = clear_points(model, thresholds, 10)

p = plot(model, use.ggplot = TRUE)
png("example.png")
p + ggtitle("Eugene, Oregon - May 29, 2012")
dev.off()

