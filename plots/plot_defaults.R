require(clearskies)
require(ggplot2)
require(gridExtra)

# Constants for Eugene
site = data.frame(Year = 2002, Month = 'January', DayOfMonth = 20,
                  DayOfYear = 20, Interval = 1)

loc = data.frame(Site = 'EU', Lat = 44.05, Long = -123.07, Elev = 150,
                 TZ = -8)

params = c(0.51, 6.08, 0.64, 3)
names(params) = c('a', 'b', 'c', 'TL')

fit = clear_sky('Ineichen', x = site, y = loc,
                parameters = params[1:4])
f = fit$predicted

parametertable = tableGrob(data.frame(params), cols = 'Parameters')

png('Ineichenmodeldefaults.png')
qplot(1:length(f), f, geom = 'line', color = f,
      ylab = 'Irradiance', xlab = 'Minutes',
      main = 'Ineichen-Perez Clear Sky Model') +
guides(color = FALSE) +
annotation_custom(grob = parametertable,
                  xmin = 1100, xmax = 1500,
                  ymin = 200, ymax = 400)
dev.off()

