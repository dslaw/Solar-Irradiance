## Metropolis sampler
##
## Model:
## y_t | theta ~ N(z_t, ss)
## z_t | alpha, beta, gamma ~ RS(alpha, beta, gamma)
##
## Priors:
## alpha ~ N(a0, phi^2)
## beta ~ N(b0, phi^2)
## gamma ~ N(c0, phi^2)
## ss ~ Inverse Gamma(v, eta)

require(clearskies)
require(compiler); enableJIT(3)
require(data.table)
require(parallel)

source('../data/impute.R')
source('mcmc.R')
source('posterior.R')

## Load training data and site parameters
# For a single year at Eugene
site = "EU"

#dat = readRDS('../data/Eugene.rds')
#
### Impute missing / negative values if possible and remove entire day if not
#ghi_col = which(colnames(dat) %in% "Hour") + 1L
#colnames(dat)[ghi_col] = "Ghi"
#
#num_days = nrow(dat) / 1440 # 1440 points in a day for interval = 1
#dat = impute(x = dat, which.col = ghi_col)
#dat = rm_day(dat[, 1:ghi_col]) # drops additional data columns
#num_days_removed = num_days - (nrow(dat) / 1440)
#
#saveRDS(dat, file = 'eugene_imputed.rds')

dat = readRDS('eugene_imputed.rds')
y = data.table(dat)
setkey(y, Year, DayOfYear)
dat = y[ list(2014, 1:11) ]

# Time and site information
loc = locations[locations[['Site']] == site, ]
time_info = unique(dat)

dat = data.frame(dat)

## Hyperpriors
# Normals for RS parameters centered at defaults
a0 = 1159.24
b0 = 1.179
c0 = -0.0019
phi = 10 # phi^2 = 100
# Non-informative inverse gamma
v = 0.001
eta = 0.001

## Metropolis sampler with multiple chains in parallel
ghi = dat[['Ghi']]

set.seed(131)
n.iter = 2000L
periodicity = 100L
stop.at = floor(n.iter / 2)
n.chains = 2L
d = 4L
theta = lapply(seq_len(n.chains), function(i) init_aux())
nu = lapply(seq_len(n.chains), function(i) {
            round(runif(n = 1L, 5, 10), 2)
            })

startt = Sys.time()
set.seed(131)
collect = mclapply(seq_len(n.chains), function(chain) {
                   sink(paste0("chain", chain, ".txt"))
                   theta0 = theta[[chain]]
                   nu0 = nu[[chain]]
                   nu0 = diag(nu0, nrow = length(theta0))
                   res = adaptive_met(theta = theta0, y = ghi,
                                      sigma = nu0, target = log_posterior_aux,
                                      n.samples = n.iter, periodicity = periodicity,
                                      stop.adapt = stop.at, model = 'RS',
                                      time.info = time_info, location.info = loc,
                                      thresholds = thresholds, window.len = 10L,
                                      a0 = a0, b0 = b0, c0 = c0, phi = phi,
                                      v = v, eta = eta,
                                      verbose = TRUE)
                   sink(NULL)
                   return(res)
                   }, mc.set.seed = TRUE, mc.cores = min(n.chains, detectCores()))
endt = Sys.time()

saveRDS(collect, file = "collect.rds")
saveRDS(theta, file = "theta.rds")
saveRDS(nu, file = "nu.rds")

lapply(collect, function(x) mean(x$accepted))

