## MCMC Diagnostics

require(ggplot2)
require(gridExtra)
require(reshape2)
require(MCMCpack)

runningmean <- function(x) cumsum(x) / seq_along(x)

makeggdf <- function(x) {

    if (length(x) < 2)
        stop()

    y = do.call('cbind', x)
    colnames(y) = 1:ncol(y)
    y = melt(y)
    colnames(y) = c('Iteration', 'Chain', 'value')
    y[['Chain']] = factor(y[['Chain']])
    return(y)
}

mcmcplot <- function(x, param) {
    ## Trace and marginal density plots

    traceplot <- ggplot(x, aes(x = Iteration, y = value, color = Chain)) +
                   theme_light() +
                   theme(legend.position = 'none') + # remove legend
                   scale_colour_brewer(palette = 'Dark2') +
                   geom_line(alpha = 0.65) +
                   geom_smooth(se = FALSE, linetype = 2) +
                   ggtitle(param)

    densplot <- ggplot(x, aes(x = value)) +
                  theme_light() +
                  scale_colour_brewer(palette = 'Dark2') +
                  geom_density(colour = 'seagreen') +
                  ggtitle(param)

    list(traceplot, densplot)
}

collect = readRDS("collect.rds")
theta = readRDS("theta.rds")
nu = readRDS("nu.rds")

samples = lapply(collect, function(i) i$samples)
chains = mcmc.list(lapply(samples, function(i) mcmc(i)))

# Either determine burn-in graphically or discard first half
params = colnames(samples[[1]])
for (param in params) {
    fname = paste0('diagnostics_', param, '.pdf')

    param_chains = lapply(chains, function(chain) chain[, param])

    # Trace plots
    pp = mcmcplot(makeggdf(param_chains), param)

    # Running mean plots
    running_means = lapply(param_chains, runningmean)
    running_means = makeggdf(running_means)
    colnames(running_means)[3] = 'Mean'

    pp[[3]] = ggplot(running_means, aes(x = Iteration, y = Mean,
                color = Chain, linetype = Chain)) +
                theme_light() +
                theme(legend.position = 'none') +
                geom_line() +
                scale_colour_brewer(palette = 'Dark2') +
                ggtitle(paste0('Running mean: ', param))

    ggsave(fname,
           do.call('marrangeGrob', c(pp, list(nrow = 2, ncol = 2, top = NULL)))
           )
    dev.off()
}

system('pdfjoin diagnostics_*.pdf &&
        mv diagnostics_*-joined.pdf mcmcdiagnostics.pdf')

burn_in_default = floor(nrow(samples[[1]]) / 2)
burn_in = burn_in_default

# Remove burn-in and check overall convergence using scale reduction factor
chains = lapply(samples, function(x) mcmc(x[(burn_in+1):nrow(x), ]))
chains = mcmc.list(chains)

gelman.plot(chains)
gelman.diag(chains) # want < 1.1 (approx)

# Check correlation
chain = do.call('rbind', chains)
chain = mcmc(chain)

pairs(as.data.frame(chain), pch = '.', col = 'darkblue')
crosscorr.plot(mcmc(chain))
autocorr.plot(chain, lag.max = 100, col = 'darkblue')

# Trace plots (post burn-in)
plot(chain, col = 'darkblue')

# Check acceptance rate: ideal rate is ~0.234
acceptance_rates = vapply(collect, function(i) mean(i$accepted), numeric(1))
overall_acceptance_rate = mean(acceptance_rates)
cat('Acceptance rate:', overall_acceptance_rate, '\n')

effectiveSize(chain)

# Summary statistics and credible intervals
quantiles = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)
posterior_summary = summary(chain, quantiles = quantiles)
cred_intervals = cbind(posterior_summary$quantiles[, '2.5%'],
                       posterior_summary$statistics[, 'Mean'],
                       posterior_summary$quantiles[, '97.5%'])
colnames(cred_intervals) = c('2.5%', 'Mean', '97.5%')

