mvrnorm <- MASS::mvrnorm
rinvgamma <- MCMCpack::rinvgamma

is.malformed <- function(z) {
    ## Check for points that are NA, Inf, etc

    is.na(z) | is.infinite(z) | !is.numeric(z) | is.nan(z)
}

# always for log posterior
adaptive_met <- function(theta, y, sigma, target,
                         n.samples, periodicity, stop.adapt,
                         model, time.info, location.info,
                         thresholds, window.len,
                         ..., verbose = TRUE) {
    ## Parameters:
    ## theta: named vector of parameters
    ## y: vector of observed GHI values
    ## sigma: covariance matrix of proposal distribution
    ## target: function for the log posterior
    ## n.samples: number of samples to take
    ## periodicity: number of iterations between adaptations. Set equal to
    ## n.samples to not use the non-adaptive Metropolis algorithm.
    ## stop.adapt: number of iterations to at which to stop adapting.
    ## model: Name of the clear sky model to use.
    ## time.info: Time parameters for the clear sky model. See help(clear_sky).
    ## location.info: Location parameters for the clear sky model. See help(clear_sky).
    ## thresholds: Thresholds for detecting clear points. See
    ## help(clear_points).
    ## window.len: Length of rolling window used in detecting clear points. See
    ## help(clear_points).
    ## ...: Additional arguments to be passed to target.
    ## verbose: Print progress?

    if (any(is.malformed(y)))
        stop('y must be a vector of clean, finite numeric values')

    if (stop.adapt > n.samples)
        stop('stop.adapt must be less than or equal to n.samples')

    if (!is.vector(theta) || !is.numeric(theta))
        stop('theta must be a numeric vector')

    sigma = as.matrix(sigma)

    # Adaptation constants
    d = length(theta)
    eps = 0.01
    eye = diag(d)
    s = (2.38 ** 2) / d

    # Storage
    samples = matrix(NA, nrow = n.samples, ncol = d)
    colnames(samples) = names(theta)
    accepted = logical(n.samples)

    # Initalize
    z = clear_sky(model, x = time.info, y = location.info, data = y,
                  parameters = theta)

    if (any(is.malformed(z$predicted)))
        stop('Invalid starting parameters')

    z = clear_points(z, thresholds = thresholds, window_len = window.len)
    clear = z$clear

    obs_clear = y[clear]
    pred_clear = z$predicted[clear]

    for (i in seq_len(n.samples)) {

        # Adapat covariance matrix of proposal distribution
        # Reference: Gelman, Pasarica 2010 proposition 4, pp. 9
        if (i <= stop.adapt && i %% periodicity == 0) {

            # debugging
            cat('Adapting covariance matrix at iteration:', i, '\n')

            sigma = s * cov(samples[1:(i-1), ]) + s * eps * eye
        }

        draws = mvrnorm(n = 1L, mu = theta, Sigma = sigma)

        # debugging
        cat('Draws:', draws, '\n')

        # Refit model and redetect clear times for proposed values
        z_proposed = clear_sky(model, x = time.info, y = location.info,
                               data = y, parameters = draws)
        pred_proposed = z_proposed$predicted

        # debugging
        summary(z_proposed)

        # Check that model is valid
        if (any(is.malformed(pred_proposed))) {

            # debugging
            cat('Proposal rejected for invalid model\n')
            cat('Parameters:', theta, '\n')

            reject = TRUE
        } else {
            clear_proposed = clear_points(z_proposed, thresholds = thresholds,
                                          window_len = window.len)
            clear_proposed = clear_proposed$clear

            # debugging
            cat('Number of clear (proposed) points:', sum(clear_proposed), '\n')

            obs_proposed = y[clear_proposed]
            pred_proposed = pred_proposed[clear_proposed]

            r = target(theta = draws, y = obs_proposed, z = pred_proposed, ...) -
                target(theta = theta, y = obs_clear, z = pred_clear, ...)
            u = log(runif(n = 1L))

            # debugging
            cat('log ratio is:', r, '\n')

            reject = if (!is.nan(r) && !is.infinite(r) && !is.na(r) && u <= r) FALSE else TRUE
        }

        if (!reject) {
            accepted[i] = TRUE
            theta = draws
            obs_clear = obs_proposed
            pred_clear = pred_proposed
        }

        samples[i, ] = theta

        if (verbose && i %% 1000 == 0)
            cat('Iteration:', i, '\n')
    }

    return(list(samples = samples, accepted = accepted))
}

adaptive_met_gibbs <- function(theta, y, sigma, target,
                               n.samples, periodicity, stop.adapt,
                               model, time.info, location.info,
                               thresholds, window.len,
                               ..., verbose = TRUE) {
    ## Parameters:
    ## theta: named vector of parameters
    ## y: vector of observed GHI values
    ## sigma: covariance matrix of proposal distribution
    ## target: function for the log posterior
    ## n.samples: number of samples to take
    ## periodicity: number of iterations between adaptations. Set equal to
    ## n.samples to not use the non-adaptive Metropolis algorithm.
    ## stop.adapt: number of iterations to at which to stop adapting.
    ## model: Name of the clear sky model to use.
    ## time.info: Time parameters for the clear sky model. See help(clear_sky).
    ## location.info: Location parameters for the clear sky model. See help(clear_sky).
    ## thresholds: Thresholds for detecting clear points. See
    ## help(clear_points).
    ## window.len: Length of rolling window used in detecting clear points. See
    ## help(clear_points).
    ## ...: Additional arguments to be passed to target.
    ## verbose: Print progress?

    if (any(is.malformed(y)))
        stop('y must be a vector of clean, finite numeric values')

    if (stop.adapt > n.samples)
        stop('stop.adapt must be less than or equal to n.samples')

    if (!is.vector(theta) || !is.numeric(theta))
        stop('theta must be a numeric vector')

    sigma = as.matrix(sigma)

    # Adaptation constants
    d = length(theta) - 1
    eps = 0.01
    eye = diag(d)
    s = (2.38 ** 2) / d

    # Storage
    samples = matrix(NA, nrow = n.samples, ncol = length(theta))
    colnames(samples) = names(theta)
    accepted = logical(n.samples)

    # Initalize
    z = clear_sky(model, x = time.info, y = location.info, data = y,
                  parameters = theta)
    pred = z$predicted

    if (any(is.malformed(pred)))
        stop('Invalid starting parameters')

    z = clear_points(z, thresholds = thresholds, window_len = window.len)
    clear = z$clear

    obs_clear = y[clear]
    pred_clear = pred[clear]

    for (i in seq_len(n.samples)) {

        # Adapt covariance matrix of proposal distribution
        # Reference: Gelman, Pasarica 2010 proposition 4, pp. 9
        if (i <= stop.adapt && i %% periodicity == 0) {

            # debugging
            cat('Adapting covariance matrix at iteration:', i, '\n')

            sigma = s * cov(samples[1:(i-1), c('a', 'b', 'c')]) + s * eps * eye
        }

        draws = mvrnorm(n = 1L, mu = theta[c('a', 'b', 'c')], Sigma = sigma)

        # Refit model and redetect clear times for proposed values
        z_proposed = clear_sky(model, x = time.info, y = location.info,
                               data = y, parameters = draws)
        pred_proposed = z_proposed$predicted

        # Check that model is valid
        if (any(is.malformed(pred_proposed))) {

            # debugging
            cat('Proposal rejected for invalid model\n')
            cat('Parameters:', theta, '\n')

            reject = TRUE
        } else {
            clear_proposed = clear_points(z_proposed, thresholds = thresholds,
                                          window_len = window.len)
            clear_proposed = clear_proposed$clear

            # debugging
            cat('Number of clear (proposed) points:', sum(clear_proposed), '\n')

            obs_proposed = y[clear_proposed]
            pred_proposed = pred_proposed[clear_proposed]

            draws[['ss']] = theta[['ss']]

            r = target(theta = draws, y = obs_proposed, z = pred_proposed, ...) -
                target(theta = theta, y = obs_clear, z = pred_clear, ...)
            u = log(runif(n = 1L))

            # debugging
            cat('log ratio is:', r, '\n')

            reject = if (!is.nan(r) && !is.infinite(r) && u <= r) FALSE else TRUE
        }

        if (!reject) {
            accepted[i] = TRUE
            theta[c('a', 'b', 'c')] = draws[c('a', 'b', 'c')]
            obs_clear = obs_proposed
            pred_clear = pred_proposed
        }

        # Draw from conditional of sigma^2
        ssdraw = rconditionalss(n = 1L, y = obs_clear, z = pred_clear, ...)
        theta[['ss']] = ssdraw

        samples[i, ] = theta

        if (verbose && i %% 1000 == 0)
            cat('Iteration:', i, '\n')
    }

    return(list(samples = samples, accepted = accepted))
}

