log_posterior <- function(theta, y, z, phi, a0, b0, c0, v, eta) {
    ## Log posterior, up to a constant of proportionality, for RS model

    ss = theta[['ss']]
    alpha = theta[['a']]
    beta = theta[['b']]
    gamma = theta[['c']]

    if (ss <= 0)
        return(NA)

    n = length(y)
    pp = 1 / (2 * phi^2)

    fpart = -1 * (v + (n/2) + 1) * log(ss)
    spart = (1/ss) * (eta + (1/2)*sum((y - z)^2))
    tpart = -pp * ((alpha - a0)^2 + (beta - b0)^2 + (gamma - c0)^2)

    logpost = fpart + spart + tpart
    return(logpost)
}

log_posterior_aux <- function(theta, y, z, phi, a0, b0, c0, v, eta) {
    ## Log posterior, up to constant of proportionality, using auxilary variable
    ## w = log(sigma^2)

    w = theta[['w']]
    alpha = theta[['a']]
    beta = theta[['b']]
    gamma = theta[['c']]

    n = length(y)
    pp = 1 / (2 * phi^2)

    fpart = -1 * pp * ((alpha - a0)^2 + (beta - b0)^2 + (gamma - c0)^2)
    spart = w * (-v - (n/2))
    tpart = (-1 / exp(w)) * (eta + (1/2) * sum((y - z)^2))
    logpost = fpart + spart + tpart
    return(logpost)
}


rconditionalss <- function(n, y, z, v, eta, ...) {
    ## Draw from the conditional of sigma^2

    number_draws = n
    n = length(y)
    alpha = (n/2) + v
    beta = eta + (1/2) * sum((y - z)^2)

    rinvgamma(n = number_draws, shape = alpha, scale = beta)
}

init <- function() {
    ## Initialize parameter vector theta

    alpha = runif(n = 1L, 800, 1400)
    beta = runif(n = 1L, 0, 3)
    gamma = runif(n = 1L, 0, 1)

    theta = c(alpha, beta, gamma, 1)
    names(theta) = c('a', 'b', 'c', 'ss')
    return(theta)
}

init_aux <- function() {
    ## Initialize parameter vector theta with auxiliary variable
    ## w = log(sigma^2)

    alpha = runif(n = 1L, 800, 1400)
    beta = runif(n = 1L, 0, 3)
    gamma = runif(n = 1L, 0, 1)
    w = runif(n = 1L, -1, 1)

    theta = c(alpha, beta, gamma, w)
    names(theta) = c('a', 'b', 'c', 'w')
    return(theta)
}

