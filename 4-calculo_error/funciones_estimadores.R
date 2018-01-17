# functions to compute estimates and standard errors

### raio_est
# computes estimate and standard error of y/M, 
# used for estimates per strata (not overall!)

## Arguments
# M: denominator in ratio, (eg. vector of polygon areas)
# y: numerator in ratio (eg. binary vector 1 correctly classified 0 ow)
# N: number of polygons (in the strata)
ratio_est <- function(M, y, N = Inf){
    n <- length(M)
    M0_hat <- sum(M)
    M_bar <- mean(M)
    p_hat <- sum(M * y) / M0_hat
    s2_p_hat <- ifelse(n > 1, 
        1 / (M_bar ^ 2) * 1 / (n - 1) * sum((M * (y - p_hat)) ^ 2), 
        0)
    var_p_hat <- s2_p_hat / n * (1 - n / N) 
    return(data_frame(p_hat = p_hat, se_p_hat = sqrt(var_p_hat), 
        s2_p_hat = s2_p_hat, n = n))
}


### strat_ratio_est
# computes overall p_hat and se(p_hat), when p_strata is a ratio estimator

## Arguments
# p_h: vector of ratio estimators per strata (as returned by ratio_est)
# n_h: vector of number of polygons sampled per strata
# s2_h: estimate of s^2 per strata (as returned by ratio_est)
# N_h: vector of  number of polygons per strata (population)
# M_h: vector of strata areas

strat_est <- function(p_h, n_h, N_h, M_h, s2_h){
    M0 <- sum(M_h) # total area
    p_hat <- sum(M_h / M0 * p_h)
    se_p_hat <- sqrt(sum((1 - n_h / N_h) * (M_h / M0) ^ 2 * s2_h / n_h))
    return(data.frame(p_hat = p_hat, se_p_hat = se_p_hat))
}


mse_ratio <- function(M, y, n){
    N <- length(M)
    w <- M * y
    Sx <- sd(M)
    Sy <- sd(w)
    B <- sum(w) / sum(M)
    R <- cor(w, M)
    (1 - n / N) * (Sy ^ 2 - 2 * B * R * Sx * Sy + B ^ 2 * Sx ^ 2) / (n * mean(M) ^ 2)
}

#### ignorando área y tomando % de polígonos clasificados correctos

