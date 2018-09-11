library('fGarch')

simulate_temperature_path = function(i, train_size, num_lag, fourier_order, sdhat, 
                                        delta_temp, s1, s2, garch_params, dat) {
    "Simulate a temperature path based on the 
    fitted garch model

    Arguments

    "
    # garch volatility model parameters
    skew = garch_params['skew',1] 
    omega = garch_params['omega',1] 
    alpha = garch_params['alpha1',1] 
    beta  = garch_params['beta1',1]
    gamma = garch_params['gamma1',1]
    rho = matrix(garch_params[1:num_lag, 1], nr=1, nc=num_lag)

    # E[|z|], z ~ skNormal(0,1, xi). E[|z|] is the expected value of eGARCH residuals. 
    # It is a parameter needed in the eGARCH model. Even if the 
    # distribution of z is skewed, the mean remaind the same
    # as a normal.
    Ez = sqrt(2/pi)
    
    # index of initial values of the year to forecast
    lag = ((train_size-1)*365 + 1):((train_size-1)*365+num_lag)
    ar = vector(length=365) 

    lsigma2 = vector(length = 365)
    lsigma2[1] = log(sdhat[365*train_size]^2)
    
    sim_delta_temp = matrix(nr=365,nc=1)
    sim_delta_temp[1:num_lag] = delta_temp[lag]
    
    sim_temp = vector(length = 365)
    sim_temp[1:num_lag] = dat[lag]
    
    z = fGarch::rsnorm(365, xi=skew)
    
    for (i in (num_lag+1):365){
        ar[i] = rho %*% sim_delta_temp[(i-1):(i-num_lag)]
        lsigma2[i] = s2[i] + omega + alpha*z[i-1] + gamma*(abs(z[i-1])-Ez) + beta*lsigma2[i-1]
        sim_temp[i] = s1[i] + ar[i] + exp(lsigma2[i]/2)*z[i]
        sim_delta_temp[i] = sim_temp[i] - s1[i]
    }
    return(sim_temp)
}