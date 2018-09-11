

source('simulate_temperature_path.R')

option_expected_payoff = function(x, K, option_type){ 
    if (option_type=='call')
        return(mean(pmax(x-K,0)))
    if (option_type=='put')
        return(mean(pmax(K-x,0)))
}

compute_expected_payoff = function(num_sim, rain_params, args) {
    "Estimate price of derivative
    
    Arguments
        num_sim: (int) number of simulation
    "
    months = args$dat$Month[1:365]

    simulate_temp = function(i) {

        simulate_temperature_path(i, train_size=args$train_size, 
                                     num_lag=args$num_lag, 
                                     fourier_order=args$fourier_order,
                                     sdhat=args$sdhat, 
                                     delta_temp=args$delta_temp, 
                                     s1=args$s1, 
                                     s2=args$s2, 
                                     garch_params=args$garch_params, 
                                     dat=args$dat$Mean)
    }
    

    # simulate daily average temperature
    sim_dat = cbind(months, sapply(1:num_sim, simulate_temp))
    
    # simulate cumulative index
    AccCDD  = colSums(pmax(sim_dat[sim_dat[, 1] %in% c(6,7,8), -c(1)]-18, 0))
    AccHDD  = colSums(pmax(18-sim_dat[sim_dat[, 1]==9, -c(1)], 0))
    AccRain = rgamma(num_sim, shape=rain_models$shape_qc, scale=rain_models$scale_qc)

    expected_payoff = list()
    
    ep_call_cdd     = sapply(seq(0, 500, 1), function(x) option_expected_payoff(AccCDD, x, 'call'))
    expected_payoff$price_call_cdd  = cbind(K=seq(0, 500, 1), ep_call_cdd)
    
    ep_put_cdd = sapply(seq(0, 500, 1), function(x) option_expected_payoff(AccCDD, x, 'put'))
    expected_payoff$price_put_cdd   = cbind(K=seq(0, 500, 1), ep_put_cdd)
    
    ep_call_hdd = sapply(seq(0, 200, 1), function(x) option_expected_payoff(AccHDD, x, 'call'))
    expected_payoff$price_call_hdd  = cbind(K=seq(0, 200, 1), ep_call_hdd)
    
    ep_call_rain = sapply(seq(0, 250, 1), function(x) option_expected_payoff(AccRain, x, 'call'))
    expected_payoff$price_call_rain = cbind(K=seq(0, 250, 1), ep_call_rain)

    return(expected_payoff)
}
