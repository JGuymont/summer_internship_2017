library(parallel)

source('simulate_temperature_path.R')

find_hedging_stratedy = function(args){

    simulate_temp = function(i) {

        sim_dat = simulate_temperature_path(i,  train_size=args$train_size, 
                                                num_lag=args$P, 
                                                fourier_order=args$M2,
                                                sdhat=args$sdhat, 
                                                delta_temp=args$delta_temp, 
                                                s1=args$s1, 
                                                s2=args$s2, 
                                                garch_params=args$garch_params, 
                                                dat=args$dat)
        return(sim_dat)    
    }

    varlist = names(args)

    hedging_env = new.env()

    for (i in 1:length(args)) {
        assign(varlist[i], args[[i]], envir=hedging_env)
    }

    assign('simulate_temp', simulate_temp, envir=hedging_env)

    # Initialize a parallel cluster
    Sys.sleep(1); cat("\r", "      +Creating cluster...")
    cluster = parallel::makeCluster(detectCores()-1, type='PSOCK')

    Sys.sleep(1); cat("\r", "      +Initializing cluster...")
    parallel::clusterExport(cluster, varlist=varlist, envir=hedging_env)

    

    # each columns of 'opt_params' is a vector of parameters (opt_params have num_sim columns).
    Sys.sleep(1); cat("\r", "   +Searching for optimal strategy...")
    opt_params = parSapply(cluster, 1:NUM_SIM, function(i) {
        #library(fGarch)
        library(copula)
        sim_temp = sapply(1:SIM_SIZE, simulate_temp)
        months = train_temp_data$Month[1:365]
        sim_dat  = cbind(months, sim_temp)
        AccCDD   = colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8), -c(1)]-18, 0))
        AccHDD   = colSums(pmax(18-sim_dat[sim_dat[,1]==9, -c(1)], 0))
        AccRain  = copula::rMvdc(SIM_SIZE, copula_dist) # AccRain[1,]=QC, AccRain[2,]=Chicago
        
        epsilon = rnorm(SIM_SIZE, mean=0, sd=yld_sd)
        design = matrix(cbind(1, training_years+1, AccRain[,1], AccHDD, AccCDD, AccCDD^2), nrow=SIM_SIZE, ncol=6)
        B = matrix(yld_params, nrow=6, ncol=1)
        R = exp(as.vector(design %*% B) + epsilon)
        
        quantity = c(
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0)
        )

        strike = c(
            runif(1, 4, max=5), 
            runif(1, 5, max=6), 
            runif(1, 4, max=5), 
            runif(1, 5, max=6))

        par0 = cbind(quantity, strike)

        COST = function(par0) cost(par0, R, AccRain, AccCDD, AccHDD, expected_payoff, 'svar', 0.10)
        COST(par0)
        hedge = optim(par0, COST)
        opt_params = exp(hedge$par)
        return(opt_params)

    })

    # close cluster
    if (!is.null(cluster)) {
        stopCluster(cluster)
        cluster = c()
    }

    return(opt_params)
    
}

find_hedging_stratedy_ = function(
            train_size,
            train_temp_data, 
            training_years, 
            training_days, 
            test_year, 
            test_days,
            yld_params, 
            yld_sd, 
            P, 
            M2, 
            garch_params, 
            sdhat, 
            NUM_SIM, 
            SIM_SIZE,
            s1, 
            s2, 
            seasonal_trend, 
            delta_temp,
            copula_dist, 
            cost, 
            tvar, 
            svar, 
            expected_payoff,
            dat,
            simulate_temperature_path){

    simulate_temp = function(i) {

        sim_dat = simulate_temperature_path(i,  train_size=train_size, 
                                                num_lag=P, 
                                                fourier_order=M2,
                                                sdhat=sdhat, 
                                                delta_temp=delta_temp, 
                                                s1=s1, 
                                                s2=s2, 
                                                garch_params=garch_params, 
                                                dat=dat)
        return(sim_dat)    
    }

    

    opt_params = sapply(1:NUM_SIM, function(i) {
        
        sim_temp = sapply(1:SIM_SIZE, simulate_temp)
        
        months = train_temp_data$Month[1:365]
        sim_dat  = cbind(months, sim_temp)
        AccCDD   = colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8), -c(1)]-18, 0))
        AccHDD   = colSums(pmax(18-sim_dat[sim_dat[,1]==9, -c(1)], 0))
        AccRain  = rMvdc(SIM_SIZE, copula_dist) # AccRain[1,]=QC, AccRain[2,]=Chicago
        
        epsilon = rnorm(SIM_SIZE, mean=0, sd=yld_sd)
        design = matrix(cbind(1, training_years+1, AccRain[,1], AccHDD, AccCDD, AccCDD^2), nrow=SIM_SIZE, ncol=6)
        B = matrix(yld_params, nrow=6, ncol=1)
        R = exp(as.vector(design %*% B) + epsilon)
        
        quantity = c(
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0),
            runif(1, min=-1, max=0)
        )

        strike = c(
            runif(1, 4, max=5), 
            runif(1, 5, max=6), 
            runif(1, 4, max=5), 
            runif(1, 5, max=6))

        par0 = cbind(quantity, strike)

        COST = function(par0) cost(par0, R, AccRain, AccCDD, AccHDD, expected_payoff, 'svar', 0.10)
        COST(par0)
        hedge = optim(par0, COST)
        opt_params = exp(hedge$par)
        return(opt_params)

    })

    return(opt_params)
    
}

