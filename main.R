load('./data/data.Rdata')
load('./data/temperatures.Rdata')
load('./data/db_prcp_mai.Rdata')

source('yield_model.R')
source('temperature_model.R')
source('expected_payoff.R')
source('hedging.R')
source('rain_copula.R')
source('functions.R')

rain_models = readRDS('./data/rain_models.rds')

# hyperparameters
NUM_SIM = 10
SIM_SIZE = 50
TRAIN_SIZE = 15
START_YEAR = 16

run_simulation = function(start_year, train_size){

    M1 = 3
    M2 = 3
    P = 3

    for (end_year in start_year:49) {

        # in-sample years 
        training_years = (end_year - train_size + 1):end_year 
        
        # in-sample days 
        training_days  = ((end_year - train_size)*365 + 1):(end_year*365) 
        
        # out-sample years
        test_year = end_year + 1
        
        # out-sample days
        test_days = (end_year*365 + 1):((end_year + 1)*365)

        # out of sample years are removed from the data
        train_annual_data = annual_data[training_years,]
        train_temp_data = temperatures[training_days,] 
        dat = train_temp_data$Mean # daily Average temperature
        train_db_prcp_mai = db_prcp_mai[training_years,]

        # yield model
        cat(' [*] Fitting corn yield model...')
        yield_model = fit_corn_yield(train_annual_data)
        yld_params = yield_model$yld_params
        yld_sd = yield_model$yld_sd
        cat('done\n')

        # temperature model
        cat(' [*] Fitting temperature model...')
        seasonal_trend_model = fit_seasonal_trend(M1, length(training_days), train_temp_data, dat)
        seasonal_trend = seasonal_trend_model$fitted
        delta_temp = seasonal_trend_model$delta_temp

        garch_model = fit_garch_model(delta_temp, training_days, fourier_size=M1, num_lag=P)
        garch_params = garch_model$garch_params
        fourier_params = garch_params[(P+5):(M2*2+P+4),1]
        sdhat = garch_model$sdhat
        cat('done\n')

        # the seasonal trend is deterministic. We used the last year of 
        # data available because of the global warming trend.
        s1 = matrix(cbind(1, test_days, Fourier(M1, 365)), nr=365, nc=8) %*% matrix(seasonal_trend_model$coef, nr=8, nc=1) 

        # The seasonal trend of the volatility (deterministic trend)
        s2 = colSums(fourier_params * t(Fourier(M2, length(training_days))))

        args = list(
            train_size=train_size, 
            num_lag=P, 
            fourier_order=M2,
            sdhat=sdhat, 
            delta_temp=delta_temp, 
            s1=s1, 
            s2=s2, 
            garch_params=garch_params, 
            dat=train_temp_data
        )

        cat(' [*] Computing derivatives expected payoffs...')
        expected_payoff = compute_expected_payoff(num_sim=10, rain_models, args)
        cat('done\n')

        cat(' [*] fitting rain copula...')
        copula_dist = fit_copula_dist(rain_models)
        cat('done\n')

        cl_objects = list(
            train_size=train_size,
            train_temp_data=train_temp_data, 
            training_years=training_years, 
            training_days=training_days, 
            test_year=test_year, 
            test_days=test_days,
            yld_params=yld_params, 
            yld_sd=yld_sd, 
            P=P, 
            M2=M2, 
            garch_params=garch_params, 
            sdhat=sdhat, 
            NUM_SIM=NUM_SIM, 
            SIM_SIZE=SIM_SIZE,
            s1=s1, 
            s2=s2, 
            seasonal_trend=seasonal_trend, 
            delta_temp=delta_temp,
            copula_dist=copula_dist, 
            cost=cost, 
            tvar=tvar, 
            svar=svar, 
            expected_payoff=expected_payoff,
            dat=dat,
            simulate_temperature_path=simulate_temperature_path
        )
        
        cat(' [*] Finding best hedging strategy...')
        opt_params = find_hedging_stratedy(cl_objects)
        cat('done')

        stop()


    }
}

run_simulation(START_YEAR, TRAIN_SIZE)
