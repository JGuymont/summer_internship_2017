load('./data/data.Rdata')
load('./data/temperatures.Rdata')
load('./data/db_prcp_mai.Rdata')

source('yield_model.R')
source('temperature_model.R')
source('functions.R')

# hyperparameters
NUM_SIM = 10
SIM_SIZE = 1000
TRAIN_SIZE = 15
START_YEAR = 16

run_simulation = function(start_year, train_size){
    for (end_year in start_year:49){

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
        yield_model = fit_corn_yield(train_annual_data)
        yld_params = yield_model$yld_params
        yld_sd = yield_model$yld_sd

        # temperature model
        seasonal_trend = fit_seasonal_trend(3, length(training_days), train_temp_data, dat)
        delta_temp = seasonal_trend$delta_temp

        garch_model = fit_garch_model(delta_temp, training_days)
        garch_params = garch_model$garch_params
        sdhat = garch_model$sdhat

        
        stop()
    }
}

run_simulation(START_YEAR, TRAIN_SIZE)
