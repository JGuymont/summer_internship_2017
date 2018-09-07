fit_corn_yield = function(annual_data){
    "Fit the logarithm of the corn yields with a simple linear model"
	linear_model = lm(lyield ~ t + AccRain5 + AccHDD9 + AccCDD + sqAccCDD, data=annual_data)
	yld_params = coef(linear_model)
	yld_sd = summary(linear_model)$sigma 

	return(list(yld_params=yld_params, yld_sd=yld_sd))
}