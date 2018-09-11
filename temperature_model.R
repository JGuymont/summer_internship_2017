library(rugarch)

Fourier = function(M, l){
    "
    Function description:
        computes element of a Fourier series
    
    Arguments
        M: number of sin and cos terms
        l: number of data
    "
    t = 1:l 
    if (M == 0){return(0)}
    if (M > 0){
        X = matrix(nr=l,nc=2*M)
        for (m in 1:M){
            X[,m] = cos(2*pi*t*m/365)
            X[,M+m] = sin(2*pi*t*m/365)
        }
    }
    return(X)
}

compute_bic = function(par, res, t){
    # compute_bic find the Bayesian information criterion 
    # for each combinations (M2, P) in order to find the 
    # best orders for the AR(P) component and the mean of the GARCH
    # (omega + seasonal_trend2). All parameters are fitted 
    # at the same time using Gaussian quasi-maximum likelihood.
    l = length(t)
    M2 = par[1]
    P = par[2]
    cos.mtx = matrix(nr=l, nc=M2)
    sin.mtx = matrix(nr=l, nc=M2)
    X = matrix(nr=l, nc=2*M2)
    for (m in 1:M2){
        X[, m] = cos(2*pi*t*m/365)
        X[, M2+m] = sin(2*pi*t*m/365)
    }
    spec = rugarch::ugarchspec(
        variance.model = list(model='eGARCH',external.regressors = X),
        distribution.model = "snorm",
        mean.model = list(armaOrder=c(P, 0), include.mean=FALSE),
        start.pars = list(omega=1.1, alpha1=0.1,beta1=0.5)
    )
    egarchfit = rugarch::ugarchfit(spec=spec, data=res)
    k = length(coef(egarchfit))
    bic = k*log(l)-2*likelihood(egarchfit)
    return(bic)
}

best_garch_order = function(maxM2, maxP, delta_temp, training_days){
    eval_bic = function(par) compute_bic(par, delta_temp, training_days) 
    order_comb = expand.grid(M2=1:maxM2, P=1:maxP) 
    order_mean = matrix(apply(order_comb, 1, eval_bic), nr=maxM2, nc=maxP) 
    order_mean = as.numeric(which(order_mean==min(order_mean), arr.ind=TRUE)) 
}

fit_seasonal_trend = function(fourier_size, series_length, temperatures_data, dat){
	# return fitted values of the temperatures seasonal trend 
	fourier_terms = Fourier(fourier_size, series_length)
	seasonal_trend_model = lm(Mean ~ t + fourier_terms, data=temperatures_data) 
	seasonal_trend = fitted(seasonal_trend_model)
	delta_temp = dat - seasonal_trend

	return(list(coef=seasonal_trend_model$coefficients, delta_temp=delta_temp, fitted=seasonal_trend))
}

fit_garch_model = function(delta_temp, training_days, fourier_size=NA, num_lag=NA){
	"Fit temperatures volatility"

    if (is.na(num_lag) | is.na(num_lag)) {
        # best hyperparameters for the mean 
	    # of the garch model
	    garch_order = best_garch_order(maxM2=6, maxP=4, delta_temp, training_days)

	    # order of the fourier series
	    fourier_size = garch_order[1]
	
	    # arma order for the mean of the residual
	    num_lag = garch_order[2]
    }
	

	spec = rugarch::ugarchspec(
  		variance.model = list(model='eGARCH', external.regressors=Fourier(fourier_size, length(training_days))), 
  		distribution.model = "snorm",
  		mean.model = list(armaOrder = c(num_lag, 0), include.mean=FALSE),
  		start.pars = list(omega=1.1, alpha1=0.1, beta1=0.5)
	)

	egarchfit = rugarch::ugarchfit(spec=spec, data=delta_temp) 
	garch_params = data.frame(coef(egarchfit)) 
    sdhat = as.numeric(sigma(egarchfit))

	return(list(garch_params=garch_params, sdhat=sdhat))
}