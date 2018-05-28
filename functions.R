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
    return(X)
  }
}

# Meilleurs paramètres du eGARCH selon le bic
compute_bic = function(par, res, t){
  l = length(t)
  M2 = par[1]
  P = par[2]
  X = matrix(nr=l,nc=2*M2)
  for (m in 1:M2){
    X[,m] = cos(2*pi*t*m/365)
    X[,M2+m] = sin(2*pi*t*m/365)
  }
  spec = ugarchspec(
    variance.model = list(model='eGARCH',external.regressors = X),
    distribution.model = "snorm",
    mean.model = list(armaOrder = c(P, 0),include.mean=FALSE),
    start.pars = list(omega=1.1, alpha1=0.1,beta1=0.5))

  egarchfit = ugarchfit(spec = spec, data = res)
  k = length(coef(egarchfit))
  bic = k*log(l) - 2*likelihood(egarchfit)
  return(bic)
}

best_garch_order = function(maxM2, maxP){
  eval_bic = function(par) Bic_Garch(par, deltaTemp, training_days) 
  order_comb = expand.grid(M2=1:maxM2,P=1:maxP) 
  order_mean = matrix(apply(order_comb, 1, eval_bic), nr=maxM2, nc=maxP) 
  order_mean = as.numeric(which(order_mean == min(order_mean), arr.ind=TRUE)) 
}

simulate_temp = function(i) {
  # index of initial values of the year to forecast
  lag = ((train_size-1)*365 + 1):((train_size-1)*365+P)
  AR = vector(length=365) 

  lsigma2 = vector(length = 365)
  lsigma2[1] = log(sdhat[365*train_size]^2)
  
  sim_deltaTemp = matrix(nr=365,nc=1)
  sim_deltaTemp[1:P] = deltaTemp[lag]
  
  sim_temp = vector(length = 365)
  sim_temp[1:P] = temp_data$Mean[lag]
  
  z = rsnorm(365, xi = skew)
  
  for (i in (P+1):365){
    AR[i] = rho %*% sim_deltaTemp[(i-1):(i-P)]
    lsigma2[i] = s2[i] + omega + alpha*z[i-1] + gamma*(abs(z[i-1])-Ez) + beta*lsigma2[i-1]
    sim_temp[i] = s1[i] + AR[i] + exp(lsigma2[i]/2)*z[i]
    sim_deltaTemp[i] = sim_temp[i]-s1[i] 
  }
  return(sim_temp)
}

expected_payoff = function(x,K,option_type){ 
  if (option_type == 'call')
    return(mean(pmax(x-K,0)))
  if (option_type=='put')
    return(mean(pmax(K-x,0)))
}

svar = function(x, mean_x){mean(pmin(x-mean_x, 0)^2)^0.5}

tvar = function(x, p){
  VaR = 0.95*146 #quantile(x, probs=p)
  TVaR = mean(x[x < VaR])
  return(TVaR)
}

cost = function(par, R, AccRain, AccCDD, AccHDD, cost_function, q){
  
  a  = exp(par[1])
  b  = exp(par[2])
  c  = exp(par[3])
  d  = exp(par[4])
  
  K1 = round(exp(par[5]))
  K2 = round(exp(par[6]))
  K3 = round(exp(par[7]))
  K4 = round(exp(par[8]))
  
  WD1 = a*(pmax(AccRain[,2] -K1,0) - price_call_rain[price_call_rain[,1]== K1,2])
  WD2 = b*(pmax(AccCDD - K2,0)     - price_call_cdd[price_call_cdd[,1] == K2,2])
  WD3 = c*(pmax(AccHDD - K3,0)     - price_call_hdd[ price_call_hdd[,1] == K3,2])
  WD4 = d*(pmax(K4-AccCDD,0)       - price_put_cdd[price_put_cdd[,1] == K4,2])
  
  profit = R + WD1 + WD2 + WD3 + WD4
  
  if (cost_function == 'tvar'){
    cost = - tvar(profit, q)
    return(cost)
  }
  
  if (cost_function == 'var'){
    cost = sum((profit-146)^2)/2 + 5*(a^2 + b^2 + c^2 + d^2)/2
    return(cost)
  }
  
  if (cost_function == 'svar'){
    cost = svar(profit, 155)
    return(cost)
  } 
}

#graphics parameters
par_default = par()
par(mar=c(2.5,3.5,1,3.5)) #bottom, left, top, and right
par(oma=c(0,0,0,0))
par(cex=0.6)
par(tcl=c(-0.25))
par(mgp=c(1.25, 0.3, 0))

test_price = function(){

  load("annual_data.Rdata")

  profit = vector()

  for (i in 1:1000){
    quantity = c(runif(1, min = 0.1, max=0.5), runif(1, min = 0.1, max=0.5), runif(1, min = 0.1, max=0.5), runif(1, min = 0.1, max=0.5))
    strike = c(runif(1, 50, max = 150), runif(1, 250, max = 400), runif(1, 50, max = 200), runif(1, 250, max = 400))
    par = cbind(quantity, strike)

    sim_temp = simulate_temp(1)
    sim_dat  = cbind(temp_data$Month[1:365], sim_temp)
    AccCDD   = sum(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8),-c(1)]-18,0))
    AccHDD   = sum(pmax(18-sim_dat[sim_dat[,1] == 9,-c(1)],0))
    AccRain  = rMvdc(1, copula_dist) # AccRain[1,]=QC, AccRain[2,]=Chicago

    a  = exp(par[1])
    b  = exp(par[2])
    c  = exp(par[3])
    d  = exp(par[4])
    
    K1 = round(exp(par[5]))
    K2 = round(exp(par[6]))
    K3 = round(exp(par[7]))
    K4 = round(exp(par[8]))

    WD1 = a*(max(AccRain[,2] -K1,0) - price_call_rain[price_call_rain[,1]== K1,2])
    WD2 = b*(max(AccCDD - K2,0)     - price_call_cdd[price_call_cdd[,1] == K2,2])
    WD3 = c*(max(AccHDD - K3,0)     - price_call_hdd[ price_call_hdd[,1] == K3,2])
    WD4 = d*(max(K4-AccCDD,0)       - price_put_cdd[price_put_cdd[,1] == K4,2])

    W1 = a*(max(annual_data$AccRain5[21] - K1, 0) - price_call_rain[price_call_rain[,1] == K1,2])
    W2 = b*(max(annual_data$AccCDD[21] -  K2, 0) - price_call_cdd[price_call_cdd[,1]   == K2,2])
    W3 = c*(max(annual_data$AccHDD9[21] - K3, 0) - price_call_hdd[price_call_hdd[,1]   == K3,2])
    W4 = d*(max(K4-annual_data$AccCDD[21],    0) - price_put_cdd[price_put_cdd[,1]     == K4,2])
    profit[i] = W1 + W2 + W3 + W4
  }

  print(mean(profit))
  print(sd(profit))

  stop('break')
}

detrend = function(y, x, log){
  if (log == TRUE){y = log(y)}
  trend = fitted(lm(y~x))
  detrend = y - trend + trend[length(trend)]
}
