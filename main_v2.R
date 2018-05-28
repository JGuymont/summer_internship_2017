yield_model = lm(yield ~ t + AccRain5 + AccHDD9 + AccCDD + sqAccCDD, data = annual_data)
ols_param = coef(yield_model)
Sigma = summary(yield_model)$sigma

# seasonal_trend1
M1 = 3
four_terms = Fourier(M1, length(training_days))
seatrend_model = lm(Mean ~ t + four_terms, data = temp_data) 
seatrend = fitted(seatrend_model)
deltaTemp = DAT - seatrend  

###### Section 2: DAT GARCH component ------------------------------------------------------------
M2 = 3; P = 3
#garch_order = best_garch_order(maxM2 = 6, maxP = 4)
#M2 = garch_order[1] 
#P = garch_order[2]

spec = ugarchspec(
  variance.model = list(model='eGARCH',external.regressors = Fourier(M2, length(training_days))), 
  distribution.model = "snorm",
  mean.model = list(armaOrder = c(P, 0), include.mean=FALSE), 
  start.pars = list(omega=1.1, alpha1=0.1,beta1=0.5)) 

egarchfit = ugarchfit(spec = spec, data = deltaTemp) 
coef_garch = data.frame(coef(egarchfit)) 

skew = coef_garch['skew',1] 
omega = coef_garch['omega',1] 
alpha = coef_garch['alpha1',1] 
beta  = coef_garch['beta1',1]
gamma = coef_garch['gamma1',1]
rho = matrix(coef_garch[1:P,1], nr=1,nc=P) 
M2s = coef_garch[(P+5):(M2*2+P+4),1]
sdhat = as.numeric(sigma(egarchfit)) 

###### Section 3: Determination of Hedging strategy 
s1 = matrix(cbind(1, test_days, Fourier(M1, 365)), nr = 365, nc = 8) %*% matrix(seatrend_model$coefficients, nr = 8, nc =1) 
s2 = colSums(M2s*t(Fourier(M2,length(training_days))))  
Ez = sqrt(2/pi) 

# Estimated prices of derivatives -----------------------------------------
sim_dat = cbind(temp_data$Month[1:365],sapply(1:10000, simulate_temp))
AccCDD  = colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8),-c(1)]-18,0))
AccHDD  = colSums(pmax(18-sim_dat[sim_dat[,1] == 9,-c(1)],0))
AccRain = rgamma(m, shape = shape_qc, scale=scale_qc)
ep_call_cdd     = sapply(seq(0,500,1), function(x) expected_payoff(AccCDD, x, 'call'))
price_call_cdd  = cbind(K=seq(0,500,1), ep_call_cdd)
ep_put_cdd = sapply(seq(0, 500, 1), function(x) expected_payoff(AccCDD, x, 'put'))
price_put_cdd   = cbind(K=seq(0, 500, 1), ep_put_cdd)
ep_call_hdd = sapply(seq(0, 200, 1), function(x) expected_payoff(AccHDD, x, 'call'))
price_call_hdd  = cbind(K=seq(0, 200, 1), ep_call_hdd)
ep_call_rain = sapply(seq(0, 250, 1), function(x) expected_payoff(AccRain, x, 'call'))
price_call_rain = cbind(K=seq(0, 250, 1), ep_call_rain)

# SECTION: Determination of Hedging strategy --------------------------------------------------------
cl = makeCluster(detectCores()-1) ; print(cl)
cl_objects = list( 'temp_data','end_year','train_size','training_years','training_days','test_year','test_days',
                  # parameters
                   'ols_param','Sigma', 'P', 'M2','omega','alpha','beta','gamma','skew',
                   'rho','Ez', 'sdhat', 'm', 'num_sim',
                  # relevant series
                   's1', 's2', 'seatrend','deltaTemp',
                  # relevant functions
                   'copula_dist', 'simulate_temp', 'cost', 'tvar', 'svar',
                  # derivatives prices  
                   'price_call_cdd', 'price_put_cdd', 'price_call_hdd', 'price_call_rain')

clusterExport(cl, cl_objects)

p = parSapply(cl, 1:num_sim, function(i) {
  library(fGarch)
  library(copula)
  sim_temp = sapply(1:m, simulate_temp)
  sim_dat  = cbind(temp_data$Month[1:365], sim_temp)
  AccCDD   = colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8),-c(1)]-18,0))
  AccHDD   = colSums(pmax(18-sim_dat[sim_dat[,1] == 9,-c(1)],0))
  AccRain  = rMvdc(m, copula_dist) # AccRain[1,]=QC, AccRain[2,]=Chicago
  
  epsilon = rnorm(m ,mean=0, sd=Sigma)
  design = matrix(cbind(1, 50, AccRain[,1], AccHDD, AccCDD, AccCDD^2), nrow = m, ncol = 6)
  B = matrix(ols_param, nrow=6, ncol=1)
  R = exp(as.vector(design %*% B))
  
  quantity = c(runif(1,min=-1,max=0),runif(1,min =-1,max=0),runif(1,min=-1,max=0),runif(1,min=-1,max=0))
  strike = c(runif(1, 4, max = 5), runif(1, 5, max = 6), runif(1, 4, max = 5), runif(1, 5, max = 6))
  par0 = cbind(quantity, strike)
  print(par0)
  COST = function(par0) cost(par0, R, AccRain, AccCDD, AccHDD, 'svar', 0.10)
  COST(par0)
  hedge = optim(par0, COST)
  p = exp(hedge$par)
  return(p)
})

if(!is.null(cl)) {
  stopCluster(cl)
  cl = c()
}

# Section: Determination of 'hedged' profits ------------------------------------------------------------------
load("annual_data.Rdata") # reload the data to have complete historic for backtesting

# 'yld' is the vector of historical profits 
yld = exp(annual_data$dtr_yield)
print(mean(yld))

hyld = vector()
derivatives_profit = vector()
for (i in 1:num_sim){
  K1 = round(p[5,i]) 
  K2 = round(p[6,i])
  K3 = round(p[7,i])
  K4 = round(p[8,i])
  
  # Profit/loss on contracts
  W1 = p[1,i]*(max(annual_data$AccRain5[test_year] - K1, 0) - price_call_rain[price_call_rain[,1] == K1,2])
  W2 = p[2,i]*(max(annual_data$AccCDD[test_year] -  K2, 0) - price_call_cdd[price_call_cdd[,1]   == K2,2])
  W3 = p[3,i]*(max(annual_data$AccHDD9[test_year] - K3, 0) - price_call_hdd[price_call_hdd[,1]   == K3,2])
  W4 = p[4,i]*(max(K4-annual_data$AccCDD[test_year],    0) - price_put_cdd[price_put_cdd[,1]     == K4,2])
  
  derivatives_profit[i] = W1 + W2 + W3 + W4 
  
  # hedged profits

  hyld[i] = yld[test_year] + W1 + W2 + W3 + W4 
}
