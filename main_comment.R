#-----------------------------------------------------------------------------
#
#  Description:  Étant donné la relation de dépendance entre la température
#                le rendements des champs de maïs, déterminer le portefeuille optimal de 
#                dérivés climatique afin de se protéger contre les pertes engendrés par
#                les anomalies climatiques. Dans cet exemple, le critère utilisé pour
#                déterminé la stratégie optimal est le conditionnal value at risk (CVaR ou TVaR), i.e. la portefeuille 
#                optimal est celui qui maximise le CVaR du profit.
#                La CVaR est définie comme E[Profit | Profit < VaR_{alpha}(Profit)].
#                
#                Étape non incluses dans le code:
#                Les varibles explicatives ont déjà été déterminé pour la relation de dépendance entre la température
#                le rendements des champs de maïs: 
#                lnY = a + b1*AccRain + b2*AccHDD + b3*AccCDD + b4*AccCDD^2 + u, u~normal(0, sigma^2)               
#
#                La copule qui décrit le mieux la relation entre les précipitations
#                entre chicago et la province de quebec a aussi été déterminé (il s'agit de la copule normale). Les 
#                paramètre des marges de la copule (distribution des precip. de Quebec et de chicago) ont été déterminés.
#  
#
#       Output:  Les paramètres des portefeuilles optimales et les profits correspondants.
#
#      Method :  La stratégie est développer en se basant sur n données historiques (eg 1966-1991). 
#                 Étapes
#                 i. Un modèle de regression pour la relation entre la temperature
#                 ii. Un modele eGarch pour la temperature quotidienne moyenne
#                 iii. Le portefeuille optimal est déterminé en simulants des données à partir des 
#                      modèles basés sur les n données historiques.
#                 iv. (Backtest) La stratégie est tester sur l'ensemble des données historiques (1966-2015). 
#
#                Éventuellement, le modèle sera mis à jour à chaque année en ajoutant iterativement 
#                1 années de données et en déterminant à chaque fois une nouvelle stratégie pour l'année suivante.
#
#   
# ===========================================================================*/

setwd('/home/gj/Desktop/Machine Learning Project/Stage de recherche')

###### Section 1: Crop yield model --------------------------------------------------------
# Parameters are fitted using in-sample data only. Variables have been choosed previously. 
yield_model = lm(lyield~t+AccRain5+AccHDD9+AccCDD+sqAccCDD, data = annual_data)
ols_param = coef(yield_model)
S = summary(yield_model)
Sigma = S$sigma

###### Section 2: DAT mean model ------------------------------------------------------------

# DAT model: 
# model: DAT = trend + seasonal_trend1 (seatrend := s1) + AR(P) + sigma_t*u_t, u_t iid(0,1) 
# (eGARCH(1,1)): 
# ln(sigma_t^2) = omega + seasonal_trend2 (s2) + alpha*g(u_{t-1}) + beta*ln(sigma_{t-1}^2)
# g(u_{t-1}) = u_{t-1} + gamma*(|u_{t-1}|-E|u_{t-1}|) 
# daily average temperature is modeled using in-sample data only

M1 = 3
# seasonal_trend1:
# seasonal trend of the series fitted using a Fourier series.
four_terms = Fourier(M1, length(training_days))
seatrend_model = lm(Mean ~ t + four_terms, data = temp_data) 
seatrend = fitted(seatrend_model)

deltaTemp = DAT - seatrend # remainder of the DAT once the mean is removed 

###### Section 2: DAT GARCH component ------------------------------------------------------------

bic_garch = function(par) Bic_Garch(par, deltaTemp, training_days) #
# bic_garch find the Bayesian information criterion for each combinations
# (M2, P) in order to find the best orders for the AR(P) component and the mean of the GARCH
# (omega + seasonal_trend2). All parameters are fitted at the same time using Gaussian quasi-maximum likelihood.


maxM2=6; maxP=4 # maximum orders to be tested.
order_comb = expand.grid(M2=1:maxM2,P=1:maxP) # matrix of all the combinations of M2 and P.
order_mean = matrix(apply(order_comb,1,bic_garch),nr=maxM2,nc=maxP) # bic_garch is applied to all the combinations (lines of order_comb).
order_mean = as.numeric(which(order_mean == min(order_mean),arr.ind=TRUE)) # find the position of min(bic_garch). Row=M2, Column = P.
M2  = order_mean[1] 
P = order_mean[2]

# Method for creating a univariate GARCH specification object prior to fitting.
spec = ugarchspec(
  variance.model = list(model='eGARCH',external.regressors = Fourier(M2, length(training_days))), # There is a cyclical trend in the volatility
                                                                             # so we had a Fourier to model the mean
  
  distribution.model = "snorm", # the skewed normal seems to fit the residuals better than the normal
  
  mean.model = list(armaOrder = c(P, 0), include.mean=FALSE), # P is the AR order of the series
  
  start.pars = list(omega=1.1, alpha1=0.1,beta1=0.5)) # starting parameters have been determined previously and are sometimes
                                                      # needed for the algorithm to converge

egarchfit = ugarchfit(spec = spec, data = deltaTemp) # the algo that fit the GARCH is called here with parameters spec
coef_garch = data.frame(coef(egarchfit)) 

skew = coef_garch['skew',1] # parameter of the residuals distribution
omega = coef_garch['omega',1]  # omega, alpha1, beta1 and gamma are parameters of the eGARCH
alpha = coef_garch['alpha1',1] 
beta  = coef_garch['beta1',1]
gamma = coef_garch['gamma1',1]
rho = matrix(coef_garch[1:P,1], nr=1,nc=P) # AR(P) coefficients
M2s = coef_garch[(P+5):(M2*2+P+4),1]
sdhat = as.numeric(sigma(egarchfit)) # estimated of the conditionnal standard deviation

###### Section 3: Determination of Hedging strategy --------------------------------------------------------

############### initial value for simulations #################

# seasonal trend 
s1 = matrix(cbind(1, test_days, four_terms[1:365,]), nr = 365, nc = 8) %*% matrix(seatrend_model$coefficients, nr = 8, nc =1) # the seasonal trend is deterministic. We used the last year of 
                                                               # data available because of the global warming trend.

# seasonal volatility
s2 = colSums(M2s*t(Fourier(M2,length(training_days)))) # the seasonal trend in the volatility is also deterministic. s2 
                                               # is a Fourier serie fitted in section 2

Ez=sqrt(2/pi) # E[|z|], z ~ skNormal(0,1, xi). E[|z|] is the expected value of eGARCH residuals. 
              # It is a parameter needed in the eGARCH model. Even if the 
              # distribution of z is skewed, the mean remaind the same
              # as a normal.


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

cl = makeCluster(detectCores()-1) # Start up a parallel cluster
print(cl)

# cl.objects = objects we need to 'link' to the cluster environment
cl_objects = list( 'temp_data','training_years','training_days','test_year','test_days',
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

# each columns of 'p' is a vector of parameters (p have num_sim columns).
p = parSapply(cl, 1:num_sim, function(i) {
  library(fGarch)
  library(copula)
  sim_temp = sapply(1:m, simulate_temp)
  sim_dat  = cbind(temp_data$Month[1:365], sim_temp)
  AccCDD   = colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8),-c(1)]-18,0))
  AccHDD   = colSums(pmax(18-sim_dat[sim_dat[,1] == 9,-c(1)],0))
  AccRain  = rMvdc(m, copula_dist) # AccRain[1,]=QC, AccRain[2,]=Chicago
  
  epsilon = rnorm(m ,mean=0, sd=Sigma)
  design = matrix(cbind(1, training_years + 1, AccRain[,1], AccHDD, AccCDD, AccCDD^2), nrow = m, ncol = 6)
  B = matrix(ols_param, nrow=6, ncol=1)
  R = exp(as.vector(design %*% B)+epsilon)
  par0 = c(0.25, 0.25, 0.25, 0.25, 100, 402, 50, 200)
  COST = function(par0) cost(par0, R, AccRain, AccCDD, AccHDD, 'tvar', 0.10)
  COST(par0)
  hedge = optim(par0, COST)
  p = abs(hedge$par)
  return(p)
})

if(!is.null(cl)) {
  stopCluster(cl)
  cl = c()
}

# Section: Determination of 'hedged' profits ------------------------------------------------------------------
load("annual_data.Rdata") # reload the data to have complete historic for backtesting

# 'yld' is the vector of historical profits 
yld = annual_data$yield 

# Each row of «hyld» is a hedged profit corresponding to a strategy. 
# Row i is the vector of profits that agricultor would havr earned in years 1966-2015  
# if they had bougth portfolio i, where portfolio i correspond to column i of «p». 
hyld = vector()
derivatives_profit = vector()
for (i in 1:num_sim){
  K1 = round(abs(p[5,i])) # contract strikes 
  K2 = round(abs(p[6,i]))
  K3 = round(abs(p[7,i]))
  K4 = round(abs(p[8,i]))
  
  # Profit/loss on contracts
  W1 = p[1,i]*(max(annual_data$AccRain[test_year] - K1, 0) - price_call_rain[price_call_rain[,1] == K1,2])
  W2 = p[2,i]*(max(annual_data$AccCDD[test_year] -  K2, 0) - price_call_cdd[price_call_cdd[,1]   == K2,2])
  W3 = p[3,i]*(max(annual_data$AccHDD9[test_year] - K3, 0) - price_call_hdd[price_call_hdd[,1]   == K3,2])
  W4 = p[4,i]*(max(K4-annual_data$AccCDD[test_year],    0) - price_put_cdd[price_put_cdd[,1]     == K4,2])
  
  derivatives_profit[i] = W1 + W2 + W3 + W4
  hyld[i] = yld[test_year] + W1 + W2 + W3 + W4 # hedged profits
  }

# Output ------------------------------------------------------------------
