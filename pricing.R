m=100000
l=18250
inSample_length = 50
setwd('/home/gj/Desktop/Link to risque/Agriculture/Quebec/Data')
folder_source <- '/home/gj/Desktop/Link to risque/Agriculture/Quebec/R/pricing/'
library(fGarch)
library(copula)
library(parallel)

load('dbTemp.Rdata')
load('garchtrorder.Rdata')
load('data_rain_mai.Rdata')

# paramètres des distributions de précipitations
fit_rain_dist = readRDS('fit_rain_dist.rds')
shape_qc  = fit_rain_dist$shape.qc
scale_qc  = fit_rain_dist$scale.qc
shape_det = fit_rain_dist$shape.det
scale_det = fit_rain_dist$scale.det
shape_ch  = fit_rain_dist$shape.ch
scale_ch  = fit_rain_dist$scale.ch

source(paste0(folder_source,'rain_copula.R'))
source(paste0(folder_source,'functions.R'))

# Section: DAT model ------------------------------------------------------------
seatrend <- fitted(lm(Mean~t+Fourier(3,18250), data = db.temp))
deltaTemp <- db.temp$Mean - seatrend
P=3;M2=4

spec <- ugarchspec(
  variance.model = list(model='eGARCH',external.regressors = Fourier(4,18250)),
  distribution.model = "snorm",
  mean.model = list(armaOrder = c(3, 0), include.mean=FALSE), 
  start.pars = list(omega=omega0, alpha1=0.1,beta1=beta0))

egarchfit = ugarchfit(spec = spec, data = res1)
coef.garch <- coef(egarchfit)

skew = coef.garch['skew']
skew = coef.garch['skew']
omega = coef.garch['omega']
alpha = coef.garch['alpha1']
beta  = coef.garch['beta1']
gamma = coef.garch['gamma1']
rho <- matrix(coef.garch[1:3], nr=1,nc=3)

# SECTION: Détermination of Hedging strategy --------------------------------------------------------

############### valeurs initialles pour les simulation #################

# seasonal trend 
s1 <- seatrend[(18250-365+1):18250]

# seasonal volatility
s2 = colSums(coef.garch[(P+5):(M2*2+P+4)]*t(Fourier(M2,18250)))

# E[|z|], z ~ N(0,1)
Ez=sqrt(2/pi)

sim_dat <- cbind(db.temp$Month[1:365],sapply(1:m, simulate_dat))
AccCDD   <- colSums(pmax(sim_dat[sim_dat[,1] %in% c(6,7,8),-c(1)]-18,0))
AccHDD   <- colSums(pmax(18-sim_dat[sim_dat[,1] == 9,-c(1)],0))
AccRain  <- rgamma(m, shape = shape_qc, scale=scale_qc)

expected_payoff <- function(x,K,option_type){ 
  if (option_type=='call')
    return(mean(pmax(x-K,0)))
  if (option_type=='put')
    return(mean(pmax(K-x,0)))
}

quantiles_payoff <- function(x,K,option.type){ 
  if (option.type=='call')
    return(quantile(pmax(x-K,0), probs=c(0.05,0.95)))
  if (option.type=='put')
    return(quantile(pmax(K-x,0),probs=c(0.05,0.95)))
}

ep_call_cdd <- sapply(seq(10,500,10),function(x) expected_payoff(AccCDD,x,'call'))
qt_call_cdd <- sapply(seq(10,500,10),function(x) quantiles_payoff(AccCDD,x,'call'))
price_call_cdd  <- cbind(K=seq(10,500,10),ep_call_cdd,L90=qt_call_cdd[1,], U90=qt_call_cdd[2,])

ep_put_cdd <- sapply(seq(10,500,10),function(x) expected_payoff(AccCDD,x,'put'))
qt_put_cdd <- sapply(seq(10,500,10),function(x) quantiles_payoff(AccCDD,x,'put'))
price_put_cdd   <- cbind(K=seq(10,500,10),ep_put_cdd,L90=qt_put_cdd[1,],U90=qt_put_cdd[2,])

ep_call_hdd <- sapply(seq(10,200,10),function(x) expected_payoff(AccHDD,x,'call'))
qt_call_hdd <- sapply(seq(10,200,10),function(x) quantiles_payoff(AccHDD,x,'call'))
price_call_hdd  <- cbind(K=seq(10,200,10),ep_call_hdd,L90=qt_call_hdd[1,],U90=qt_call_hdd[2,])

ep_call_rain <- sapply(seq(10,250,10),function(x) expected_payoff(AccRain,x,'call'))
qt_call_rain <- sapply(seq(10,250,10),function(x) quantiles_payoff(AccRain,x,'call'))
price_call_rain <- cbind(K=seq(10,250,10),ep_call_rain,L90=qt_call_rain[1,],U90=qt_call_rain[2,])

saveRDS(list(price_call_cdd=price_call_cdd,
             price_put_cdd=price_put_cdd,
             price_call_hdd=price_call_hdd,
             price_call_rain=price_call_rain),
        file=paste0(folder_source,'price.rds'))

