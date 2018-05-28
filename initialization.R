# Fitted parameters for rain distributions. These distribution have been 
# fitted previously. For Qc it is a Gamma and for Chicago it's a Weibull.
fit_rain_dist = readRDS('fit_rain_dist.rds')
shape_qc  = fit_rain_dist$shape.qc
scale_qc  = fit_rain_dist$scale.qc
shape_ch  = fit_rain_dist$shape.ch
scale_ch  = fit_rain_dist$scale.ch

source('rain_copula.R') # The copula between precipitation in Quebec and Chicago is fitted here

source('functions.R')   # Contains all the other functions needed

# load data
load('annual_data.Rdata')
load('temp_data.Rdata') # daily temperature from 1966 to 2015
load('db_prcp_mai.Rdata') # prov. of Quebec and some US cities monthly precipitation

# out of sample years are removed from the data
temp_data = temp_data[training_days,] 
DAT = temp_data$Mean # Daily Average temperature
annual_data = annual_data[training_years,]
db.prcp.mai = db.prcp.mai[training_years,]


