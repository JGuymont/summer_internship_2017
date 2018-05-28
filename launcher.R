setwd('/home/gj/Documents/Machine_Learning_Project/Stage de recherche')

# required libraries
library(fGarch)   # for skewed normal distribution
library(copula)   # Fitting copula models and goodness-of-fit tests. 
library(rugarch)  # GARCH modeling and testing environment
library(parallel) # package for parallel computing

num_sim = 10
m = 1000
train_size = 15
output = matrix(nr = num_sim, nc = 50)
wd_profits = matrix(nr = num_sim, nc = 50)

# Fitted parameters for rain distributions. These distribution have been 
# fitted previously. For Qc it is a Gamma and for Chicago it's a Weibull.
fit_rain_dist = readRDS('fit_rain_dist.rds')
shape_qc  = fit_rain_dist$shape.qc
scale_qc  = fit_rain_dist$scale.qc
shape_ch  = fit_rain_dist$shape.ch
scale_ch  = fit_rain_dist$scale.ch

source('rain_copula.R') # The copula between precipitation in Quebec and Chicago is fitted here
source('functions.R')   # Contains all the other functions needed
start_year = 16
for (end_year in start_year:49){ 

  # in-sample years 
  training_years = (end_year - train_size + 1):end_year 
  
  # in-sample days 
  training_days  = ((end_year - train_size)*365 + 1):(end_year*365) 
  
  # out-sample years
  test_year = end_year + 1
  
  # out-sample days
  test_days = (end_year*365 + 1):((end_year + 1)*365)
  
  # initialize data and function according to in-sample length
  load('annual_data.Rdata')
  load('temp_data.Rdata') # daily temperature from 1966 to 2015

  # out of sample years are removed from the data
  temp_data = temp_data[training_days,] 
  DAT = temp_data$Mean # Daily Average temperature
  annual_data = annual_data[training_years,]
  db.prcp.mai = db.prcp.mai[training_years,]
  
  # main program
  source('main_v2.R')
  
  output[, end_year + 1] = hyld
  wd_profits[, end_year + 1] = derivatives_profit
}

output[,start_year] = yld[start_year] 

x = colMeans(output)
ymin = min(min(output, na.rm = TRUE), min(yld)) - 10
ymax = max(max(output, na.rm = TRUE), max(yld)) + 10

dev.new()
plot(yld, type = 'l', ylim = c(ymin, ymax))
for (i in 1:num_sim){
  color = 450 + i
  lines(output[i,], lty = 2, col = color)
}
lines(x, col = 'red', lwd = 2)
dev.off()

dev.new()
plot(yld, type = 'l', ylim = c(100, 200))
lines(x, col = 'red', lwd = 2)
lines(c(0, 50), c(146*0.95, 146*0.95), lty = 2)
lines(c(0, 50), c(146*1.05, 146*1.05) , lty = 2)
dev.off()

dev.new()
plot(yld, type = 'l', ylim = c(ymin, ymax), col = 'white')
for (i in 1:num_sim){
  color = 450 + i
  lines(output[i,], lty = 2, col = color, lwd = 0.5)
}
lines(yld, type = 'l', ylim = c(ymin, ymax),lwd = 2)
lines(x, col = 'black', lwd = 3, lty = 2)
lines(c(0, 50), c(146*0.95, 146*0.95), lty = 2)
lines(c(0, 50), c(146*1.05, 146*1.05) , lty = 2)
dev.off()

dev.new()
plot(yld, type = 'l', ylim = c(ymin, ymax), col = 'white')
for (i in 1:num_sim){
  color = 450 + i
  lines(output[i,], lty = 2, col = 'black')
}
lines(yld, type = 'l', ylim = c(ymin, ymax),lwd = 2)
lines(x, col = 'red', lwd = 2)
lines(c(0, 50), c(146*0.95, 146*0.95), lty = 2)
lines(c(0, 50), c(146*1.05, 146*1.05) , lty = 2)
dev.off()