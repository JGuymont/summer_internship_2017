setwd('/home/gj/Desktop/Machine Learning Project/Stage de recherche/data_preprocessing')

cum_index = function(daily_index, type){
  
  "
  Function description:
  Compute the monthly cumulative index
  of a daily index for a given month.
  
  Arguments:
  - daily_index: vector of daily temerature
  - month: month on wich the cumulative index
  compute in number (e.g. january = 1)
  - type: 'HDD' or 'CDD'
  "
  x = daily_index
  if (type == 'hdd'){
    hdd = pmax(18 - x, 0)
    cum_index = colSums(matrix(hdd, nc = 50))
  }
  if (type == 'cdd'){
    cdd = pmax(x - 18, 0)
    cum_index = colSums(matrix(cdd, nc = 50))
  }
  return(cum_index)
}

# load daily temperature data
temp_vars = c('Year','Month','Day','Mean','Tmin','Tmax') # relevant columns
temp_data = read.table('temperature.txt',header=T)[,temp_vars]

# remove all Feb 29s
temp_data = temp_data[temp_data$Month != 2 | temp_data$Day != 29,]

# add variable "t"
temp_data$t = 1:length(temp_data[,1])

# index days from 1 to 365
temp_data$d=rep(1:365,50)

# save dataframe
save(temp_data, file = '/home/gj/Desktop/Machine Learning Project/Stage de recherche/temp_data.Rdata')

# Load Quebec corn yields data
yield_data = read.table('corn_yields.txt', header = T)

# Load monthly Quebec precipitations data
rain_data = read.table('qc_rains.txt', header = T)

# initialize matrices of cumulative index. 
# Columns 1 to 5 correspond to month may to september  
AccCDD  = matrix(nr=50,nc=5)
AccHDD  = matrix(nr=50,nc=5)
AccRain = matrix(nr=50,nc=5)

# computation of cumulatives index for months May to September
for (i in 5:9){
  AccCDD[,i-4]  = cum_index(temp_data$Mean[temp_data$Month == i], 'cdd')
  AccHDD[,i-4]  = cum_index(temp_data$Mean[temp_data$Month == i], 'hdd')
  AccRain[,i-4] = colSums(matrix(rain_data$total[rain_data$month==i], nc=50))
}

# initialize annual database 
annual_data = data.frame(matrix(nr=50, nc=19))

annual_data = data.frame(cbind(1:50, 1966:2015, yield_data$Yield[1:50], AccCDD, AccHDD, AccRain))
colnames(annual_data) <- c('t','year', 'yield', paste0('AccCDD',5:9), paste0('AccHDD',5:9), paste0('AccRain',5:9))

annual_data$AccRain = rowSums(annual_data[,paste0('AccRain',6:8)])
annual_data$AccCDD  = rowSums(annual_data[,paste0('AccCDD',6:8)])
annual_data$sqAccCDD = annual_data$AccCDD^2
annual_data$lyield = log(annual_data$yield)

yield_trend = fitted(lm(lyield~t, data=annual_data))
annual_data$dtr_yield = annual_data$lyield-yield_trend + yield_trend[50]

save(annual_data, file = "/home/gj/Desktop/Machine Learning Project/Stage de recherche/annual_data.Rdata")
