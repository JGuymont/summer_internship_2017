# setwd('/home/gj/Desktop/Machine Learning Project/Stage de recherche/data_preprocessing')

cumulative_index = function(daily_index, type){ 
    "Compute the monthly cumulative index
    of a daily index for a given month.
    
    Arguments:
        daily_index: vector of daily temperatures
        month: month on wich the cumulative index compute in number (e.g. january = 1)
        type: 'HDD' or 'CDD'
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

preprocess_temperatures = function(data){
    "Preprocess daily temperatures

    "
    # remove all february 29
    data = data[data$Month != 2 | data$Day != 29,]    

    # index all lines from 1 to the data length
    data$t=1:length(data$Mean)

    # add a column d (for day) that goes from 1 to 365
    data$d=rep(1:365,50)

    # compute cdd and hdd
    data$cdd = pmax(data$Mean-18, 0)
    data$hdd = pmax(18-data$Mean, 0)
    
    # compute temperature trend
    trend = fitted(lm(Mean~t, data=data))

    # compute detrend temperature
    data$dtr_temp = data$Mean-trend+trend[18250-182] 
    
    # compute detrend cdd and hdd
    data$dtr_cdd = pmax(data$dtr_temp-18, 0)
    data$dtr_hdd = pmax(18-data$dtr_temp, 0)

    return(data)

}

# load daily temperature data
temperatures = read.table('./data/temperatures.csv', header=T)[, c('Year', 'Month', 'Day', 'Mean', 'Tmin', 'Tmax')]

# preprocess daily temperatures
temperatures = preprocess_temperatures(temperatures)

# save preprocessed temperatures
save(temperatures, file = './data/temperatures.Rdata')

# Load Quebec corn yields data
yield_data = read.table('./data/corn_yields.csv', header = T)

# Load monthly Quebec precipitations data
rain_data = read.table('./data/qc_daily_precipitations.csv', header = T)

# initialize matrices of cumulative index. 
# Columns 1 to 5 correspond to month may to september  
AccCDD  = matrix(nr=50,nc=5)
AccHDD  = matrix(nr=50,nc=5)
AccRain = matrix(nr=50,nc=5)

# computation of cumulatives index for months May to September
for (i in 5:9){
    AccCDD[,i-4]  = cumulative_index(temperatures$Mean[temperatures$Month == i], 'cdd')
    AccHDD[,i-4]  = cumulative_index(temperatures$Mean[temperatures$Month == i], 'hdd')
    AccRain[,i-4] = colSums(matrix(rain_data$total[rain_data$month==i], nc=50))
}

# initialize annual database 
annual_data = data.frame(matrix(nr=50, nc=19))

annual_data = data.frame(cbind(1:50, 1966:2015, yield_data$Yield[1:50], AccCDD, AccHDD, AccRain))
colnames(annual_data) = c('t','year', 'yield', paste0('AccCDD',5:9), paste0('AccHDD',5:9), paste0('AccRain',5:9))

annual_data$AccRain = rowSums(annual_data[,paste0('AccRain',6:8)])
annual_data$AccCDD  = rowSums(annual_data[,paste0('AccCDD',6:8)])
annual_data$sqAccCDD = annual_data$AccCDD^2
annual_data$lyield = log(annual_data$yield)

yield_trend = fitted(lm(lyield~t, data=annual_data))
annual_data$dtr_yield = annual_data$lyield-yield_trend + yield_trend[50]

save(annual_data, file='./data/data.Rdata')
