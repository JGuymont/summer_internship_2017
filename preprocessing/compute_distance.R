

select_station = function(i, path_data, stations, fields){
    "Go trough the weather stations and select the
     closest that has at least 95% of the data.
	
    Arguments
		R: (Int) 
		path_data: (string) path to the directory where the data on precipitation is saved
		stations: (dataframe)
		fields: (dataframe)
	"
    R = 6373

	lon1 = stations$Longitude*pi/180
    lat1 = stations$Latitude*pi/180
    
	lon2 = fields$x[i]*pi/180
    lat2 = fields$y[i]*pi/180

	dlat = lat2 - lat1
    dlon = lon2 - lon1
    
	a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
    c = 2 * atan2( sqrt(a), sqrt(1-a) )
    
	distance = R * c

    stations_id = stations$Id_Station[order(distance)]
    
    filenames = paste0(path_data, stations_id, '.csv')
    prcp_len = vector()
    data = read.table(file=filenames[1], header=TRUE, skip=18, sep=",")[,c('Year','Month','Total.Precip..mm.')]
    prcp = data$Total.Precip..mm.[data$Year %in% 1966:2015 & data$Month == 5]
    prcp_len[1] = length(prcp[!is.na(prcp)])/((2015-1965)*12)
    
    j = 1
    while(prcp_len[j] < 0.95 & j < length(stations_id)){
		j = j+1
		data = read.table(file=filenames[j], header=TRUE, skip = 18, sep=",")[,c('Year','Month','Total.Precip..mm.')]
		prcp = data$Total.Precip..mm.[data$Year %in% 1966:2015 & data$Month == 5]
		prcp_len[j] = length(prcp[!is.na(prcp)])/((2015-1965))
    }

    return(list(stations_id=stations_id[j],
                distance=distance[order(distance)][j],
                prcp_len=prcp_len[j],
                lon=stations$Longitude[stations$Id_Station == stations_id[j]],
                lat=stations$Latitude[stations$Id_Station == stations_id[j]]
			)
    	)
}
