load("./data/stations.Rdata")
load("./data/fields.Rdata")

source('./preprocessing/compute_distance.R')

PATH_DATA = './data/precip_by_cty/'
REGIONS = as.character(fields$region[which(!duplicated(fields$region))])
PROD = read.table('./data/prod_5ans_qc.csv',header = TRUE, sep=",")
PROD$w = round(rowMeans(PROD[,4:8])/sum(rowMeans(PROD[,4:8])), 2)

add_closest_station = function(stations, fields){
    # use the function select_station to add the closest
    # station to each fields. 
    for (i in 1:dim(fields)[1]){
        
        cur_selected_station = select_station(i, PATH_DATA, stations, fields)

        if (fields$flag[i]=='O'){
            fields$station[i]     = cur_selected_station[1]
            fields$distance[i]    = cur_selected_station[2]
            fields$prcp_length[i] = cur_selected_station[3]
            fields$station_lon[i] = cur_selected_station[4]
            fields$station_lat[i] = cur_selected_station[5]
        }
        if (fields$flag[i]=='X'){
            fields$station[i]     = NA
            fields$distance[i]    = NA
            fields$prcp_length[i] = NA
            fields$station_lon[i] = NA
            fields$station_lat[i] = NA
        }
    }

    fields$flag_ = ifelse(fields$flag == 'O' & !duplicated(fields$station), 1, NA)
    fields$station_number[!is.na(fields$flag_)] = 1:length(fields$flag_[!is.na(fields$flag_)]) 
    return(fields)     
}

fields = add_closest_station(stations, fields)

create_data_precipitations = function(path_data, fields){

    data_precipitations = data.frame(year = rep(1966:2015, each = 12), month = rep(1:12, 50))

	for (i in which(fields$flag == 'O')){
		data_name = as.character(fields$county[i])
		cur_station = read.table(file=paste0(path_data, fields$station[i],'.csv'), header=TRUE, skip = 18, sep=",")
		cur_data = data.frame(year=cur_station$Year, month=cur_station$Month)
		cur_data[data_name] = cur_station$Total.Precip..mm.
		data_precipitations = merge(x=data_precipitations, y=cur_data, by=c('year', 'month'), all.x=TRUE, sort=TRUE)
	}

	data_precipitations = data_precipitations[order(data_precipitations$year, data_precipitations$month), ]
	precipitations_may = subset(data_precipitations[data_precipitations$month==5,])

	return(precipitations_may)
}

MAY_PRECIPITATIONS = create_data_precipitations(PATH_DATA, fields)

compute_weigted_precip_cty = function(k){

  	cty_list_ = as.character(fields$county[fields$region == REGIONS[k] & fields$flag == 'O'])

    rain_data_ = matrix(nr = 600, nc = length(cty_list_))
  	
    j = 0
  	for (cty in cty_list_){
    	j = j + 1
        rain_data_[,j] = as.numeric(MAY_PRECIPITATIONS[,cty])  
  	}

  	rain_data_[is.na(rain_data_)] = 0
    
  	if (REGIONS[k] %in% c('Capitale-Nationale','Mauricie')) {
    	total_field_ = sum(fields$area_size[fields$region %in% c('Capitale-Nationale', 'Mauricie')])
	}
  
  	if (REGIONS[k] %in% c('Montreal','Laval','Lanaudière')) {
    	total_field_ = sum(fields$area_size[fields$region %in% c('Montreal','Laval','Lanaudière')])
	}
  
  	if (REGIONS[k] %in% c('Outaouais','Laurentides')) {
    	total_field_ = sum(fields$area_size[fields$region %in% c('Outaouais','Laurentides')])
	}
  
  	if (!(REGIONS[k] %in% c('Outaouais','Laurentides','Montreal','Laval','Lanaudière','Capitale-Nationale','Mauricie'))){
    	total_field_ = sum(fields$area_size[fields$region == REGIONS[k]])
	}
  
  	field_ = fields$area_size[fields$county %in% cty_list_]/total_field_
  	
    weighted_precip = vector(length = 600)
    
    for (i in 1:600){
    	missing = which(is.na(rain_data_[i,]))
    	weighted_field = field_ / (1-sum(field_[missing]))
    	weighted_field[missing] = 0
    	weighted_precip[i] = as.vector(rain_data_[i,] %*% weighted_field)
  	}
  	
	return(weighted_precip)
}

compute_weighted_precip_region = function(){
    w = vector()
    for (i in 1:length(REGIONS)){
        w[i] = PROD$w[as.character(PROD$radm1) == REGIONS[i] | as.character(PROD$radm2) == REGIONS[i] | as.character(PROD$radm3) == REGIONS[i]]
    }

    weighted_precip_region = data.frame(
        year = rep(1966:2015, each = 12), 
        month = rep(1:12, 50),
        bap = compute_weigted_precip_cty(1)*w[1],
        cpn = compute_weigted_precip_cty(2)*w[2],
        cdq = compute_weigted_precip_cty(3)*w[3],
        est = compute_weigted_precip_cty(4)*w[4],
        lan = compute_weigted_precip_cty(5)*w[5],
        lau = compute_weigted_precip_cty(6)*w[6],
        lvl = compute_weigted_precip_cty(7)*w[7],
        mau = compute_weigted_precip_cty(8)*w[8],
        mon = compute_weigted_precip_cty(9)*w[9],
        mtl = compute_weigted_precip_cty(10)*w[10],
        out = compute_weigted_precip_cty(11)*w[11]
    )
    
    weighted_precip_region$total = rowSums(weighted_precip_region[,3:13])
    qc_prcp = weighted_precip_region[,c('year','month','total')]
    write.table(qc_prcp, "./data/qc_prcp.txt")

    qc_prcp = qc_prcp$total[qc_prcp$month==5]
    return(qc_prcp)
}

create_us_precip_data = function(){
    data_ = data.frame(year=1966:2015)

    for (city in c('ny','ch','dm','det')){
        # city for which the temperatures are monthly
        cur_city_data = read.table(paste0('./data/', city,'_prcp.txt'),header=T)
        cur_city_data$PRCP2[cur_city_data$PRCP=='M'] = NA
        cur_city_data$PRCP2[cur_city_data$PRCP=='T'] = as.numeric(0.00) #remplace les 'T' par des 0.0
        cur_city_data$PRCP2[!(cur_city_data$PRCP %in% c('T','M'))] = as.numeric(as.character(cur_city_data$PRCP[!(cur_city_data$PRCP %in% c('T','M'))]))
        cur_city_data$prcp.mm = cur_city_data$PRCP2*25.4 # conversion inch to mm
        cur_city_data$year  = substr(cur_city_data$Date, 1, 4)
        cur_city_data$month = substr(cur_city_data$Date, 6, 7)
        cur_city_data$day = substr(cur_city_data$Date, 9, 10)
        
        # compute monthly precopitation
        cumul = vector(length = 50)
        for (j in 1:50){
            cumul[j] = sum(cur_city_data$prcp.mm[cur_city_data$year==1965+j & cur_city_data$month=='05'])
        }
        
        data_[paste0('prcp_', city)] = cumul
    }

    for (city in c('ral','jack','la','port','dallas')){
        cur_city_data = read.table(file=paste0('./data/', city,'_prcp.csv'), header=TRUE, skip = 0, sep=",")

        cur_city_data$year  = substr(cur_city_data$Date, 5, 8)
        N = length(cur_city_data$Date)
        cur_city_data = cur_city_data[c(1:N),]
        cur_city_data$month = as.character(substr(cur_city_data$Date, 1, 3)) 
        for (k in 1:N){
            cur_city_data$month.num[k] = which(month.abb==cur_city_data$month[k])
        }
        cur_city_data$PRCP2[cur_city_data$Precipitation=='M'] = NA #remplace les 'T' par des 0.0
        cur_city_data$PRCP2[cur_city_data$Precipitation=='T'] = as.numeric(0.00) #remplace les 'T' par des 0.0
        cur_city_data$PRCP2[!(cur_city_data$Precipitation %in% c('T','M'))] = as.numeric(as.character(cur_city_data$Precipitation[!(cur_city_data$Precipitation %in% c('T','M'))]))
        cur_city_data$prcp.mm = cur_city_data$PRCP2*25.4 # conversion pouce->mm
        
        cumul = vector(length = 50)
        for (j in 1:50){
            cumul[j] = cur_city_data$prcp.mm[cur_city_data$year==1965+j & cur_city_data$month.num==5]
        }

        data_[paste0('prcp_', city)] = cumul
    }

    return(data_)
}

qc_precip_data = compute_weighted_precip_region()
us_precip_data = create_us_precip_data()

db_prcp_mai = data.frame(qc_precip_data, us_precip_data)
save(db_prcp_mai, file = './data/db_prcp_mai.Rdata')