# base de données contenant les info sur les stations canadiennes (nom, Latitude Longitude)
stations = read.table('./data/stations.txt', header = TRUE)

# drop station contient les id des stations sur lesquelles on a aucune données
drop_stations = c(42783,42784,42903,43103,43165,43184,43188,43189,43190,43191,43192,43681,44143,44204,44786,45309, 
                  47587,47888,48188,48228,48268,48288,48289,48371,48374,48968,48970,48971,48974,49028,49029,49288,
                  49308,49390,49491,49608,49648,49649,50090,50719,50720,50822,50841,51139,51157,51297,51457,51638,
                  51698,52038,52080,52081,52138,52179,52199,52201,52378,52603,52605,53001,53002,53018,53019,53140,
                  53158,53159,53338,53978,54018,54038,54059,54061,54063,54065,54067,54069,54071,54073,54118,54298,
                  54299,54300,54558,54559)

stations = stations[!(stations$Id_Station %in% drop_stations),]

save(stations, file='./data/stations.Rdata')

