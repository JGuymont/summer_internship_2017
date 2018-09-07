library(ggmap)

counties = readRDS('./data/counties.rds')

fields = read.table('./data/counties_productions.txt', header = TRUE)

# remove empty space at the begining and at the end of string 
fields$county = substr(as.character(fields$Division), 2, nchar(as.character(fields$Division))-1)

# we need to add 'Regional County Municipality' at the end of 
# counties name in order to find the coordinates
fields$county_long = paste(fields$county,'Regional County Municipality')

# add a flag variable: 'X' means data was not available
# 'O' means data was available
fields$flag[fields$points=='X'] = 'X'
fields$flag[fields$points != 'X'] = 'O'

# create an area size variable
fields$area_size = as.character(fields$points)
fields$area_size[fields$flag=='X'] = 0
fields$area_size = as.numeric(fields$area_size)

add_cty_reformulation = function(data){
    "Reformulate county names to find 
     them in the county database
    "
    data$county.equ = data$county
    data$county.equ[data$county == 'Québec'] = 'Communauté-Urbaine-de-Québec'
    data$county.equ[data$county == 'Sherbrooke'] = 'La Région-Sherbrookoise'
    data$county.equ[data$county == 'Montréal'] = 'Communauté-Urbaine-de-Montréal'
    data$county.equ[data$county == 'Pierre-De Saurel'] = 'Le Bas-Richelieu'
    data$county.equ[data$county == 'Longueuil'] = 'Champlain'
    data$county.equ[data$county == 'Frontenac'] = 'Le Granit'
    data$county.equ[data$county == 'Les Appalaches'] = "L'Amiante"
    return(data)
}


add_regions = function(data){
    # add a variable region
    data$region[data$county.equ %in% counties$est$NAME_2] = 'Estrie'
    data$region[data$county.equ %in% counties$mon$NAME_2] = 'Monteregie'
    data$region[data$county.equ %in% counties$mau$NAME_2] = 'Mauricie'
    data$region[data$county.equ %in% counties$lau$NAME_2] = 'Laurentides'
    data$region[data$county.equ %in% counties$lan$NAME_2] = 'Lanaudière'
    data$region[data$county.equ %in% counties$cdq$NAME_2] = 'C-D-Q'
    data$region[data$county.equ %in% counties$bap$NAME_2] = 'Beauce-Appalache'
    data$region[data$county.equ %in% counties$cpn$NAME_2] = 'Capitale-Nationale'
    data$region[data$county.equ %in% counties$mtl$NAME_2] = 'Montreal'
    data$region[data$county.equ %in% counties$lvl$NAME_2] = 'Laval'
    data$region[data$county.equ %in% counties$out$NAME_2] = 'Outaouais'
    return(data)
}

add_coordinates = function(data){
    coordinates = ggmap::geocode(as.character(data$county_long))
    data$x = coordinates[,1]
    data$y = coordinates[,2]    
    return(data)
}

order_data = function(data){
    data = data[order(data$region), ]
    data = data.frame(x=data[,1], y=data[,2], points=data[,3], flag=data[,4], county=data[,5], region=data[,6])
    data$county_id = 1:length(data$x)
    return(data)
}

fix_coordinates = function(data){
    # Correction de certaines coordonnées

    #Bellechasse
    data[data$county=='Bellechasse', 'x'] <- -70.71667
    data[data$county=='Bellechasse', 'y'] <- 46.73333
    # Francheville
    data[data$county=='Francheville', 'x'] <- -72.55
    data[data$county=='Francheville', 'y'] <- 46.35
    # L'Érable
    data[data$county=="L'Érable", 'x'] <- -71.75
    data[data$county=="L'Érable", 'y'] <- 46.25
    #Acton
    data[data$county=='Acton','x'] <- -72.56667
    data[data$county=='Acton','y'] <- 45.65
    # Montcalm
    data[data$county=='Montcalm','x'] <- -73.66666
    data[data$county=='Montcalm','y'] <- 45.9
    # Matawinie
    data[data$county=='Matawinie','x'] <- -73.78333
    data[data$county=='Matawinie','y'] <- 46.26667
    # Roussillon
    data[data$county=='Roussillon','x'] <- -73.56667
    data[data$county=='Roussillon','y'] <- 45.36666
    #Drummond
    data[data$county=='Drummond','x'] <- -72.48333
    data[data$county=='Drummond','y'] <- 45.88333
    # Becancour
    data[data$county=='Bécancour','x'] <- -72.43 
    data[data$county=='Bécancour','y'] <- 46.33 
    # Maskinongé
    data[data$county=='Maskinongé','x'] <- -72.93333  
    data[data$county=='Maskinongé','y'] <- 46.31667 
    # Argenteuil
    data[data$county=='Argenteuil','x'] <- -74.41666 
    data[data$county=='Argenteuil','y'] <- 45.68333 
    # Le Bas-Richelieu
    data[data$county=='Pierre-De Saurel','x'] <- -73 
    data[data$county=='Pierre-De Saurel','y'] <- 45.96667
    # Le Haut-Richelieu
    data[data$county=='Le Haut-Richelieu','x'] <- -73.23333 
    data[data$county=='Le Haut-Richelieu','y'] <- 45.2
    # Les Maskoutains
    data[data$county=='Les Maskoutains','x'] <- -72.95       
    data[data$county=='Les Maskoutains','y'] <- 45.61666
    # Portneuf, Capitale Nationale       
    data[data$county=='Portneuf','x'] <- -71.91666       
    data[data$county=='Portneuf','y'] <- 46.88333
    # Pontiac, Outaouais
    data[data$county=='Pontiac','x'] <- -76.91666       
    data[data$county=='Pontiac','y'] <- 46.05
    # Les collines de l'Outaouais, Outaouais 
    data[data$county=="Les Collines-de-l'Outaouais",'x'] <- -75.86667       
    data[data$county=="Les Collines-de-l'Outaouais",'y'] <- 45.55
    # Antoine-Labelle, Laurentide
    data[data$county=="Antoine-Labelle",'x'] <- -75.33333            
    data[data$county=="Antoine-Labelle",'y'] <- 46.88333
    # La Vallée-de-la-Gatineau, Outaouais
    data[data$county=="La Vallée-de-la-Gatineau",'x'] <- -76.05                   
    data[data$county=="La Vallée-de-la-Gatineau",'y'] <- 46.53333
    return(data)
}

fields = add_cty_reformulation(fields)
fields = add_regions(fields)
fields = subset(fields, fields$flag %in% c('X','O') & !is.na(fields$region))
fields = add_coordinates(fields)
fields = fix_coordinates(fields)
save(fields, file='./data/fields.Rdata')



