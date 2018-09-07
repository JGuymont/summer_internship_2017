require(ggmap)
require(rworldmap)
require(raster)
require(maps)

# mapping quebec
can1 = raster::getData('GADM', country="CAN", level=1) # provinces
can2 = raster::getData('GADM', country="CAN", level=2) # county

qc1  = can1[can1$NAME_1 %in% c('Québec'),]
qc2  = can2[can2$NAME_1 %in% c('Québec'),]

# les comptés sont séparés par régions administratives
mau.cty = c('Maskinongé','Francheville','Mékinac',"Le Centre-de-la-Mauricie")
cpn.cty = c("Communauté-Urbaine-de-Québec",'Charlevoix','Charlevoix-Est',"L'Île-d'Orléans",
            "La Côte-de-Beaupré",'La Jacques-Cartier','Portneuf')
est.cty = c('Coaticook','Le Granit','Le Haut-Saint-François','Le Val-Saint-François',
            'Asbestos','Memphrémagog','La Région-Sherbrookoise')
mon.cty = c('Acton','Brome-Missisquoi','La Haute-Yamaska','La Vallée-du-Richelieu',
            'Le Haut-Richelieu','Les Maskoutains','Lajemmerais', 'Le Bas-Richelieu','Rouville', 
            'Le Haut-Saint-Laurent','Vaudreuil-Soulanges','Roussillon', 'Les Jardins-de-Napierville', 'Beauharnois-Salaberry','Champlain')
bap.cty = c('Beauce-Sartigan','Bellechasse','La Nouvelle-Beauce',"L'Amiante",'Les Etchemins',"L'Islet",
            'Lotbinière','Montmagny','Robert-Cliche',"Les Chutes-de-la-Chaudière",'Desjardins')
lan.cty = c("D'Autray",'Joliette',"L'Assomption",'Les Moulins','Matawinie','Montcalm')
lau.cty = c('Antoine-Labelle','Argenteuil','Deux-Montagnes','La Rivière-du-Nord','Les Laurentides',
            "Les Pays-d'en-Haut",'Mirabel','Thérèse-De Blainville')
cdq.cty = c('Arthabaska','Bécancour','Drummond',"L'Érable",'Nicolet-Yamaska')
mtl.cty = c('Communauté-Urbaine-de-Montréal')
lvl.cty = c('Laval')
out.cty = c('La Vallée-de-la-Gatineau',"Les Collines-de-l'Outaouais",'Papineau','Pontiac','Gatineau',
            "Communauté-Urbaine-de-l'Outaouais")
cty.all = c(mau.cty,lau.cty,lan.cty,est.cty,bap.cty,cdq.cty,mon.cty,cpn.cty, mtl.cty,lvl.cty,out.cty)

est = qc2[qc2$NAME_2 %in% est.cty,] 
mon = qc2[qc2$NAME_2 %in% mon.cty,]
cdq = qc2[qc2$NAME_2 %in% cdq.cty,]
lau = qc2[qc2$NAME_2 %in% lau.cty,]
lan = qc2[qc2$NAME_2 %in% lan.cty,]
bap = qc2[qc2$NAME_2 %in% bap.cty,]
mau = qc2[qc2$NAME_2 %in% mau.cty,]
cpn = qc2[qc2$NAME_2 %in% cpn.cty,]
mtl = qc2[qc2$NAME_2 %in% mtl.cty,]
lvl = qc2[qc2$NAME_2 %in% lvl.cty,]
out = qc2[qc2$NAME_2 %in% out.cty,]

saveRDS(list(est=est, mon=mon, cdq=cdq, lau=lau, lan=lan, bap=bap, mau=mau, cpn=cpn, mtl=mtl, lvl=lvl, out=out),
        file='./data/counties.rds')