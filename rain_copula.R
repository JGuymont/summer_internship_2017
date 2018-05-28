load('db_prcp_mai.Rdata') # prov. of Quebec and some US cities monthly precipitation
qc = db.prcp.mai$qc.prcp
ch = db.prcp.mai$prcp.ch
det = db.prcp.mai$prcp.det

u = pobs(as.matrix(cbind(qc,ch))) # rank/(n+1)

selectedCopula = summary(fitCopula(normalCopula(dim=2), u))

param = selectedCopula$coefficients[1]

copula_dist = mvdc(copula=normalCopula(param = param, dim = 2), 
                    margins=c("gamma","weibull"),
                    paramMargins=list(list(shape = shape_qc, scale = scale_qc),
                                      list(shape = shape_ch, scale = scale_ch)))


