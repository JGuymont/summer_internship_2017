library(copula)

load('./data/db_prcp_mai.Rdata') # prov. of Quebec and some US cities monthly precipitation

fit_copula_dist = function(rain_models) {
    qc = db_prcp_mai$qc_precip_data
    ch = db_prcp_mai$prcp_ch

    u = pobs(as.matrix(cbind(qc, ch))) # rank/(n+1)

    selectedCopula = summary(copula::fitCopula(normalCopula(dim=2), u))

    param = selectedCopula$coefficients[1]

    copula_dist = mvdc(
        copula=normalCopula(param=param, dim=2),
        margins=c("gamma", "weibull"),
        paramMargins=list(
            list(shape=rain_models$shape_qc, scale=rain_models$scale_qc),
            list(shape=rain_models$shape_ch, scale=rain_models$scale_ch)
        )
    )
    
    return(copula_dist)
}



