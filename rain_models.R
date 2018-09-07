library(fitdistrplus)

load('./data/db_prcp_mai.Rdata')

qc = db_prcp_mai$qc_precip
ch = db_prcp_mai$prcp_ch
det = db_prcp_mai$prcp_det

fn  <- fitdist(qc, 'norm')
fw  <- fitdist(qc, 'weibull')
fln <- fitdist(qc, 'lnorm')
fg  <- fitdist(qc, 'gamma')

fn <-  fitdist(ch, 'norm')
fw <-  fitdist(ch, 'weibull')
fln <- fitdist(ch, 'lnorm')
fg <-  fitdist(ch, 'gamma')

fn  <- fitdist(det, 'norm')
fw  <- fitdist(det, 'weibull')
fln <- fitdist(det, 'lnorm')
fg  <- fitdist(det, 'gamma')

# fit final
fit.qc  <- fitdist(qc, 'gamma')
fit.ch  <- fitdist(ch, 'weibull')
fit.det <- fitdist(det, 'gamma')

shape_qc = fit.qc$estimate[1]
scale_qc = 1/fit.qc$estimate[2]

shape_det = fit.det$estimate[1]
scale_det = 1/fit.det$estimate[2]

shape_ch = fit.ch$estimate[1]
scale_ch = fit.ch$estimate[2]

rain_models = list(
    shape_qc=shape_qc, scale_qc=scale_qc,
    shape_det=shape_det, scale_det=scale_det,
    shape_ch=shape_ch, scale_ch=scale_ch
    )

saveRDS(rain_models, file='./data/rain_models.rds')