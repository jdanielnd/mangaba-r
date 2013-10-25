ens.forecast <-
function(x, start, frequency, level, xreg=NULL) {

  # Creating Time Series object
  tser = ts(x, start=start, frequency=frequency)
  # Getting series size
  size = length(tser)


  # Seasonal is disabled if the size is smaller then 2*frequency
  if (length(x) < frequency*2) {
    mod.aa = auto.arima(tser, seasonal=FALSE, xreg=xreg)
    mod.aach = mod.aa
    mod.ee = ets(tser, model="ZZN")
  } else {

    ## Try Canova-Hansen test, if any error happens, use OCSB test.
    mod.aach = NULL
    try({mod.aach = auto.arima(tser, seasonal.test="ch", xreg=xreg)}, silent=TRUE)
    if(is.null(mod.aach)) {
      mod.aach = auto.arima(tser, xreg=xreg)  
    }
    
    # If length is smaller than freq*2+5 both arima are ch
    # Else uses OCSB test
    if(length(x)>frequency*2+5){
      mod.aa = auto.arima(tser, xreg=xreg)
    } else {
      mod.aa = mod.aach
    }
    mod.ee = ets(tser)
  }

  # Set the default horizont for each type of frequency
  if(frequency == 12) {
    h <- 18
  } else if (frequency == 4) {
    h <- 12
  } else if (frequency == 2) {
    h <- 10
  } else if (frequency == 1) {
    h <- 10
  } else {
    h <- 12
  }

  # Set the horizont to the minimun between the series size and the default horizont
  h <- min(size,h)

  # Calculate forecasts
  if(is.null(xreg)){
    fc.aa = forecast(mod.aa, level=level, h=h)
    fc.aach = forecast(mod.aach, level=level, h=h)
  } else {
    fc.aa = forecast(mod.aa, level=level, h=h, xreg=rep(0,h))
    fc.aach = forecast(mod.aach, level=level, h=h, xreg=rep(0,h))
  }
  fc.ee = forecast(mod.ee, level=level, h=h)

  # Calculate MAPE's
  mae.aa = accuracy(mod.aa)[,"MAE"]
  mae.aach = accuracy(mod.aach)[,"MAE"]
  mae.ee = accuracy(mod.ee)[,"MAE"]

  # Correct MAPE's - If it's infinite, replace by 1
  # We don't need to correct the MAPE anymore, because MAE doesn't have
  # the 0/infinity problem
  # mae.aa = ifelse(is.infinite(mape.aa), 1, mape.aa)
  # mae.aach = ifelse(is.infinite(mape.aach), 1, mape.aach)
  # mae.ee = ifelse(is.infinite(mape.ee), 1, mape.ee)
  
  # Calculate mean MAE of ARIMAS
  mae.maa = (mae.aa + mae.aach)/2

  # Calculate sum of MAE's
  sum.mae = mae.maa + mae.ee
  
  # If MAPE's are zero or NaN use ETS
  # We don't need this correction anymore, because
  # the MAE always exists.
  # if(sum.mae == 0 || is.nan(sum.me)) {
  #   upper = fc.ee$upper
  #   point = fc.ee$mean
  #   lower = fc.ee$lower
  # } else {
  # Else combine models  
    weight.maa = mae.ee/sum.mae
    weight.ee = mae.maa/sum.mae
    upper = (fc.aa$upper*0.5 + fc.aach$upper*0.5)*weight.maa + fc.ee$upper*weight.ee
    point = (fc.aa$mean*0.5 + fc.aach$mean*0.5)*weight.maa + fc.ee$mean*weight.ee
    lower = (fc.aa$lower*0.5 + fc.aach$lower*0.5)*weight.maa + fc.ee$lower*weight.ee
  # }

  # Calculate MAPE of ensembled model
  
  # Get fitted of each model
  fitted.aa <- fitted(mod.aa)
  fitted.aach <- fitted(mod.aach)
  fitted.ee <- fitted(mod.ee)

  # Calculate fitted of ensebled model
  fitted.ens <- (fitted.aa*0.5 + fitted.aach*0.5)*weight.maa + fitted.ee*weight.ee
  errors <- accuracy(f=fitted.ens, x=tser)[,c("MAE","MAPE")]

  # Calculate effect size if xreg in not null
  if(is.null(xreg)){
    effect <- 0
  } else {
    effect.aa <- coef(mod.aa)[["xreg"]]
    effect.aach <- coef(mod.aach)[["xreg"]]
    effect <- mean(c(effect.aa,effect.aach))
  }

  list(sup=upper,point,inf=lower,errors=errors,effect=effect)
}
