ens.forecast <-
function(x, start, frequency, level) {

  tser = ts(x, start=start, frequency=frequency)
  size = length(tser)

  if (length(x) < frequency*2) {
    mod.aa = auto.arima(tser, seasonal=FALSE)
    mod.aach = mod.aa
    mod.ee = ets(tser, model="ZZN")
  } else {
    mod.aa = auto.arima(tser)
    if (length(x) > (frequency+1)) {
      mod.aach = auto.arima(tser, seasonal.test="ch")
    } else {
      mod.aach = mod.aa
    }
    mod.ee = ets(tser)
  }

  if(frequency == 12) {
    h <- 12
  } else if (frequency == 4) {
    h <- 12
  } else if (frequency == 2) {
    h <- 10
  } else if (frequency == 1) {
    h <- 10
  } else {
    h <- 12
  }

  h <- min(size,h)

  fc.aa = forecast(mod.aa, level=level, h=h)
  fc.aach = forecast(mod.aach, level=level, h=h)
  fc.ee = forecast(mod.ee, level=level, h=h)
  mape.aa = accuracy(mod.aa)[["MAPE"]]
  mape.aach = accuracy(mod.aach)[["MAPE"]]
  mape.ee = accuracy(mod.ee)[["MAPE"]]
  mape.aa = ifelse(is.infinite(mape.aa), 1, mape.aa)
  mape.aach = ifelse(is.infinite(mape.aach), 1, mape.aach)
  mape.maa = (mape.aa + mape.aach)/2
  mape.ee = ifelse(is.infinite(mape.ee), 1, mape.ee)
  sum.mape = mape.maa + mape.ee
  if(sum.mape == 0 || is.nan(sum.mape)) {
    upper = fc.ee$upper
    point = fc.ee$mean
    lower = fc.ee$lower
  } else {
    weight.maa = mape.ee/sum.mape
    weight.ee = mape.maa/sum.mape
    upper = (fc.aa$upper*0.5 + fc.aach$upper*0.5)*weight.maa + fc.ee$upper*weight.ee
    point = (fc.aa$mean*0.5 + fc.aach$mean*0.5)*weight.maa + fc.ee$mean*weight.ee
    lower = (fc.aa$lower*0.5 + fc.aach$lower*0.5)*weight.maa + fc.ee$lower*weight.ee
  }
  list(sup=upper,point,inf=lower)
}
