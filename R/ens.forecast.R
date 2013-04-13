ens.forecast <-
function(x, start, frequency, level, h=12) {

  tser = ts(x, start=start, frequency=frequency)

  mod.aa = auto.arima(tser)
  if (length(x) > (frequency+1)) {
    mod.aach = auto.arima(tser, seasonal.test="ch")
  } else {
    mod.aach = auto.arima(tser)
  }
  mod.ee = ets(tser)

  fc.aa = forecast(mod.aa, level=level, h=h)
  fc.aach = forecast(mod.aach, level=level, h=h)
  fc.ee = forecast(mod.ee, level=level, h=h)
  mape.aa = accuracy(mod.aa)[["MAPE"]]
  mape.aach = accuracy(mod.aach)[["MAPE"]]
  mape.maa = (mape.aa + mape.aach)/2
  mape.ee = accuracy(mod.ee)[["MAPE"]]
  sum.mape = mape.maa + mape.ee
  weight.maa = mape.ee/sum.mape
  weight.ee = mape.maa/sum.mape
  upper = (fc.aa$upper*0.5 + fc.aach$upper*0.5)*weight.maa + fc.ee$upper*weight.ee
  point = (fc.aa$mean*0.5 + fc.aach$mean*0.5)*weight.maa + fc.ee$mean*weight.ee
  lower = (fc.aa$lower*0.5 + fc.aach$lower*0.5)*weight.maa + fc.ee$lower*weight.ee
  upper[1]
  list(upper,point,lower)
}
