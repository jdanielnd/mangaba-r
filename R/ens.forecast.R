ens.forecast <-
function(x, start, frequency, level) {
  tser = ts(x, start=start, frequency=frequency)
  mod.aa = auto.arima(x, seasonal.test="ch")
  mod.ee = ets(x)
  fc.aa = forecast(mod.aa, level=level)
  fc.ee = forecast(mod.ee, level=level)
  mape.aa = accuracy(mod.aa)[["MAPE"]]
  mape.ee = accuracy(mod.ee)[["MAPE"]]
  sum.mape = mape.aa + mape.ee
  weight.aa = mape.ee/sum.mape
  weight.ee = mape.aa/sum.mape
  upper = fc.aa$upper*weight.aa + fc.ee$upper*weight.ee
  point = fc.aa$mean*weight.aa + fc.ee$mean*weight.ee
  lower = fc.aa$lower*weight.aa + fc.ee$lower*weight.ee
  list(upper,point,lower)
}
