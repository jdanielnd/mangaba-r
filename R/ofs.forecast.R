# ofs.forecast <- function(x, start, frequency, level, hide = 4) {
# 	xl = length(x)
# 	xc = x[1:(xl-hide)]
# 	xr = x[(xl-hide+1):xl]
# 	fc = ens.forecast(xc, start, frequency, level)
# 	df = data.frame(xc=c(xc,xr), sup=c(rep(NA,xl-hide),fc[[1]][1:hide]), point=c(rep(NA,xl-hide),fc[[2]][1:hide]), inf=c(rep(NA,xl-hide),fc[[3]][1:hide]))
# 	dts <- ts(df,start=start,frequency=frequency)
# 	# plot(dts,plot.type="single")
# 	dts
# }

# plot.ofs <- function(x) {
# 	xyplot(x,superpose=T)
# }

# mape.ofs <- function(x) {
# 	hide = length(na.omit(x[,2]))
# 	real = x[(nrow(x)-hide+1):nrow(x),1]
# 	fitted = x[(nrow(x)-hide+1):nrow(x),3]
# 	mean(abs(fitted-real)/real)
# }

# # dfl <- lapply(df,function(x) {
# # 	ofs.forecast(x,c(2010,3),4,80,4)
# # })