####import "surfing" in Australia since 2004
library(readxl)
surfAUS <- read_excel("Documents/Fisheries-Github/Surfing-Interest/Surfing-Interest/surfingAUS.xlsx", 
                         skip = 1)

timeseries_data <- ts(surfAUS)
data = list(yt=surfAUS$`Surfing: (Australia)`)
plot(timeseries_data[,2], xlab="time (months)", ylab="Google hits", main="Interest in 'surfing'")
names(timeseries_data)

#setup index variable for x - time series
x <- rep(0, length(surfAUS$Month))
for (i in 1:length(x))
{
  x[i] = i
}
surfAUS$x <- x
predict = 24
T = length(x)-predict
#plot the timeseries
par(mfrow=c(3, 1))
plot(x = x, y=surfingAUS$`Surfing: (Australia)`, type = "l", lty = 1,
     main="Google hits", ylab="Google hits")
avg = mean(surfAUS$`Surfing: (Australia)`)
var = var(surfAUS$`Surfing: (Australia)`)
surf.ts <- ts(data=surfingAUS)
#acf and pacf
acf(surf.ts[,2], lag.max = 20, xlab = "lag",
    ylab = "Sample ACF",ylim=c(-1,1),main="ACF")
pacf(surf.ts[,2], lag.max = 20,xlab = "lag",
     ylab = "Sample PACF",ylim=c(-1,1),main="PACF")

#Modeling
library(dlm) 

model_seasonal=dlmModTrig(s=12,q=4,dV=0,dW=1) 
model_trend=dlmModPoly(order=2,dV=10,dW=rep(1,2),m0=c(90,0)) 
model=model_trend + model_seasonal

#model$C0=10*diag(10)
n0=1 #df
S0=20 #obs variance
k=length(model$m0) 
T=length(timeseries_data)

Ft=array(0, c(1,k,T))
Gt=array(0,c(k,k,T))

for(t in 1:T){
  Ft[,,t]=model$FF
  Gt[,,t]=model$GG
}

source("~/Documents/Fisheries-Github/Surfing-Interest/Surfing-Interest/discount_factor_selection.R")
source("~/Documents/Fisheries-Github/Surfing-Interest/Surfing-Interest/all_dlm_functions_unkown_v.R")


matrices=set_up_dlm_matrices_unknown_v(Ft=Ft, Gt=Gt)
initial_states=set_up_initial_states_unknown_v(model$m0, model$C0,
                                               n0,S0)
df_range=seq(0.8,1,by=0.005)

##fit discount DLM
## MSE
results_MSE <- adaptive_dlm(data, matrices, initial_states,
                            df_range, "MSE", forecast=FALSE)
results_MSE$df_opt #optimum discount facotr

#retreive filtered results
results_filtered <- results_MSE$results_filtered
ci_filtered <- get_credible_interval_unknown_v(
  results_filtered$ft, results_filtered$Qt, results_filtered$nt)

#retrieve smoothed results
results_smoothed <- results_MSE$results_smoothed
ci_smoothed <- get_credible_interval_unknown_v(
  results_smoothed$fnt, results_smoothed$Qnt,
  results_filtered$nt[length(results_smoothed$fnt)])

##plot smoothing results
par(mfrow=c(1,1))
index <- timeseries_data[,3]
plot(index, data$yt, ylab='Google hits',
     main = "Google Trends: time series", type = 'l',
     xlab = 'time', lty=3,ylim=c(0,100))
  lines(x=index, y=results_smoothed$fnt, type = 'l', col='blue', 
      lwd=2)
  lines(x=index, y=ci_smoothed[, 1], type='l', col='blue', lty=2)
  lines(x=index, y=ci_smoothed[, 2], type='l', col='blue', lty=2)
  


##trend and rate of change
par(mfrow=c(2,1))
plot(x=index, y=data$yt, pch=19, cex=0.3, col='lightgray', xlab="time",
  ylab="Google hits", main="trend")
lines(x=index, y=results_smoothed$mnt[,1], lwd=2, col='magenta', type='l')
plot(x=index, y=results_smoothed$mnt[,2], lwd=2, col='darkblue',
     type='l', xlab="time", ylab="rate of change", ylim=c(-1,1))
  abline(h=0, col='red', lty=2)

##seasonal components
par(mfrow=c(2,2))
plot(index, results_smoothed$mnt[,3], lwd=2, col='darkgreen',
     type='l', xlab="time", ylab="", main="period=12",
     ylim=c(-12,12))
plot(index, results_smoothed$mnt[,5], lwd=2, col='darkgreen',
     type='l', xlab="time", ylab="", main="period=6",
     ylim=c(-12,12))
plot(index, results_smoothed$mnt[,7], lwd=2, col='darkgreen',
     type='l', xlab="time", ylab="", main="period=4",
     ylim=c(-12,12))  
plot(index, results_smoothed$mnt[,9], lwd=2, col='darkgreen',
     type='l', xlab="time", ylab="", main="period=2",
     ylim=c(-12,12))  
##estimate for observational varaince at observational level
results_filtered$St[T]
