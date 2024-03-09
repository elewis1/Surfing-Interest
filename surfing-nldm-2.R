##First attempt for analysis of interest in surfing
##In australia using NLDMs
##Model is mixture of trigonometric and linear model
## using filtering and smoothing

####import "surfing" in Australia since 2004
library(readxl)
surfingAUS <- read_excel("Documents/Fisheries-Github/Surfing-Interest/Surfing-Interest/surfingAUS.xlsx", 
                         skip = 1)

#setup index variable for x - time series
x <- rep(0, length(surfAUS$Month))
for (i in 1:length(x))
{
  x[i] = i
}
surfAUS$x <- x

#plot the timeseries
plot(x = x, y=surfingAUS$`Surfing: (Australia)`, type = "l", lty = 1)
avg = mean(surfAUS$`Surfing: (Australia)`)
sd = sd(surfAUS$`Surfing: (Australia)`)
#forecast w/ 2 trig components: annual (p=12), quarterly (p=3), and
#1st order quadratic

##
#use last 2 years for predictions
T = length(surfAUS$Month) -24
## wrap up all matrices and initial values
## dlm package
library(dlm)

#first order polynominal part
model_poly=dlmModPoly(order=1,dV=250,dW=1,m0=90,C0=sd)
#seasonal part
# peroid = 12 (anual), 2 harmonics
model_seas=dlmModSeas(frequency = 2, dV=250)
model <- model_seas + model_poly
results_filtered_dlm=dlmFilter(surfAUS$`Surfing: (Australia)`[1:T],model)
results_smoothed_dlm=dlmSmooth(results_filtered_dlm)

resid <- residuals(results_filtered_dlm)
plot.ts(resid$res)
abline(h = 0)
new=100
#forecast last 2yrs
results_forecast <- dlmForecast(model, nAhead=24, sampleNew=24)
## obtain 95% credible interval
get_credible_interval <- function(mu, sigma2, 
                                  quantile = c(0.025, 0.975)){
  z_quantile <- qnorm(quantile)
  bound <- matrix(0, nrow=length(mu), ncol=2)
  bound[, 1] <- mu + z_quantile[1]*sqrt(as.numeric(sigma2)) # lower bound
  bound[, 2] <- mu + z_quantile[2]*sqrt(as.numeric(sigma2)) # upper bound
  return(bound)
}
results_smoothed_dlm_interval <- get_credible_interval(mu=results_smoothed_dlm$s[,2], 
                                                       sigma2=results_smoothed_dlm$D.S[,2]^2)
#Plot Data
#s[,2] is expected value of matrix
plot(surfAUS$x, surfAUS$`Surfing: (Australia)`, ylab = "level", 
     main = "Interst in Surfing in Australia",
     type='l', xlab="time (months)",lty=3,ylim=c(0,100))
  points(x[1:T],surfAUS$`Surfing: (Australia)`[1:T],pch=20)
  lines(x[1:(T+1)],results_filtered_dlm$m[,2],col='red',lwd=2)
  lines(x[1:(T+1)],results_smoothed_dlm$s[1:(T+1), 2],col='blue',lwd=2)
  lines(x[1:(T+1)],results_smoothed_dlm_interval[,1],col='red',lwd=2, lty=3)
  lines(x[1:(T+1)],results_smoothed_dlm_interval[,2],col='red',lwd=2,lty=3)
  #Green is forecasted value
  #lines(x[(T+1):(T+24)],results_forecast$f,col='green',lwd=2)
  
  
#We can see annual cycle + linear trend is capture by model
  
#plot components
plot(x[1:(T+1)],results_filtered_dlm$m[,1],pch=20)
  abline(a=0, b=0, col='red')
  lines(x[1:(T+1)],results_smoothed_dlm$m[,1],col='blue',lwd=2)

#Seasonal component and harmonics
plot(x[1:(T)],results_smoothed_dlm$s[1:T, 1],pch=20)
  lines(x[1:(T)],results_smoothed_dlm$s[1:T, 1], col='blue', lwd=3)
  abline(a=0, b=0, col='red')

#Seasonal component and harmonics
plot(x[1:(T)],results_smoothed_dlm$s[1:T, 2],pch=20)
  lines(x[1:(T)],results_smoothed_dlm$s[1:T, 2], col='blue', lwd=3)
  abline(a=0, b=0, col='red') 
#mean of model components
plot(x[1:(T)],results_smoothed_dlm$s[1:T, 5],pch=20)
  lines(x[1:(T)],results_smoothed_dlm$s[1:T, 1], col='blue', lwd=3)

library("COINr")
smooth_ts <- ts(results_smoothed_dlm$s[1:T, 2], start=1)
smooth_prcnt_chng <- (smooth_ts-smooth_ts[1])/smooth_ts[1]*100
plot(smooth_prcnt_chng)
  abline(a=0, b=0, col='red')         
  