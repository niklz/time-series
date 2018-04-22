library(forecast)
library(tseries)
data("AirPassengers")


# lets look at the data
plot(AirPassengers)

# boxplot per month

boxplot(AirPassengers~cycle(AirPassengers))

# plot trend in data

plot(aggregate(AirPassengers, FUN = mean))

# plot diffs

plot(diff(AirPassengers))


# From the data we see significant trend, seasonality and unequal variance

# fixing unequal variance with log transform

plot(diff(log(AirPassengers)))

adf.test(diff(log(AirPassengers)))

#AR I MA
#p  d  q

acf(AirPassengers)

acf(diff(log(AirPassengers))) # determines the value of q. q = 1

pacf(diff(log(AirPassengers)))# determines the value of p. p = 0

# d = 1 because it is only required to diff the ts once to achieve stationarity

# now fit ARIMA
#c(p, d, q)
fit <- arima(log(AirPassengers), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1),
                             period = 12)
             )

pred = predict(fit, n.ahead = 10*12)
pred_nat = exp(pred$pred)

ts.plot(AirPassengers, exp(pred$pred), lty = c(1, 3))

# Lets fit on a hold out sample and test performance on a 1 year forecast

AirPassengers_holdout = ts(AirPassengers,
                           frequency = 12,
                           start = c(1949,1),
                           end = c(1959, 12))

fit <- arima(log(AirPassengers_holdout), c(0, 1, 1),
             seasonal = list(order = c(0, 1, 1),
                             period = 12)
)

pred = predict(fit, n.ahead = 12)
pred_nat = exp(pred$pred)
act = ts(AirPassengers,
         frequency = 12,
         start = c(1960,1),
         end = c(1960, 12))

forecast::accuracy(f = pred_nat, 
                   x = act)
