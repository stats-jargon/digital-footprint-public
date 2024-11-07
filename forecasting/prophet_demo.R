library(stats)
library(forecast)
library(prophet)

### insert csv file below ###
df <- read.csv('https://raw.githubusercontent.com/facebook/prophet/main/examples/example_wp_log_peyton_manning.csv')
m <- prophet(df, daily.seasonality = TRUE) # prophet model

future <- make_future_dataframe(m, periods = 365) # adding an additional year worth of periods to forecast
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)
prophet_plot_components(m, forecast)
