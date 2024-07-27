# create ts tibble
test <- quarter_revenue %>% 
     mutate(date = yearquarter(date)) %>% 
     as_tsibble(index = date)

# test two plot of two different date data type
# first POSIXt
quarter_revenue %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  labs(title = "GameStop Revenue",
       subtitle = "From 2012 - 2024")

quarter_revenue_tibble %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  labs(title = "GameStop Revenue",
       subtitle = "From 2012 - 2024")

autoplot(quarter_revenue_tibble) +
  labs(title = "GameStop Revenue",
       subtitle = "From 2012 - 2024")

# seasonal plot for revenue data set
gg_season(quarter_revenue_tibble, y = value)
# another seasonal plot that feasts provide
# us gg_subseries(3333335666666666666)

# see lag of our data set
quarter_revenue_tibble |>
  gg_lag(value, geom = "point")

# see ACF (Auto Correlation Factor) in our data set
quarter_revenue_tibble |>
  ACF(value, lag_max = 9) |>
  autoplot() +
  labs(title = "ACF of GameStop Revenue Q-10",
       subtitle = "From 2012 - 2024")

# ploting time series decomposition methods
# stl
test <- quarter_revenue_tibble |>
  model(stl = STL(value))

components(test) +
  autoplot()

components(test) |>
  as_tsibble() |>
  autoplot(value, color = "#45474B") +
  geom_line(aes(y = trend), color = "#D44033") +
  geom_line(aes(y = season_adjust), color = "#0072B2") +
  labs(
    y = "Revenue",
    title = "GameStop Revenue 10-Q Report",
    subtitle = "From 2012 - 2024"
  )

# using seperate STL decomposition
components(test) |>
  autoplot()

# Create train data
train <- quarter_revenue_tibble |>
  filter_index("2012 Q1" ~ "2020 Q2")

# Create model table (Mable) and forecast
test <- quarter_revenue_tibble |>
  model(
    mean = MEAN(value),
    naive = NAIVE(value),
    seasonal_naive = SNAIVE(value)
  ) |>
  forecast(h = 14)

test |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(quarter_revenue_tibble, '2020 Q1' ~ .),
    value,
    color = "black"
  ) +
  labs(
    y = 'Revenue',
    title = 'Forecast GameStop Revenue 10-Q',
    subtitle = 'From 2012 - 2026',
    color = 'Forecast Method'
  )

# create data frame of all the benchmark models that we'll use
# Extract training data set
quarter_revenue_train <- quarter_revenue_tibble |>
  filter_index(. ~ '2022 Q1')

# Train the models
revenue_models <- quarter_revenue_train |>
  model(
    mean = MEAN(value),
    naive = NAIVE(value),
    snaive = SNAIVE(value),
    drift = RW(value ~ drift())
  )

# forecase based on the models
revenue_forecast <- 
  revenue_models |>
  forecast(h = 12)

# Plot the forecast against the full data
revenue_forecast |>
  autoplot(filter_index(quarter_revenue_tibble), level = NULL)

# check the accuracy of our benchmark forecast
revenue_forecast |>
  accuracy(quarter_revenue_tibble)

# Now the second forecast method
# Exponential Smoothing (ETS) <- E(rror), T(rend), S(easonal)
fit <- quarter_revenue_tibble |>
  model(ETS = ETS(value),
        snaive = SNAIVE(value))
report(fit) # Model : ETS(M, N, M)
## trying to put other component that I think more suitable

# see their ETS(MNM) component
components(fit) |>
  autoplot() +
  labs(title = "GameStop Revenue with ETS(MNM) model")

# check the accuracy of ETS(MNM) model
accuracy(fit)

# Test the training data against test data
fc <- fit |> forecast(h = 12)
fc |> autoplot(quarter_revenue_tibble, level = NULL) +
  labs(title = "GameStop Revenue Forecasting using ETS(MNM) and SNAIVE model",
       y = "Revenue") +
  guides(color = guide_legend(title = "Forecast:"))

# Create forecast using ETS model
ets <- quarter_revenue_tibble |>
  model(ETS(value))

ets |>
  forecast(h = 10) |>
  autoplot(quarter_revenue_tibble) +
  labs(y = "Value",
       title = "GameStop Forecast For the Next 2 years",
       subtitle = "Using ETS Model")

# bonus
## Check the coefficient between ETS and snaive model forecast
## against the real data

# Create training data set
test <- quarter_revenue_train |>
  model(
    snaive = SNAIVE(value),
    ets = ETS(value)
    )

# Create test data set
test_fc <- test |>
  forecast(h = 10)

# autoplot this two variable
test_fc |>
  autoplot(filter_index(quarter_revenue_tibble), level = NULL) +
  labs(title = "GameStop Revenue forecast with ETS and SNAIVE model",
       subtitle = "Against real data",
       y = "Revenue",
       x = "")

## Test another model (Regression Model)
# Create the model
regression_md <- quarter_revenue_tibble |>
  model(
    TSLM(value ~ trend() + season())
    )
report(regression_md) # report all the information of time series

# create scatterplot of quarterly report of GameStop revenue
# This is regression graph that tells you if you have any trend in your dataset
# The same as gg_season() function but in regression
augment(regression_md) |>
  ggplot(aes(x = value, y = .fitted,
             color = factor(quarter(date)))) +
  geom_point() +
  labs(y = "Fitted",
       x = "Values",
       title = "GameStop Revenue in Quarter") +
  geom_abline(intercept = 0, slope = 1) +
  guides(color = guide_legend(title = "Quarter:"))

## Test the last forecast model (ARIMA)
quarter_revenue_tibble |>
  gg_tsdisplay(difference(value), plot_type = 'partial') #MA = 4

# Finding the best ARIMA model
# using automate method and manualy
arima <- quarter_revenue_tibble |>
  model(ARIMA(value))
report(arima)

# plot the ARIMA (1,0,0)(0,1,1)[4] w/ drift
arima |>
  forecast(h = 12) |>
  autoplot(quarter_revenue_tibble) +
  labs(y = "Values",
       title = "GameStop Revenue Forcast",
       subtitle = "Using ARIMA Model")

# Check which is better ETS or ARIMA models
# Cross validation
quarter_revenue_tibble |>
  stretch_tsibble(.init = 15) |>
  model(
    ETS(value),
    ARIMA(value)
  ) |>
  forecast(h = 5) |>
  accuracy(quarter_revenue_tibble) |>
  select(.model, RMSE:MAPE) # ARIMA have higher more accuracy than ETS

# See the accuracy of SNaive, ETS, and ARIMA
quarter_revenue_tibble |>
  stretch_tsibble(.init = 15) |>
  model(SNAIVE(value),
        ETS(value),
        ARIMA(value)) |>
  forecast(h = 5) |>
  accuracy(quarter_revenue_tibble) |>
  select(.model, RMSE:MAPE)


##########################################################################
# So, the final decision is we used ARIMA (1,0,0)(0,1,1) model
# as our forecast method.
##########################################################################

# Create train data
train <- quarter_revenue_tibble |>
  filter_index(. ~ "2022 Q1")

# Create model table
arima <- train |>
  model(ARIMA(value)) |>

# forecast data using ARIMA model for the next 5 quarter report
fc <- arima |>
  forecast(h = 10)

fc |>
  autoplot(filter_index(quarter_revenue_tibble), level = NULL) +
  labs(title = "GameStop Forecasting Revenue",
       y = "USD in Million")