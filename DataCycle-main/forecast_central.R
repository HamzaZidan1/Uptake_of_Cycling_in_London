### Forecast on Central London Dataset

##################################################################################

# Import the data

# Import packages
library(tidyverse) 
library(forecast)
library(tseries)

# Setting the directory.
setwd(dir='/Users/enorabarbier/Documents/LSE-Thoughtworks/Data/London')

# Import the data set (central_london_clean.csv).
central_london_for <- read.csv(file.choose(), header=TRUE) 

# Return the data frame.
View(central_london_for)

# Return the structure, type, class and dimensions of the data frame.
str(central_london_for)
typeof(central_london_for)
class(central_london_for)
dim(central_london_for)

##################################################################################

# Manipulate data before converting it to a time series

# Select Data between 2014 and 2019 
# 2020 and 2021 data collection was impacted by covid so it cannot be used.
forecast1 <- subset(central_london_for, year <= 2019)
dim(forecast1)

# Check the distribution of data by day
table(forecast1['Day'])

# Change chr to date
forecast1$Date <- ymd(forecast1$Date)

# Subset the dataframe  to only include date and total cycles
forecast2 <- forecast1[ -c(1:4,6:11)]

# Group data by month
forecast4 = forecast2 %>% 
  group_by(month = lubridate::floor_date(Date, 'month')) %>%
  summarize(total_cycles = sum(total_cycles))

# Remove any null values
forecast5 <- forecast4[-c(73, 74),]

##################################################################################

# Convert the data into a time series.

# Create a new data frame and assign time series value,
# and specify the 'ts' function.
forecast_cen <- ts(forecast5$total_cycles,
                 start = c(2014,1),
                 # Monthly frequency without missing values in data.
                 frequency = 12)

# Plot the time series.
plot(forecast_cen)

central_component <- decompose(forecast_cen)
plot(central_component)

# Test stationarity with augmented ADF test.
adf.test(forecast_cen)
# Time series is stationary

# Review random time series variables.
central_component$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(central_component$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(central_component$random)

##################################################################################

# Create a forecast for the next year and 3 years.

# Fit the model to our time series. 
arima_forecast_cen <- auto.arima(forecast_cen)

# Make a forecast for the next year.
forecast12_cen <- forecast(arima_forecast_cen, 12)

# Plot the forecast on a graph.
autoplot(forecast12_cen) + theme_classic()

# Print the values in the data frame.
forecast12_cen

# Make a forecast for the next three years.
forecast36_cen <- forecast(arima_forecast_cen, 36)

# Plot the forecast on a graph.
autoplot(forecast36_cen) + theme_classic()

# Print the values in the data frame.
forecast36_cen

#################################################################################

# Code draft
### There was not enough data history to perform an accurate prediction on the
### trend in total cycles.