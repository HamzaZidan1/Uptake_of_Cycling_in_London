### Time Series Analysis on Santander bikes data

###############################################################################

# Import the data

install.packages("readxl")

# Import packages
library(tidyverse) 
library(forecast)
library(tseries)
library(readxl)

# Setting the directory.
setwd(dir='/Users/enorabarbier/Documents/LSE-Thoughtworks/Data/London')

# Import the data set (santander_daily_hire.xlsx).
santander1 <- read_excel("santander_daily_hire.xlsx") 

# Return the data frame.
View(santander1)

# Return the structure, type, class and dimensions of the data frame.
str(santander1)
typeof(santander1)
class(santander1)
dim(santander1)

#################################################################################

# Prepare the data to be converted into a time series

# Plot the data
ggplot(data=santander1, aes(x=Day, y=Number_of_Bicycle_Hires))+
  geom_line(linewidth=0.2)

# Change chr to date
santander1$Date <- ymd(santander1$Day)

# Group cycling data by the month of each year
santander2 = santander1 %>% 
  group_by(month = lubridate::floor_date(Day, 'month')) %>%
  summarize(total_cycles = sum(Number_of_Bicycle_Hires))

#################################################################################

# Convert the data into a time series.

# Create a new data frame and assign time series value,
# and specify the 'ts' function.
santander_ts <- ts(santander2$total_cycles,
                   start = c(2010,8),
                   # Monthly frequency without missing values in data.
                   frequency = 12)

# Plot the time series.
plot(santander_ts)

santander_component <- decompose(santander_ts)
plot(santander_component)

# Test stationarity with augmented ADF test.
adf.test(santander_ts)
# Time series is staionary

# Review random time series variables.
santander_component$random

# Plot values removing NA values while doing so.
autoplot(acf(na.remove(santander_component$random), plot=FALSE)) + 
  # Add a title.
  labs(title="Randomness value") + 
  # Set the theme.
  theme_classic() 

# Plot random variables to check the distribution.
hist(santander_component$random)

#################################################################################

# Use ARIMA modelling to predict cycling for the next 8 months

# Fit the model to our time series. 
arima_santander_ts <- auto.arima(santander_ts)

# Make a forecast for the rest of 2023.
forecast8_san <- forecast(arima_santander_ts, 8)

# Plot the forecast on a graph.
autoplot(forecast8_san) + theme_classic()

# Print the values in the data frame.
forecast8_san

### The forecast predicts a 9% decrease in cycling in 2023 compared to 2022.
### As observed in the time series graph, there has been a decrease in cycling at
### the end of 2022 and start of 2023.

#################################################################################

# Test the forecast ARIMA moddel on the central london data

# (Training data) Create a new time series object
# and assign the values and parameters.
santander_train_ts = window(santander_ts,
                          start = c(2010, 8),
                          end = c(2022, 3),
                          frequency = 12)

# (Test data) Create a new time series object
# and assign the values and parameters.
santander_test_ts = window(santander_ts,
                         start = c(2022, 4),
                         end = c(2023, 4),
                         frequency = 12)

# View new data frames.
head(santander_train_ts)
head(santander_test_ts)

# Create a new object and specify the forecast function and pass the ARIMA model.
forecast_santander_train_ts <- forecast(auto.arima(santander_train_ts), 12)

# Plot the values and forecast and [4] add a theme:
autoplot(forecast_santander_train_ts) + theme_classic()

# Add the autolayer(), specify the data set, and series name.
autoplot(forecast_santander_train_ts) +
  autolayer(santander_train_ts, series='Train') +
  autolayer(santander_test_ts, series='Test') +
  theme_classic()

#################################################################################

# The dip in cycling in December 2022 was overestimated with the model.
