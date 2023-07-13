### Performing regression analysis on normalised and non-normailsed data

# Load the tidyverse library.
library(tidyverse)


# Set your working directory.
# Import the data set.
central <- read.csv(file.choose(), header=T)


## Explore the data set.
summary(central)
head(central)


###############################################################################

# This data set has already been cleaned.

###############################################################################

# Identify relationships between total cycles and hire bike cycles

# Plot the relationship 
plot(central$hire_bike_cycles, central$total_cycles)
plot(central$hire_bike_cycles, central$private_cycles)


###############################################################################

# Fit a simple linear regression model.

# Create a model with only one x variable.
model1 <- lm(total_cycles~hire_bike_cycles,
             data=central)


# View the model.
model1


# View more outputs for the model - the full regression table.
summary(model1)

# The variability of hire cycles explains 57.1% of the variability of total cycles.


###############################################################################

# Plot the model.

# View residuals on a plot.
plot(model1$residuals)


# Plot the relationship 
plot(central$hire_bike_cycles, central$total_cycles, pch = 16)
coefficients(model1)


# Add line-of-best-fit.
abline(coefficients(model1),col='red', lwd=2)
title("Relationship between hire bike cycles and total cycles", cex.main=1)


###############################################################################

# Create a log transformation.

# A log transformation cannot be done on 0 values
central1 <- filter(central, hire_bike_cycles > 0, private_cycles > 0, total_cycles > 0)

head(central1)

# Complete a log transformation with dplyr's mutate() function.
central1 <- mutate(central1, 
              loghire=log(hire_bike_cycles),
              logtotal=log(total_cycles))


# View new object with new variable.
head(central1)


# Create a new model using logIndex.
model2 <- lm(logtotal~loghire,
             data=central1)

# The variability of hire cycles explains 47.9% of the variability of total cycles.


# View full regression table.
summary(model2)


# Plot the relationship 
plot(central1$loghire, central1$logtotal)


# Add a line-of-best fit to existing plot.
abline(coefficients(model2))


###############################################################################

## Normalising data does not make the regression analysis.
# The rest of the analysis will be performed on the non-normalised data.
