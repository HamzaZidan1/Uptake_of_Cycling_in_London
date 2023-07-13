# The necessary libraries for analysis are imported:
library(tidyverse)
library(plotly)
library(ggplot2)

# Transport emissions data is imported into a dataframe.
# The data has previously been cleaned using Excel to only include the
# required data from a larger emissions dataset:
emissions <- read.csv('Transport Emmissions by Borough.csv', header = TRUE)

# The data is viewed and sense-checked:
View(emissions)

# The data is reshaped for analysis into long form, so that it can be analysed
# by borough:
library(reshape2)
emissions_long <- melt(emissions,id.vars="Borough")

View(emissions_long)

# The column names are changed to make more sense for the analysis:
emissions_long <- emissions_long %>%
  rename(year_emissions = variable, 
         volume_co2 = value)


# The dplyr library is imported:
library(dplyr)
# The data is then filtered to include only the top 5 boroughs for
# emissions.  These were identified using Excel pivot tables:
boroughs <- c("Barnet", "Enfield", "Havering", "Hillingdon", "Hounslow")
emissions5 <- filter(emissions_long, Borough %in% boroughs)

# The new data frame is sense-checked:
View(emissions5)
class(emissions5$Borough)
class(emissions5$year_emissions)
class(emissions5$volume_co2)

# A bar plot is then created to visualise the top boroughs on an interactive
# chart:
yearschart <- ggplot(emissions5, aes(x=Borough, y=volume_co2, fill=year_emissions)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top 5 Boroughs for CO2 Emissions")
ggplotly(yearschart)


