#### Cleaning the Central London Data

################################################################################

### Import the data

# Import packages
library(tidyverse) 

# Setting the directory.
setwd(dir='/Users/enorabarbier/Documents/LSE-Thoughtworks/Data/London')

# Import the data set (Central London.csv).
central_london <- read.csv(file.choose(), header=TRUE) 

# Return the data frame.
View(central_london)

# Return the structure, type, class and dimensions of the data frame.
str(central_london)
typeof(central_london)
class(central_london)
dim(central_london)

################################################################################

### Clean the Data

# Drop the unnecessary columns
drop <- c("Equivalent.financial.quarter", "Site.ID", "Start.hour", 
          "Start.minute", "Unnamed..14", "Unnamed..15", "Unnamed..16")
central_london2 = central_london[,!(names(central_london) %in% drop)]

# Rename columns to simplify the analysis of the dataset
central_london2 <- central_london2 %>% 
  rename(
    private_cycles = Number.of.private.cycles,
    hire_bike_cycles = Number.of.cycle.hire.bikes,
    total_cycles = Total.cycles
  )

# Check data has been changed correctly
colnames(central_london2)
dim(central_london2)
View(central_londonn2)

# Check for null values (There are 870737 null values)
sum(is.na(central_london2))

# Remove null vlues
central_london2 <- central_london2 %>% drop_na()

# Check for empty cells in DataFrame (There are 21990 empty cells)
sum(central_london2=="")

# Replace empty cells with NA
central_london2[central_london2==""] <- NA

# Check for duplicate rows (There are 3 duplicate values)
sum(duplicated(central_london2))

# Remove duplicate rows
central_london3 <- central_london2[!duplicated(central_london2), ]

# Check duplicate rows have been removed
dim(central_london3)

# Seperate Survey.date column into two columns
central_london4 <- central_london3 %>%
  separate(col= Survey.date, into=c('Day', 'Date'), sep=',')

# Seperate Survey.date column into two columns
central_london5 <- central_london4 %>%
  separate(col=Survey.wave..calendar.quarter., into=c('year', 'quarter', 'month'), sep=' ')

# Drop the quarter column
drop1 <- c("month")
cen_london = central_london5[,!(names(central_london5) %in% drop1)]

# Check column was separated correctly
head(cen_london,n=5)

# Replace days in french with days in english
cen_london$Day<- case_when(
  cen_london$Day == "lun" ~ "mon",
  cen_london$Day == "mar" ~ "tue",
  cen_london$Day == "mer" ~ "wed",
  cen_london$Day == "jeu" ~ "thu",
  cen_london$Day == "ven" ~ "fri",
  cen_london$Day == "sam" ~ "sat",
  cen_london$Day == "dim" ~ "sun",
  TRUE ~ NA
)

# Check days were changed correctly
head(cen_london$Day,n=15)

# Convert character to date format
cen_london$Date <- dmy(cen_london$Date)

# View updated class of date column
class(cen_london$Date)

################################################################################

### Export the clean data set.

# Export the data as a CSV file.
write_csv (cen_london, file='central_london_clean.csv')

################################################################################

### Calculating statistical values

# Determine descriptive statistics of the data set.
summary(cen_london)

DataExplorer::create_report(cen_london)

# Import moments library
library(moments)

### Check shape of private cycles distribution

# Histogram of data
hist(cen_london$private_cycles)
# Shapiro-Wilk test cannot be done on a sample larger than 5000
# Checking for skewness.
skewness(cen_london$private_cycles)
# Our output suggests a very high positive skewness (5.14).
#Checking for kurtosis.
kurtosis(cen_london$private_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of hire bike cycles distribution

# Histogram of data
hist(cen_london$hire_bike_cycles)
# Checking for skewness.
skewness(cen_london$hire_bike_cycles)
# Our output suggests a very high positive skewness (5.15).
#Checking for kurtosis.
kurtosis(cen_london$hire_bike_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of total cycles distribution

# Histogram of data
hist(cen_london$total_cycles)
# Checking for skewness.
skewness(cen_london$total_cycles)
# Our output suggests a very high positive skewness (5.04).
#Checking for kurtosis.
kurtosis(cen_london$total_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.