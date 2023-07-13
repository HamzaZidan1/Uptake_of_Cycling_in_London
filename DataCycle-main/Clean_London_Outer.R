#### Cleaning the Outer London Data

################################################################################

### Import the data

# Import packages
library(tidyverse) 

# Setting the directory.
#setwd(dir='/Users/enorabarbier/Documents/LSE-Thoughtworks/Data/London')

# Import the data set (Outer London.csv).
outer_london <- read.csv(file.choose(), header=TRUE) 

# Return the data frame.
View(outer_london)

# Return the structure, type, class and dimensions of the data frame.
str(outer_london)
typeof(outer_london)
class(outer_london)
dim(outer_london)

################################################################################

### Clean the Data

# Drop the unnecessary columns
drop <- c("Site.ID", "Start.hour", "Start.minute")
outer_london2 = outer_london[,!(names(outer_london) %in% drop)]

# Rename columns to simplify the analysis of the dataset
outer_london2 <- outer_london2 %>% 
  rename(
    year = Survey.wave..year.,
    male_cycles = Number.of.male.cycles,
    female_cycles = Number.of.female.cycles,
    unknown_cycles = Number.of.unknown.cycles,
    total_cycles = Total.cycles
  )

# Check data has been changed correctly
colnames(outer_london2)
dim(outer_london2)
View(outer_london2)

# Check for null values (There are 0 null values)
sum(is.na(outer_london2))

# Check for empty cells in DataFrame (There are 2136 empty cells)
sum(outer_london2=="")

# Replace empty cells with NA
outer_london2[outer_london2==""] <- NA

# Check for duplicate rows (There are 47 duplicate values)
sum(duplicated(outer_london2))

# Remove duplicate rows
outer_london3 <- outer_london2[!duplicated(outer_london2), ]

# Check duplicate rows have been removed
dim(outer_london3)

# Separate Survey.date column into two columns
out_london <- outer_london3 %>%
  separate(col=Survey.date, into=c('Day', 'Date'), sep=',')

# Check column was separated correctly
head(out_london,n=5)

# Replace days in french with days in english
out_london$Day<- case_when(
  out_london$Day == "lun" ~ "mon",
  out_london$Day == "mar" ~ "tue",
  out_london$Day == "mer" ~ "wed",
  out_london$Day == "jeu" ~ "thu",
  out_london$Day == "ven" ~ "fri",
  out_london$Day == "sam" ~ "sat",
  out_london$Day == "dim" ~ "sun",
  TRUE ~ NA
  )

# Check days were changed correctly
head(out_london$Day,n=15)

# Convert character to date format
out_london$Date <- dmy(out_london$Date)

# View updated class of date column
class(out_london$Date)

################################################################################

### Export the clean data set.

# Export the data as a CSV file.
write_csv (out_london, file='outer_london_clean.csv')

################################################################################

### Calculating statistical values

# Calculate descriptive statistics of the data set.
summary(out_london)

# Create a report of the dataframe
DataExplorer::create_report(out_london)
# The data is not noramlly distributed

# Import moments library
library(moments)

### Check shape of male cycles distribution

# Histogram of data
hist(out_london$male_cycles)
# Shapiro-Wilk test cannot be done on a sample larger than 5000
# Checking for skewness.
skewness(out_london$male_cycles)
# Our output suggests a very high positive skewness (5.18).
#Checking for kurtosis.
kurtosis(out_london$male_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of female cycles distribution

# Histogram of data
hist(out_london$female_cycles)
# Checking for skewness.
skewness(out_london$female_cycles)
# Our output suggests a very high positive skewness (5.78).
#Checking for kurtosis.
kurtosis(out_london$female_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of total cycles distribution

# Histogram of data
hist(out_london$total_cycles)
# Checking for skewness.
skewness(out_london$total_cycles)
# Our output suggests a very high positive skewness (5.17).
#Checking for kurtosis.
kurtosis(out_london$total_cycles)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.
