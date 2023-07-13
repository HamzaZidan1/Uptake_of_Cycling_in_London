#### Cleaning the Inner London Data
# Hussein 

################################################################################

### Import the data

# Import packages
library(tidyverse) 

# Setting the directory.
#setwd(dir="/Users/husseinfarook/Desktop/Data science course/Employer Project/
#LSE_DA_Employer_Project_TW_London/London")

# Import the data set (Inner London.csv).
inner_london <- read.csv(file.choose(), header=TRUE) 

# Return the data frame.
View(inner_london)

# Return the structure, type, class and dimensions of the data frame.
str(inner_london)
typeof(inner_london)
class(inner_london)
dim(inner_london)

################################################################################

### Clean the Data

# Drop the unnecessary columns
drop <- c("Site.ID", "Start.hour", "Start.minute")
inner_london2 = inner_london[,!(names(inner_london) %in% drop)]

# Rename columns to simplify the analysis of the dataset
inner_london2 <- inner_london2 %>% 
  rename(
    year = Survey.wave..year.,
    private_cycles = Number.of.private.cycles,
    hire_cycles = Number.of.cycle.hire.bikes,
    total_cycles = Total.cycles
  )

# Check data has been changed correctly
colnames(inner_london2)
dim(inner_london2)
View(inner_london2)

# Check for null values (There are 365,568 null values)
sum(is.na(inner_london2))

# Check for empty cells in DataFrame (There are 0 empty cells)
sum(inner_london2=="")

# Replace null cells with NA
inner_london2[is.null(inner_london2)] <- NA

# Check for duplicate rows (There are 106,463 duplicate values)
sum(duplicated(inner_london2))

# Remove duplicate rows
inner_london3 <- inner_london2[!duplicated(inner_london2), ]

# Check duplicate rows have been removed
dim(inner_london3)

# Separate Survey.date column into two columns
in_london <- inner_london3 %>%
  separate(col=Survey.date, into=c('Day', 'Date'), sep=',')

# Check column was separated correctly
head(in_london,n=5)

# Replace days in french with days in english
in_london$Day<- case_when(
  in_london$Day == "lun" ~ "mon",
  in_london$Day == "mar" ~ "tue",
  in_london$Day == "mer" ~ "wed",
  in_london$Day == "jeu" ~ "thu",
  in_london$Day == "ven" ~ "fri",
  in_london$Day == "sam" ~ "sat",
  in_london$Day == "dim" ~ "sun",
  TRUE ~ NA
)

# Check days were changed correctly
view(in_london$Day)

# Convert character to date format
in_london$Date <- dmy(in_london$Date)

# View updated class of date column
class(in_london$Date)

################################################################################

### Export the clean data set.

# Export the data as a CSV file.
write_csv (in_london, file='inner_london_clean.csv')

################################################################################

### Calculating statistical values

# Calculate descriptive statistics of the data set.
summary(in_london)

# Create a report of the dataframe
DataExplorer::create_report(in_london)
# The data is not noramlly distributed

# Import moments library
library(moments)

### Check shape of private cycles distribution

# Histogram of data
hist(in_london$private_cycles)
# Shapiro-Wilk test cannot be done on a sample larger than 5000
# Checking for skewness.
skewness(in_london$private_cycles, na.rm = TRUE)
# Our output suggests a very high positive skewness (5.83).
#Checking for kurtosis.
kurtosis(in_london$private_cycles, na.rm = TRUE)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of hire cycles distribution

# Histogram of data
hist(in_london$hire_cycles)
# Checking for skewness.
skewness(in_london$hire_cycles, na.rm = TRUE)
# Our output suggests a very high positive skewness (6.78).
#Checking for kurtosis.
kurtosis(in_london$hire_cycles, na.rm = TRUE)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

### Check shape of total cycles distribution

# Histogram of data
hist(in_london$total_cycles)
# Checking for skewness.
skewness(in_london$total_cycles, na.rm = TRUE)
# Our output suggests a very high positive skewness (5.74).
#Checking for kurtosis.
kurtosis(in_london$total_cycles, na.rm = TRUE)
# Our kurtosis value is more than 3, suggesting our data is leptokurtic.

# Log of data to adjust distribution
hist(log(in_london$total_cycles))

# Square root of data to adjust distribution
hist(sqrt(in_london$total_cycles))

in_london <- read.csv(file.choose(), header = TRUE)
view(in_london)

length(unique(in_london$Location))
print(unique(in_london$Location))
print(unique(in_london$Direction))
print(unique(in_london$year))


ggplot(in_london, aes(x = year, y = total_cycles, fill = year)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart", x = "Year", y = "Total Cycles")
