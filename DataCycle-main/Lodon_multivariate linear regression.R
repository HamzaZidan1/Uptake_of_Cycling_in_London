# Install and import Tidyverse.
install.packages('tidyverse')
install.packages("skimr")
library(tidyverse)
library(moments)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(DataExplorer)

london_infra_cycle <- read.csv('london_infra_cycle2_p.csv',header=TRUE)
london_infra_cycle

# View data frame .
View(london_infra_cycle)
head(london_infra_cycle)
tail(london_infra_cycle)
dim(london_infra_cycle)
str(london_infra_cycle)

# Check output: Determine the min, max, and mean values.
summary(london_infra_cycle)

# View the descriptive statistics.
skim(london_infra_cycle)

ggplot(data = london_infra_cycle,
       mapping = aes(x=infra_count, y=total_cycles))+
  geom_point(color='red',alpha=0.5,size=1.5)+
  geom_smooth(method='lm')

ggplot(data=london_infra_cycle,
       mapping = aes(x=infra_count,y=total_cycles,
                     color=infra_type))+
  
  geom_point(alpha=0.5, size=1.5)+
  
  geom_smooth(method='lm',
              se=FALSE,
              size=1.5)