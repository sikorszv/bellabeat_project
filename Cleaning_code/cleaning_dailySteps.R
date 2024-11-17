install.packages('tidyverse')
library(tidyverse)

# 1. Opening daily Steps 3.12-5.12.2016 
dailySteps <- read_csv('~/dataset_Bellabeat/Fitabase_Data_2/dailySteps_merged.csv')

## Checking the data frame
glimpse(dailySteps)

## Filtering rows start with 3, 4, 5
dailySteps %>% filter(str_detect(ActivityDay, "^3"))
dailySteps %>% filter(str_detect(ActivityDay, "^4"))
dailySteps %>% filter(str_detect(ActivityDay, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
dailySteps$ActivityDay <- str_replace(dailySteps$ActivityDay, "^4.*", "4/12/2016 ")
dailySteps$ActivityDay <- str_replace(dailySteps$ActivityDay, "^3.*", "3/12/2016 ")
dailySteps$ActivityDay <- str_replace(dailySteps$ActivityDay, "^5.*", "5/12/2016 ")

levels(as.factor(dailySteps$ActivityDay))

## Formatting data types and separating date, time and am/pm values
dailySteps$Id <- as.character(dailySteps$Id) %>% str_trim()

## Verifying the dates and am/pm 
levels(as.factor(dailySteps$ActivityDay))

## Removing duplicates
total <- count(dailySteps)
dailySteps <- distinct(dailySteps)
distinct <- count(dailySteps)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
dailySteps %>% filter(!complete.cases(.))
dailySteps <- na.omit(dailySteps)
na.action(dailySteps)

## Filtering infinite values
dailySteps %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
dailySteps <- dailySteps %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


## Outliers 
### Selecting numeric columns 
numeric_columns <- dailySteps[sapply(dailySteps, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(dailySteps$Steps)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(dailySteps)

write.csv(dailySteps, '~/dataset_Bellabeat/Clean_tables/dailySteps_clean.csv', row.names = FALSE)