install.packages('tidyverse')
library(tidyverse)

# 1. Opening daily intensities 3.12-5.12.2016 
dailyIntensities <- read_csv('~/dataset_Bellabeat/Fitabase_Data_2/dailyIntensities_merged.csv')

## Checking the data frame
glimpse(dailyIntensities)

## Filtering rows start with 3, 4, 5
dailyIntensities %>% filter(str_detect(ActivityDay, "^3"))
dailyIntensities %>% filter(str_detect(ActivityDay, "^4"))
dailyIntensities %>% filter(str_detect(ActivityDay, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
dailyIntensities$ActivityDay <- str_replace(dailyIntensities$ActivityDay, "^4.*", "4/12/2016 ")
dailyIntensities$ActivityDay <- str_replace(dailyIntensities$ActivityDay, "^3.*", "3/12/2016 ")
dailyIntensities$ActivityDay <- str_replace(dailyIntensities$ActivityDay, "^5.*", "5/12/2016 ")

levels(as.factor(dailyIntensities$ActivityDay))

## Formatting data types and separating date, time and am/pm values
dailyIntensities$Id <- as.character(dailyIntensities$Id) %>% str_trim()

## Verifying the dates and am/pm 
levels(as.factor(dailyIntensities$ActivityDay))

## Removing duplicates
total <- count(dailyIntensities)
dailyIntensities <- distinct(dailyIntensities)
distinct <- count(dailyIntensities)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
dailyIntensities %>% filter(!complete.cases(.))
dailyIntensities <- na.omit(dailyIntensities)
na.action(dailyIntensities)

## Filtering infinite values
dailyIntensities %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
dailyIntensities <- dailyIntensities %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


## Outliers 
### Selecting numeric columns 
numeric_columns <- dailyIntensities[sapply(dailyIntensities, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(dailyIntensities$Intensities)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(dailyIntensities)

write.csv(dailyIntensities, '~/dataset_Bellabeat/Clean_tables/dailyIntensities_clean.csv', row.names = FALSE)