# 4. Merging hourlyIntensities 3.12.-5.12.2016

install.packages('tidyverse')
library(tidyverse)

hourlyIntensities_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/hourlyIntensities_merged.csv')
hourlyIntensities_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/hourlyIntensities_merged.csv')
hourlyIntensities_merged <- rbind(hourlyIntensities_1, hourlyIntensities_2)

glimpse(hourlyIntensities_merged)

## Filtering rows start with 3, 4, 5
hourlyIntensities_merged %>% filter(str_detect(ActivityHour, "^3"))
hourlyIntensities_merged %>% filter(str_detect(ActivityHour, "^4"))
hourlyIntensities_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
hourlyIntensities_merged$ActivityHour <- str_replace(hourlyIntensities_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
hourlyIntensities_merged$ActivityHour <- str_replace(hourlyIntensities_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
hourlyIntensities_merged$ActivityHour <- str_replace(hourlyIntensities_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

levels(as.factor(hourlyIntensities_merged$ActivityHour))

## Formatting data types and separating date, time and am/pm values
hourlyIntensities_merged$Id <- as.character(hourlyIntensities_merged$Id) %>% str_trim()
hourlyIntensities_merged$Date <- sub(" .*", "", hourlyIntensities_merged$ActivityHour)
hourlyIntensities_merged$Hour <- sub(".*? ", "", hourlyIntensities_merged$ActivityHour)
hourlyIntensities_merged$Hour_pure <- sub(" .*", "", hourlyIntensities_merged$Hour)
hourlyIntensities_merged$AM_PM <- sub(".* ", "", hourlyIntensities_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(hourlyIntensities_merged$Date))
levels(as.factor(hourlyIntensities_merged$Hour))
levels(as.factor(hourlyIntensities_merged$Hour_pure))
levels(as.factor(hourlyIntensities_merged$AM))

head(hourlyIntensities_merged)

## Re-order columns 
col_order <- c("Id", "ActivityHour", "Date",
               "Hour", "Hour_pure","AM_PM", "TotalIntensity", "AverageIntensity")
hourlyIntensities_merged <- hourlyIntensities_merged[, col_order]
head(hourlyIntensities_merged)

## Removing duplicates
total <- count(hourlyIntensities_merged)
hourlyIntensities_merged <- distinct(hourlyIntensities_merged)
distinct <- count(hourlyIntensities_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
hourlyIntensities_merged %>% filter(!complete.cases(.))
hourlyIntensities_merged <- na.omit(hourlyIntensities_merged)
na.action(hourlyIntensities_merged)

## Filtering infinite values
hourlyIntensities_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
hourlyIntensities_merged <- hourlyIntensities_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- hourlyIntensities_merged[sapply(hourlyIntensities_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

### Checking outlier values 
boxplot.stats(hourlyIntensities_merged$TotalIntensity)$out
boxplot.stats(hourlyIntensities_merged$AverageIntensity)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(hourlyIntensities_merged)

write.csv(hourlyIntensities_merged, '~/dataset_Bellabeat/Clean_tables/hourlyIntensities_clean.csv', row.names = FALSE)
