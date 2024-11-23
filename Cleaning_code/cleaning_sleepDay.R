# Opening sleepDay_merged 3.12.-5.12.2016

sleepDay_merged <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/sleepDay_merged.csv')
glimpse(sleepDay_merged)

## Filtering rows start with 3, 4, 5
sleepDay_merged %>% filter(str_detect(SleepDay, "^3"))
sleepDay_merged %>% filter(str_detect(SleepDay, "^4"))
sleepDay_merged %>% filter(str_detect(SleepDay, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
sleepDay_merged$SleepDay <- str_replace(sleepDay_merged$SleepDay, "^4\\S* ", "4/12/2016 ")
sleepDay_merged$SleepDay <- str_replace(sleepDay_merged$SleepDay, "^3\\S* ", "3/12/2016 ")
sleepDay_merged$SleepDay <- str_replace(sleepDay_merged$SleepDay, "^5\\S* ", "5/12/2016 ")

levels(as.factor(sleepDay_merged$SleepDay))

## Formatting data types and separating date, time and am/pm values
sleepDay_merged$Id <- as.character(sleepDay_merged$Id) %>% str_trim()
sleepDay_merged$Date <- sub(" .*", "", sleepDay_merged$SleepDay)
sleepDay_merged$Hour <- sub(".*? ", "", sleepDay_merged$SleepDay)
sleepDay_merged$Hour_pure <- sub(" .*", "", sleepDay_merged$Hour)
sleepDay_merged$AM_PM <- sub(".* ", "", sleepDay_merged$SleepDay)

## Verifying the dates and am/pm 
levels(as.factor(sleepDay_merged$Date))
levels(as.factor(sleepDay_merged$Hour))
levels(as.factor(sleepDay_merged$Hour_pure))
levels(as.factor(sleepDay_merged$AM_PM))

## Re-order columns 
col_order <- c("Id", "SleepDay", "Date",
               "Hour", "Hour_pure", "AM_PM", "TotalSleepRecords", "TotalMinutesAsleep", "TotalTimeInBed")
sleepDay_merged <- sleepDay_merged[, col_order]
head(sleepDay_merged)

## Removing duplicates
total <- count(sleepDay_merged)
sleepDay_merged <- distinct(sleepDay_merged)
distinct <- count(sleepDay_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
sleepDay_merged %>% filter(!complete.cases(.))
sleepDay_merged <- na.omit(sleepDay_merged)
na.action(sleepDay_merged)

## Filtering infinite values
sleepDay_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
sleepDay_merged <- sleepDay_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- sleepDay_merged[sapply(sleepDay_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(sleepDay_merged$TotalSleepRecords)$out
boxplot.stats(sleepDay_merged$TotalMinutesAsleep)$out
boxplot.stats(sleepDay_merged$TotalTimeInBed)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(sleepDay_merged)

write.csv(sleepDay_merged, '~/dataset_Bellabeat/Clean_tables/sleepDay_clean.csv', row.names = FALSE)