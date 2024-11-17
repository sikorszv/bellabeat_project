# 3. Merging minuteSleep 3.12.-5.12.2016
minuteSleep_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/minuteSleep_merged.csv')
minuteSleep_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteSleep_merged.csv')
minuteSleep_merged <- rbind(minuteSleep_1, minuteSleep_2)

glimpse(minuteSleep_merged)

minuteSleep_merged <- minuteSleep_merged %>%
  rename(
    ActivityMinute = date,
    Value = value,
    LogId = logId
  )

glimpse(minuteSleep_merged)

## Filtering rows start with 3, 4, 5
minuteSleep_merged %>% filter(str_detect(ActivityMinute, "^3"))
minuteSleep_merged %>% filter(str_detect(ActivityMinute, "^4"))
minuteSleep_merged %>% filter(str_detect(ActivityMinute, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteSleep_merged$ActivityMinute <- str_replace(minuteSleep_merged$ActivityMinute, "^4\\S* ", "4/12/2016 ")
minuteSleep_merged$ActivityMinute <- str_replace(minuteSleep_merged$ActivityMinute, "^3\\S* ", "3/12/2016 ")
minuteSleep_merged$ActivityMinute <- str_replace(minuteSleep_merged$ActivityMinute, "^5\\S* ", "5/12/2016 ")

levels(as.factor(minuteSleep_merged$ActivityMinute))

## Formatting data types and separating date, time and am/pm values
minuteSleep_merged$Id <- as.character(minuteSleep_merged$Id) %>% str_trim()
minuteSleep_merged$Date <- sub(" .*", "", minuteSleep_merged$ActivityMinute)
minuteSleep_merged$Hour <- sub(".*? ", "", minuteSleep_merged$ActivityMinute)
minuteSleep_merged$Hour_pure <- sub(" .*", "", minuteSleep_merged$Hour)
minuteSleep_merged$AM_PM <- sub(".* ", "", minuteSleep_merged$ActivityMinute)

## Verifying the dates and am/pm 
levels(as.factor(minuteSleep_merged$Date))
levels(as.factor(minuteSleep_merged$Hour))
levels(as.factor(minuteSleep_merged$Hour_pure))
levels(as.factor(minuteSleep_merged$AM_PM))

## Sleeping values
levels(as.factor(minuteSleep_merged$Value))

## Re-order columns 
col_order <- c("Id", "ActivityMinute", "Date",
               "Hour", "Hour_pure", "AM_PM", "Value", "LogId")
minuteSleep_merged <- minuteSleep_merged[, col_order]
head(minuteSleep_merged)

## Removing duplicates
total <- count(minuteSleep_merged)
minuteSleep_merged <- distinct(minuteSleep_merged)
distinct <- count(minuteSleep_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteSleep_merged %>% filter(!complete.cases(.))
minuteSleep_merged <- na.omit(minuteSleep_merged)
na.action(minuteSleep_merged)

## Filtering infinite values
minuteSleep_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteSleep_merged <- minuteSleep_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteSleep_merged[sapply(minuteSleep_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteSleep_merged$Value)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(minuteSleep_merged)

write.csv(minuteSleep_merged, '~/dataset_Bellabeat/Clean_tables/minuteSleep_clean.csv', row.names = FALSE)
