# 3. Merging minuteCaloriesNarrow 3.12.-5.12.2016
minuteCaloriesNarrow_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/minuteCaloriesNarrow_merged.csv')
minuteCaloriesNarrow_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteCaloriesNarrow_merged.csv')
minuteCaloriesNarrow_merged <- rbind(minuteCaloriesNarrow_1, minuteCaloriesNarrow_2)

head(minuteCaloriesNarrow_merged)

## Filtering rows start with 3, 4, 5
minuteCaloriesNarrow_merged %>% filter(str_detect(ActivityMinute, "^3"))
minuteCaloriesNarrow_merged %>% filter(str_detect(ActivityMinute, "^4"))
minuteCaloriesNarrow_merged %>% filter(str_detect(ActivityMinute, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteCaloriesNarrow_merged$ActivityMinute <- str_replace(minuteCaloriesNarrow_merged$ActivityMinute, "^4\\S* ", "4/12/2016 ")
minuteCaloriesNarrow_merged$ActivityMinute <- str_replace(minuteCaloriesNarrow_merged$ActivityMinute, "^3\\S* ", "3/12/2016 ")
minuteCaloriesNarrow_merged$ActivityMinute <- str_replace(minuteCaloriesNarrow_merged$ActivityMinute, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteCaloriesNarrow_merged$Id <- as.character(minuteCaloriesNarrow_merged$Id) %>% str_trim()
minuteCaloriesNarrow_merged$Date <- sub(" .*", "", minuteCaloriesNarrow_merged$ActivityMinute)
minuteCaloriesNarrow_merged$Hour <- sub(".*? ", "", minuteCaloriesNarrow_merged$ActivityMinute)
minuteCaloriesNarrow_merged$Hour_pure <- sub(" .*", "", minuteCaloriesNarrow_merged$Hour)
minuteCaloriesNarrow_merged$AM_PM <- sub(".* ", "", minuteCaloriesNarrow_merged$ActivityMinute)

## Verifying the dates and am/pm 
levels(as.factor(minuteCaloriesNarrow_merged$Date))
levels(as.factor(minuteCaloriesNarrow_merged$Hour))
levels(as.factor(minuteCaloriesNarrow_merged$Hour_pure))
levels(as.factor(minuteCaloriesNarrow_merged$AM_PM))

## Re-order columns 
col_order <- c("Id", "ActivityMinute", "Date",
               "Hour", "Hour_pure", "AM_PM" , "Calories")
minuteCaloriesNarrow_merged <- minuteCaloriesNarrow_merged[, col_order]

head(minuteCaloriesNarrow_merged)

## Removing duplicates
total <- count(minuteCaloriesNarrow_merged)
minuteCaloriesNarrow_merged <- distinct(minuteCaloriesNarrow_merged)
distinct <- count(minuteCaloriesNarrow_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteCaloriesNarrow_merged %>% filter(!complete.cases(.))
minuteCaloriesNarrow_merged <- na.omit(minuteCaloriesNarrow_merged)
na.action(minuteCaloriesNarrow_merged)

## Filtering infinite values
minuteCaloriesNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteCaloriesNarrow_merged <- minuteCaloriesNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
### Selecting numeric columns 
numeric_columns <- minuteCaloriesNarrow_merged[sapply(minuteCaloriesNarrow_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteCaloriesNarrow_merged$Calories)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(minuteCaloriesNarrow_merged)

write.csv(minuteCaloriesNarrow_merged, '~/dataset_Bellabeat/Clean_tables/minuteCaloriesNarrow_clean.csv', row.names = FALSE)
