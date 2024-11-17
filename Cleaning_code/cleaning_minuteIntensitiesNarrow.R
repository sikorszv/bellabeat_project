# 3. Merging minuteIntensitiesNarrow 3.12.-5.12.2016
minuteIntensitiesNarrow_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/minuteIntensitiesNarrow_merged.csv')
minuteIntensitiesNarrow_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteIntensitiesNarrow_merged.csv')
minuteIntensitiesNarrow_merged <- rbind(minuteIntensitiesNarrow_1, minuteIntensitiesNarrow_2)

glimpse(minuteIntensitiesNarrow_merged)

## Filtering rows start with 3, 4, 5
minuteIntensitiesNarrow_merged %>% filter(str_detect(ActivityMinute, "^3"))
minuteIntensitiesNarrow_merged %>% filter(str_detect(ActivityMinute, "^4"))
minuteIntensitiesNarrow_merged %>% filter(str_detect(ActivityMinute, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteIntensitiesNarrow_merged$ActivityMinute <- str_replace(minuteIntensitiesNarrow_merged$ActivityMinute, "^4\\S* ", "4/12/2016 ")
minuteIntensitiesNarrow_merged$ActivityMinute <- str_replace(minuteIntensitiesNarrow_merged$ActivityMinute, "^3\\S* ", "3/12/2016 ")
minuteIntensitiesNarrow_merged$ActivityMinute <- str_replace(minuteIntensitiesNarrow_merged$ActivityMinute, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteIntensitiesNarrow_merged$Id <- as.character(minuteIntensitiesNarrow_merged$Id) %>% str_trim()
minuteIntensitiesNarrow_merged$Date <- sub(" .*", "", minuteIntensitiesNarrow_merged$ActivityMinute)
minuteIntensitiesNarrow_merged$Hour <- sub(".*? ", "", minuteIntensitiesNarrow_merged$ActivityMinute)
minuteIntensitiesNarrow_merged$Hour_pure <- sub(" .*", "", minuteIntensitiesNarrow_merged$Hour)
minuteIntensitiesNarrow_merged$AM_PM <- sub(".* ", "", minuteIntensitiesNarrow_merged$ActivityMinute)

## Verifying the dates and am/pm 
levels(as.factor(minuteIntensitiesNarrow_merged$Date))
levels(as.factor(minuteIntensitiesNarrow_merged$Hour))
levels(as.factor(minuteIntensitiesNarrow_merged$Hour_pure))
levels(as.factor(minuteIntensitiesNarrow_merged$AM_PM))

## Re-order columns 
col_order <- c("Id", "ActivityMinute", "Date",
               "Hour", "Hour_pure", "AM_PM" , "Intensity")
minuteIntensitiesNarrow_merged <- minuteIntensitiesNarrow_merged[, col_order]

head(minuteIntensitiesNarrow_merged)

## Removing duplicates
total <- count(minuteIntensitiesNarrow_merged)
minuteIntensitiesNarrow_merged <- distinct(minuteIntensitiesNarrow_merged)
distinct <- count(minuteIntensitiesNarrow_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteIntensitiesNarrow_merged %>% filter(!complete.cases(.))
minuteIntensitiesNarrow_merged <- na.omit(minuteIntensitiesNarrow_merged)
na.action(minuteIntensitiesNarrow_merged)

## Filtering infinite values
minuteIntensitiesNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteIntensitiesNarrow_merged <- minuteIntensitiesNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteIntensitiesNarrow_merged[sapply(minuteIntensitiesNarrow_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteIntensitiesNarrow_merged$Intensities)$out

### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteIntensitiesNarrow_merged$Intensities)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(minuteIntensitiesNarrow_merged)

write.csv(minuteIntensitiesNarrow_merged, '~/dataset_Bellabeat/Clean_tables/minuteIntensitiesNarrow_clean.csv', row.names = FALSE)
