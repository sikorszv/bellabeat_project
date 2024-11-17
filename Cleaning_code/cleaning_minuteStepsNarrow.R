# 3. Merging minuteStepsNarrow 3.12.-5.12.2016
minuteStepsNarrow_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/minuteStepsNarrow_merged.csv')
minuteStepsNarrow_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteStepsNarrow_merged.csv')
minuteStepsNarrow_merged <- rbind(minuteStepsNarrow_1, minuteStepsNarrow_2)

glimpse(minuteStepsNarrow_merged)

## Filtering rows start with 3, 4, 5
minuteStepsNarrow_merged %>% filter(str_detect(ActivityMinute, "^3"))
minuteStepsNarrow_merged %>% filter(str_detect(ActivityMinute, "^4"))
minuteStepsNarrow_merged %>% filter(str_detect(ActivityMinute, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteStepsNarrow_merged$ActivityMinute <- str_replace(minuteStepsNarrow_merged$ActivityMinute, "^4\\S* ", "4/12/2016 ")
minuteStepsNarrow_merged$ActivityMinute <- str_replace(minuteStepsNarrow_merged$ActivityMinute, "^3\\S* ", "3/12/2016 ")
minuteStepsNarrow_merged$ActivityMinute <- str_replace(minuteStepsNarrow_merged$ActivityMinute, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteStepsNarrow_merged$Id <- as.character(minuteStepsNarrow_merged$Id) %>% str_trim()
minuteStepsNarrow_merged$Date <- sub(" .*", "", minuteStepsNarrow_merged$ActivityMinute)
minuteStepsNarrow_merged$Hour <- sub(".*? ", "", minuteStepsNarrow_merged$ActivityMinute)
minuteStepsNarrow_merged$Hour_pure <- sub(" .*", "", minuteStepsNarrow_merged$Hour)
minuteStepsNarrow_merged$AM_PM <- sub(".* ", "", minuteStepsNarrow_merged$ActivityMinute)

## Verifying the dates and am/pm 
levels(as.factor(minuteStepsNarrow_merged$Date))
levels(as.factor(minuteStepsNarrow_merged$Hour))
levels(as.factor(minuteStepsNarrow_merged$Hour_pure))
levels(as.factor(minuteStepsNarrow_merged$AM_PM))

## Re-order columns 
col_order <- c("Id", "ActivityMinute", "Date",
               "Hour", "Hour_pure", "AM_PM" , "Steps")
minuteStepsNarrow_merged <- minuteStepsNarrow_merged[, col_order]

head(minuteStepsNarrow_merged)

## Removing duplicates
total <- count(minuteStepsNarrow_merged)
minuteStepsNarrow_merged <- distinct(minuteStepsNarrow_merged)
distinct <- count(minuteStepsNarrow_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteStepsNarrow_merged %>% filter(!complete.cases(.))
minuteStepsNarrow_merged <- na.omit(minuteStepsNarrow_merged)
na.action(minuteStepsNarrow_merged)

## Filtering infinite values
minuteStepsNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteStepsNarrow_merged <- minuteStepsNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteStepsNarrow_merged[sapply(minuteStepsNarrow_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteStepsNarrow_merged$Steps)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(minuteStepsNarrow_merged)

write.csv(minuteStepsNarrow_merged, '~/dataset_Bellabeat/Clean_tables/minuteStepsNarrow_clean.csv', row.names = FALSE)
