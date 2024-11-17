# 3. Merging minuteMETsNarrow 3.12.-5.12.2016
minuteMETsNarrow_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/minuteMETsNarrow_merged.csv')
minuteMETsNarrow_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteMETsNarrow_merged.csv')
minuteMETsNarrow_merged <- rbind(minuteMETsNarrow_1, minuteMETsNarrow_2)

glimpse(minuteMETsNarrow_merged)

## Filtering rows start with 3, 4, 5
minuteMETsNarrow_merged %>% filter(str_detect(ActivityMinute, "^3"))
minuteMETsNarrow_merged %>% filter(str_detect(ActivityMinute, "^4"))
minuteMETsNarrow_merged %>% filter(str_detect(ActivityMinute, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteMETsNarrow_merged$ActivityMinute <- str_replace(minuteMETsNarrow_merged$ActivityMinute, "^4\\S* ", "4/12/2016 ")
minuteMETsNarrow_merged$ActivityMinute <- str_replace(minuteMETsNarrow_merged$ActivityMinute, "^3\\S* ", "3/12/2016 ")
minuteMETsNarrow_merged$ActivityMinute <- str_replace(minuteMETsNarrow_merged$ActivityMinute, "^5\\S* ", "5/12/2016 ")

levels(as.factor(minuteMETsNarrow_merged$ActivityMinute))

## Formatting data types and separating date, time and am/pm values
minuteMETsNarrow_merged$Id <- as.character(minuteMETsNarrow_merged$Id) %>% str_trim()
minuteMETsNarrow_merged$Date <- sub(" .*", "", minuteMETsNarrow_merged$ActivityMinute)
minuteMETsNarrow_merged$Hour <- sub(".*? ", "", minuteMETsNarrow_merged$ActivityMinute)
minuteMETsNarrow_merged$Hour_pure <- sub(" .*", "", minuteMETsNarrow_merged$Hour)
minuteMETsNarrow_merged$AM_PM <- sub(".* ", "", minuteMETsNarrow_merged$ActivityMinute)

## Verifying the dates and am/pm 
levels(as.factor(minuteMETsNarrow_merged$Date))
levels(as.factor(minuteMETsNarrow_merged$Hour))
levels(as.factor(minuteMETsNarrow_merged$Hour_pure))
levels(as.factor(minuteMETsNarrow_merged$AM_PM))

## Re-order columns 
col_order <- c("Id", "ActivityMinute", "Date",
               "Hour", "Hour_pure", "AM_PM", "METs")
minuteMETsNarrow_merged <- minuteMETsNarrow_merged[, col_order]
head(minuteMETsNarrow_merged)

## Removing duplicates
total <- count(minuteMETsNarrow_merged)
minuteMETsNarrow_merged <- distinct(minuteMETsNarrow_merged)
distinct <- count(minuteMETsNarrow_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteMETsNarrow_merged %>% filter(!complete.cases(.))
minuteMETsNarrow_merged <- na.omit(minuteMETsNarrow_merged)
na.action(minuteMETsNarrow_merged)

## Filtering infinite values
minuteMETsNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteMETsNarrow_merged <- minuteMETsNarrow_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteMETsNarrow_merged[sapply(minuteMETsNarrow_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(minuteMETsNarrow_merged$Calories)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(minuteMETsNarrow_merged)

write.csv(minuteMETsNarrow_merged, '~/dataset_Bellabeat/Clean_tables/minuteMETsNarrow_clean.csv', row.names = FALSE)
