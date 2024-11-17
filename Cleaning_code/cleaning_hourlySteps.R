# 3. Merging hourlySteps 3.12.-5.12.2016
hourlySteps_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/hourlySteps_merged.csv')
hourlySteps_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/hourlySteps_merged.csv')
hourlySteps_merged <- rbind(hourlySteps_1, hourlySteps_2)

glimpse(hourlySteps_merged)

## Filtering rows start with 3, 4, 5
hourlySteps_merged %>% filter(str_detect(ActivityHour, "^3"))
hourlySteps_merged %>% filter(str_detect(ActivityHour, "^4"))
hourlySteps_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
hourlySteps_merged$ActivityHour <- str_replace(hourlySteps_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
hourlySteps_merged$ActivityHour <- str_replace(hourlySteps_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
hourlySteps_merged$ActivityHour <- str_replace(hourlySteps_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

levels(as.factor(hourlySteps_merged$ActivityHour))

## Formatting data types and separating date, time and am/pm values
hourlySteps_merged$Id <- as.character(hourlySteps_merged$Id) %>% str_trim()
hourlySteps_merged$Date <- sub(" .*", "", hourlySteps_merged$ActivityHour)
hourlySteps_merged$Hour <- sub(".*? ", "", hourlySteps_merged$ActivityHour)
hourlySteps_merged$Hour_pure <- sub(" .*", "", hourlySteps_merged$Hour)
hourlySteps_merged$AM_PM <- sub(".* ", "", hourlySteps_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(hourlySteps_merged$Date))
levels(as.factor(hourlySteps_merged$Hour))
levels(as.factor(hourlySteps_merged$Hour_pure))
levels(as.factor(hourlySteps_merged$AM))

## Re-order columns 
col_order <- c("Id", "ActivityHour", "Date",
               "Hour", "Hour_pure","AM_PM", "StepTotal")
hourlySteps_merged <- hourlySteps_merged[, col_order]
head(hourlySteps_merged)

## Removing duplicates
total <- count(hourlySteps_merged)
hourlySteps_merged <- distinct(hourlySteps_merged)
distinct <- count(hourlySteps_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
hourlySteps_merged %>% filter(!complete.cases(.))
hourlySteps_merged <- na.omit(hourlySteps_merged)
na.action(hourlySteps_merged)

## Filtering infinite values
hourlySteps_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
hourlySteps_merged <- hourlySteps_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


## Outliers 
### Selecting numeric columns 
numeric_columns <- hourlySteps_merged[sapply(hourlySteps_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(hourlySteps_merged$Steps)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(hourlySteps_merged)

write.csv(hourlySteps_merged, '~/dataset_Bellabeat/Clean_tables/hourlySteps_clean.csv', row.names = FALSE)