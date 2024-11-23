# 3. Merging hourlyCalories 3.12.-5.12.2016
hourlyCalories_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/hourlyCalories_merged.csv')
hourlyCalories_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/hourlyCalories_merged.csv')
hourlyCalories_merged <- rbind(hourlyCalories_1, hourlyCalories_2)

glimpse(hourlyCalories_merged)

## Filtering rows start with 3, 4, 5
hourlyCalories_merged %>% filter(str_detect(ActivityHour, "^3"))
hourlyCalories_merged %>% filter(str_detect(ActivityHour, "^4"))
hourlyCalories_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
hourlyCalories_merged$ActivityHour <- str_replace(hourlyCalories_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
hourlyCalories_merged$ActivityHour <- str_replace(hourlyCalories_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
hourlyCalories_merged$ActivityHour <- str_replace(hourlyCalories_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

levels(as.factor(hourlyCalories_merged$ActivityHour))

## Formatting data types and separating date, time and am/pm values
hourlyCalories_merged$Id <- as.character(hourlyCalories_merged$Id) %>% str_trim()
hourlyCalories_merged$Date <- sub(" .*", "", hourlyCalories_merged$ActivityHour)
hourlyCalories_merged$Hour <- sub(".*? ", "", hourlyCalories_merged$ActivityHour)
hourlyCalories_merged$Hour_pure <- sub(" .*", "", hourlyCalories_merged$Hour)
hourlyCalories_merged$AM_PM <- sub(".* ", "", hourlyCalories_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(hourlyCalories_merged$Date))
levels(as.factor(hourlyCalories_merged$Hour))
levels(as.factor(hourlyCalories_merged$Hour_pure))
levels(as.factor(hourlyCalories_merged$AM))

## Re-order columns 
col_order <- c("Id", "ActivityHour", "Date",
               "Hour", "Hour_pure","AM_PM", "Calories")
hourlyCalories_merged <- hourlyCalories_merged[, col_order]
head(hourlyCalories_merged)

## Removing duplicates
total <- count(hourlyCalories_merged)
hourlyCalories_merged <- distinct(hourlyCalories_merged)
distinct <- count(hourlyCalories_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
hourlyCalories_merged %>% filter(!complete.cases(.))
hourlyCalories_merged <- na.omit(hourlyCalories_merged)
na.action(hourlyCalories_merged)

## Filtering infinite values
hourlyCalories_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
hourlyCalories_merged <- hourlyCalories_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


## Outliers 
### Selecting numeric columns 
numeric_columns <- hourlyCalories_merged[sapply(hourlyCalories_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(hourlyCalories_merged$Calories)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(hourlyCalories_merged)

write.csv(hourlyCalories_merged, '~/dataset_Bellabeat/Clean_tables/hourlyCalories_clean.csv', row.names = FALSE)