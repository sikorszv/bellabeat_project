# 1. Merging daily activities 3.12-5.12.2016 
dailyActivities_1 <- read_csv('~/dataset_Bellabeat/Fitabase_Data_1/dailyActivity_merged.csv')
dailyActivities_2 <- read_csv('~/dataset_Bellabeat/Fitabase_Data_2/dailyActivity_merged.csv')
dailyActivities <- rbind(dailyActivities_1,dailyActivities_2)

## Checking the data frame
glimpse(dailyActivities)

## Filtering rows start with 3, 4, 5
dailyActivities %>% filter(str_detect(ActivityDate, "^3"))
dailyActivities %>% filter(str_detect(ActivityDate, "^4"))
dailyActivities %>% filter(str_detect(ActivityDate, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
### Selecting and updating cells starting with 3, if they don’t match with correct date:
dailyActivities$ActivityDate[str_detect(dailyActivities$ActivityDate, "^3/")] <- "3/12/2016"
### Repeating for 4th and 5th Dec
dailyActivities$ActivityDate[str_detect(dailyActivities$ActivityDate, "^4/")] <- "4/12/2016"
dailyActivities$ActivityDate[str_detect(dailyActivities$ActivityDate, "^5/")] <- "5/12/2016"

## Removing duplicates
total <- count(dailyActivities)
dailyActivities <- distinct(dailyActivities)
distinct <- count(dailyActivities)
paste('Number of removed rows: ', total-distinct)


## Controlling the result by setting ActivityDate to factor
dailyActivities$ActivityDate <- as.factor(dailyActivities$ActivityDate)
levels(dailyActivities$ActivityDate)

## Transforming data types
dailyActivities$ActivityDate <- as.Date(dailyActivities$ActivityDate, "%d/%m/%Y")
dailyActivities$Id <- as.character(dailyActivities$Id) %>% str_trim()
glimpse(dailyActivities)


## Finding cases with missing data
dailyActivities %>% filter(!complete.cases(.))
dailyActivities <- na.omit(dailyActivities)
na.action(dailyActivities)

## Filtering infinite values
dailyActivities %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
dailyActivities <- dailyActivities %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- dailyActivities[sapply(dailyActivities, is.numeric)]

### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

boxplot.stats(dailyActivities$TotalSteps)$out
boxplot.stats(dailyActivities$TotalDistance)$out
boxplot.stats(dailyActivities$TrackerDistance)$out
boxplot.stats(dailyActivities$LoggedActivitiesDistance)$out
boxplot.stats(dailyActivities$VeryActiveDistance)$out
boxplot.stats(dailyActivities$ModeratelyActiveDistance)$out
boxplot.stats(dailyActivities$LightActiveDistance)$out
boxplot.stats(dailyActivities$SedentaryActiveDistance)$out 
boxplot.stats(dailyActivities$VeryActiveMinutes)$out
boxplot.stats(dailyActivities$FairlyActiveMinutes)$out
boxplot.stats(dailyActivities$LightlyActiveMinutes)$out
boxplot.stats(dailyActivities$SedentaryMinutes)$out
boxplot.stats(dailyActivities$Calories)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(dailyActivities)

## Saving the new merged file: 
write.csv(dailyActivities, '~/dataset_Bellabeat/Clean_tables/dailyActivities_clean.csv', row.names = FALSE)
