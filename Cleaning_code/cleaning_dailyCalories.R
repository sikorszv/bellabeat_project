# 1. Opening daily calories 3.12-5.12.2016 
dailyCalories <- read_csv('~/dataset_Bellabeat/Fitabase_Data_2/dailyCalories_merged.csv')

glimpse(dailyCalories)

## Filtering rows start with 3, 4, 5
dailyCalories %>% filter(str_detect(ActivityDay, "^3"))
dailyCalories %>% filter(str_detect(ActivityDay, "^4"))
dailyCalories %>% filter(str_detect(ActivityDay, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
dailyCalories$ActivityDay <- str_replace(dailyCalories$ActivityDay, "^4.*", "4/12/2016 ")
dailyCalories$ActivityDay <- str_replace(dailyCalories$ActivityDay, "^3.*", "3/12/2016 ")
dailyCalories$ActivityDay <- str_replace(dailyCalories$ActivityDay, "^5.*", "5/12/2016 ")

levels(as.factor(dailyCalories$ActivityDay))

## Formatting data types and separating date, time and am/pm values
dailyCalories$Id <- as.character(dailyCalories$Id) %>% str_trim()

## Verifying the dates and am/pm 
levels(as.factor(dailyCalories$ActivityDay))

## Removing duplicates
total <- count(dailyCalories)
dailyCalories <- distinct(dailyCalories)
distinct <- count(dailyCalories)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
dailyCalories %>% filter(!complete.cases(.))
dailyCalories <- na.omit(dailyCalories)
na.action(dailyCalories)

## Filtering infinite values
dailyCalories %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
dailyCalories <- dailyCalories %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))


## Outliers 
### Selecting numeric columns 
numeric_columns <- dailyCalories[sapply(dailyCalories, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(dailyCalories$Calories)$out

## Statistical summary of minimum, maximum, average of the data frame
summary(dailyCalories)

write.csv(dailyCalories, '~/dataset_Bellabeat/Clean_tables/dailyCalories_clean.csv', row.names = FALSE)