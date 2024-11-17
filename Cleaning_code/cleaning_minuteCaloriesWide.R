# 3. Opening minuteCaloriesWide 3.12.-5.12.2016

minuteCaloriesWide_merged <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteCaloriesWide_merged.csv')
head(minuteCaloriesWide_merged)
glimpse(minuteCaloriesWide_merged)

## Filtering rows start with 3, 4, 5
minuteCaloriesWide_merged %>% filter(str_detect(ActivityHour, "^3"))
minuteCaloriesWide_merged %>% filter(str_detect(ActivityHour, "^4"))
minuteCaloriesWide_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteCaloriesWide_merged$ActivityHour <- str_replace(minuteCaloriesWide_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
minuteCaloriesWide_merged$ActivityHour <- str_replace(minuteCaloriesWide_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
minuteCaloriesWide_merged$ActivityHour <- str_replace(minuteCaloriesWide_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteCaloriesWide_merged$Id <- as.character(minuteCaloriesWide_merged$Id) %>% str_trim()
minuteCaloriesWide_merged$Date <- sub(" .*", "", minuteCaloriesWide_merged$ActivityHour)
minuteCaloriesWide_merged$Hour <- sub(".*? ", "", minuteCaloriesWide_merged$ActivityHour)
minuteCaloriesWide_merged$Hour_pure <- sub(" .*", "", minuteCaloriesWide_merged$Hour)
minuteCaloriesWide_merged$AM_PM <- sub(".* ", "", minuteCaloriesWide_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(minuteCaloriesWide_merged$Date))
levels(as.factor(minuteCaloriesWide_merged$Hour))
levels(as.factor(minuteCaloriesWide_merged$Hour_pure))
levels(as.factor(minuteCaloriesWide_merged$AM))

## Re-order columns 
minuteCaloriesWide_merged <- relocate(minuteCaloriesWide_merged, c(Date, Hour, Hour_pure, AM_PM), .after= ActivityHour)
head(minuteCaloriesWide_merged)

## Removing duplicates
total <- count(minuteCaloriesWide_merged)
minuteCaloriesWide_merged <- distinct(minuteCaloriesWide_merged)
distinct <- count(minuteCaloriesWide_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteCaloriesWide_merged %>% filter(!complete.cases(.))
minuteCaloriesWide_merged <- na.omit(minuteCaloriesWide_merged)
na.action(minuteCaloriesWide_merged)

## Filtering infinite values
minuteCaloriesWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteCaloriesWide_merged <- minuteCaloriesWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteCaloriesWide_merged[sapply(minuteCaloriesWide_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

### Checking outlier values 
boxplot.stats(minuteCaloriesWide_merged$Calories00)$out
boxplot.stats(minuteCaloriesWide_merged$Calories01)$out
boxplot.stats(minuteCaloriesWide_merged$Calories02)$out
boxplot.stats(minuteCaloriesWide_merged$Calories03)$out
boxplot.stats(minuteCaloriesWide_merged$Calories04)$out
boxplot.stats(minuteCaloriesWide_merged$Calories05)$out
boxplot.stats(minuteCaloriesWide_merged$Calories06)$out
boxplot.stats(minuteCaloriesWide_merged$Calories07)$out
boxplot.stats(minuteCaloriesWide_merged$Calories08)$out
boxplot.stats(minuteCaloriesWide_merged$Calories09)$out
boxplot.stats(minuteCaloriesWide_merged$Calories10)$out
boxplot.stats(minuteCaloriesWide_merged$Calories11)$out
boxplot.stats(minuteCaloriesWide_merged$Calories12)$out
boxplot.stats(minuteCaloriesWide_merged$Calories13)$out
boxplot.stats(minuteCaloriesWide_merged$Calories14)$out
boxplot.stats(minuteCaloriesWide_merged$Calories15)$out
boxplot.stats(minuteCaloriesWide_merged$Calories16)$out
boxplot.stats(minuteCaloriesWide_merged$Calories17)$out
boxplot.stats(minuteCaloriesWide_merged$Calories18)$out
boxplot.stats(minuteCaloriesWide_merged$Calories19)$out
boxplot.stats(minuteCaloriesWide_merged$Calories20)$out
boxplot.stats(minuteCaloriesWide_merged$Calories21)$out
boxplot.stats(minuteCaloriesWide_merged$Calories22)$out
boxplot.stats(minuteCaloriesWide_merged$Calories23)$out
boxplot.stats(minuteCaloriesWide_merged$Calories24)$out
boxplot.stats(minuteCaloriesWide_merged$Calories25)$out
boxplot.stats(minuteCaloriesWide_merged$Calories26)$out
boxplot.stats(minuteCaloriesWide_merged$Calories27)$out
boxplot.stats(minuteCaloriesWide_merged$Calories28)$out
boxplot.stats(minuteCaloriesWide_merged$Calories29)$out
boxplot.stats(minuteCaloriesWide_merged$Calories30)$out
boxplot.stats(minuteCaloriesWide_merged$Calories31)$out
boxplot.stats(minuteCaloriesWide_merged$Calories32)$out
boxplot.stats(minuteCaloriesWide_merged$Calories34)$out
boxplot.stats(minuteCaloriesWide_merged$Calories35)$out
boxplot.stats(minuteCaloriesWide_merged$Calories36)$out
boxplot.stats(minuteCaloriesWide_merged$Calories37)$out
boxplot.stats(minuteCaloriesWide_merged$Calories38)$out
boxplot.stats(minuteCaloriesWide_merged$Calories39)$out
boxplot.stats(minuteCaloriesWide_merged$Calories40)$out
boxplot.stats(minuteCaloriesWide_merged$Calories41)$out
boxplot.stats(minuteCaloriesWide_merged$Calories42)$out
boxplot.stats(minuteCaloriesWide_merged$Calories43)$out
boxplot.stats(minuteCaloriesWide_merged$Calories44)$out
boxplot.stats(minuteCaloriesWide_merged$Calories45)$out
boxplot.stats(minuteCaloriesWide_merged$Calories46)$out
boxplot.stats(minuteCaloriesWide_merged$Calories47)$out
boxplot.stats(minuteCaloriesWide_merged$Calories48)$out
boxplot.stats(minuteCaloriesWide_merged$Calories49)$out
boxplot.stats(minuteCaloriesWide_merged$Calories50)$out
boxplot.stats(minuteCaloriesWide_merged$Calories51)$out
boxplot.stats(minuteCaloriesWide_merged$Calories52)$out
boxplot.stats(minuteCaloriesWide_merged$Calories53)$out
boxplot.stats(minuteCaloriesWide_merged$Calories54)$out
boxplot.stats(minuteCaloriesWide_merged$Calories55)$out
boxplot.stats(minuteCaloriesWide_merged$Calories56)$out
boxplot.stats(minuteCaloriesWide_merged$Calories57)$out
boxplot.stats(minuteCaloriesWide_merged$Calories58)$out
boxplot.stats(minuteCaloriesWide_merged$Calories59)$out


## Statistical summary of minimum, maximum, average of the data frame
summary(minuteCaloriesWide_merged)

write.csv(minuteCaloriesWide_merged, '~/dataset_Bellabeat/Clean_tables/minuteCaloriesWide_clean.csv', row.names = FALSE)
