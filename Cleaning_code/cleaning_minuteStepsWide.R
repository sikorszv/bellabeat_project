# Opening minuteStepsWide 3.12.-5.12.2016

minuteStepsWide_merged <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteStepsWide_merged.csv')
glimpse(minuteStepsWide_merged)

## Filtering rows start with 3, 4, 5
minuteStepsWide_merged %>% filter(str_detect(ActivityHour, "^3"))
minuteStepsWide_merged %>% filter(str_detect(ActivityHour, "^4"))
minuteStepsWide_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteStepsWide_merged$ActivityHour <- str_replace(minuteStepsWide_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
minuteStepsWide_merged$ActivityHour <- str_replace(minuteStepsWide_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
minuteStepsWide_merged$ActivityHour <- str_replace(minuteStepsWide_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteStepsWide_merged$Id <- as.character(minuteStepsWide_merged$Id) %>% str_trim()
minuteStepsWide_merged$Date <- sub(" .*", "", minuteStepsWide_merged$ActivityHour)
minuteStepsWide_merged$Hour <- sub(".*? ", "", minuteStepsWide_merged$ActivityHour)
minuteStepsWide_merged$Hour_pure <- sub(" .*", "", minuteStepsWide_merged$Hour)
minuteStepsWide_merged$AM_PM <- sub(".* ", "", minuteStepsWide_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(minuteStepsWide_merged$Date))
levels(as.factor(minuteStepsWide_merged$Hour))
levels(as.factor(minuteStepsWide_merged$Hour_pure))
levels(as.factor(minuteStepsWide_merged$AM))

## Removing duplicates
total <- count(minuteStepsWide_merged)
minuteStepsWide_merged <- distinct(minuteStepsWide_merged)
distinct <- count(minuteStepsWide_merged)
paste('Number of removed rows: ', total-distinct)

glimpse(minuteStepsWide_merged)

## Re-order columns 
minuteStepsWide_merged <- relocate(minuteStepsWide_merged, c(Date, Hour, Hour_pure, AM_PM), .after= ActivityHour)
head(minuteStepsWide_merged)

## Finding cases with missing data
minuteStepsWide_merged %>% filter(!complete.cases(.))
minuteStepsWide_merged <- na.omit(minuteStepsWide_merged)
na.action(minuteStepsWide_merged)

## Filtering infinite values
minuteStepsWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteStepsWide_merged <- minuteStepsWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteStepsWide_merged[sapply(minuteStepsWide_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

### Checking outlier values 
boxplot.stats(minuteStepsWide_merged$Steps00)$out
boxplot.stats(minuteStepsWide_merged$Steps01)$out
boxplot.stats(minuteStepsWide_merged$Steps02)$out
boxplot.stats(minuteStepsWide_merged$Steps03)$out
boxplot.stats(minuteStepsWide_merged$Steps04)$out
boxplot.stats(minuteStepsWide_merged$Steps05)$out
boxplot.stats(minuteStepsWide_merged$Steps06)$out
boxplot.stats(minuteStepsWide_merged$Steps07)$out
boxplot.stats(minuteStepsWide_merged$Steps08)$out
boxplot.stats(minuteStepsWide_merged$Steps09)$out
boxplot.stats(minuteStepsWide_merged$Steps10)$out
boxplot.stats(minuteStepsWide_merged$Steps11)$out
boxplot.stats(minuteStepsWide_merged$Steps12)$out
boxplot.stats(minuteStepsWide_merged$Steps13)$out
boxplot.stats(minuteStepsWide_merged$Steps14)$out
boxplot.stats(minuteStepsWide_merged$Steps15)$out
boxplot.stats(minuteStepsWide_merged$Steps16)$out
boxplot.stats(minuteStepsWide_merged$Steps17)$out
boxplot.stats(minuteStepsWide_merged$Steps18)$out
boxplot.stats(minuteStepsWide_merged$Steps19)$out
boxplot.stats(minuteStepsWide_merged$Steps20)$out
boxplot.stats(minuteStepsWide_merged$Steps21)$out
boxplot.stats(minuteStepsWide_merged$Steps22)$out
boxplot.stats(minuteStepsWide_merged$Steps23)$out
boxplot.stats(minuteStepsWide_merged$Steps24)$out
boxplot.stats(minuteStepsWide_merged$Steps25)$out
boxplot.stats(minuteStepsWide_merged$Steps26)$out
boxplot.stats(minuteStepsWide_merged$Steps27)$out
boxplot.stats(minuteStepsWide_merged$Steps28)$out
boxplot.stats(minuteStepsWide_merged$Steps29)$out
boxplot.stats(minuteStepsWide_merged$Steps30)$out
boxplot.stats(minuteStepsWide_merged$Steps31)$out
boxplot.stats(minuteStepsWide_merged$Steps32)$out
boxplot.stats(minuteStepsWide_merged$Steps34)$out
boxplot.stats(minuteStepsWide_merged$Steps35)$out
boxplot.stats(minuteStepsWide_merged$Steps36)$out
boxplot.stats(minuteStepsWide_merged$Steps37)$out
boxplot.stats(minuteStepsWide_merged$Steps38)$out
boxplot.stats(minuteStepsWide_merged$Steps39)$out
boxplot.stats(minuteStepsWide_merged$Steps40)$out
boxplot.stats(minuteStepsWide_merged$Steps41)$out
boxplot.stats(minuteStepsWide_merged$Steps42)$out
boxplot.stats(minuteStepsWide_merged$Steps43)$out
boxplot.stats(minuteStepsWide_merged$Steps44)$out
boxplot.stats(minuteStepsWide_merged$Steps45)$out
boxplot.stats(minuteStepsWide_merged$Steps46)$out
boxplot.stats(minuteStepsWide_merged$Steps47)$out
boxplot.stats(minuteStepsWide_merged$Steps48)$out
boxplot.stats(minuteStepsWide_merged$Steps49)$out
boxplot.stats(minuteStepsWide_merged$Steps50)$out
boxplot.stats(minuteStepsWide_merged$Steps51)$out
boxplot.stats(minuteStepsWide_merged$Steps52)$out
boxplot.stats(minuteStepsWide_merged$Steps53)$out
boxplot.stats(minuteStepsWide_merged$Steps54)$out
boxplot.stats(minuteStepsWide_merged$Steps55)$out
boxplot.stats(minuteStepsWide_merged$Steps56)$out
boxplot.stats(minuteStepsWide_merged$Steps57)$out
boxplot.stats(minuteStepsWide_merged$Steps58)$out
boxplot.stats(minuteStepsWide_merged$Steps59)$out


## Statistical summary of minimum, maximum, average of the data frame
summary(minuteStepsWide_merged)

write.csv(minuteStepsWide_merged, '~/dataset_Bellabeat/Clean_tables/minuteStepsWide_clean.csv', row.names = FALSE)
