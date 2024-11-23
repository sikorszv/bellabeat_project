# Opening minuteIntensitiesWide 3.12.-5.12.2016

minuteIntensitiesWide_merged <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/minuteIntensitiesWide_merged.csv')
glimpse(minuteIntensitiesWide_merged)

## Filtering rows start with 3, 4, 5
minuteIntensitiesWide_merged %>% filter(str_detect(ActivityHour, "^3"))
minuteIntensitiesWide_merged %>% filter(str_detect(ActivityHour, "^4"))
minuteIntensitiesWide_merged %>% filter(str_detect(ActivityHour, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
minuteIntensitiesWide_merged$ActivityHour <- str_replace(minuteIntensitiesWide_merged$ActivityHour, "^4\\S* ", "4/12/2016 ")
minuteIntensitiesWide_merged$ActivityHour <- str_replace(minuteIntensitiesWide_merged$ActivityHour, "^3\\S* ", "3/12/2016 ")
minuteIntensitiesWide_merged$ActivityHour <- str_replace(minuteIntensitiesWide_merged$ActivityHour, "^5\\S* ", "5/12/2016 ")

## Formatting data types and separating date, time and am/pm values
minuteIntensitiesWide_merged$Id <- as.character(minuteIntensitiesWide_merged$Id) %>% str_trim()
minuteIntensitiesWide_merged$Date <- sub(" .*", "", minuteIntensitiesWide_merged$ActivityHour)
minuteIntensitiesWide_merged$Hour <- sub(".*? ", "", minuteIntensitiesWide_merged$ActivityHour)
minuteIntensitiesWide_merged$Hour_pure <- sub(" .*", "", minuteIntensitiesWide_merged$Hour)
minuteIntensitiesWide_merged$AM_PM <- sub(".* ", "", minuteIntensitiesWide_merged$ActivityHour)

## Verifying the dates and am/pm 
levels(as.factor(minuteIntensitiesWide_merged$Date))
levels(as.factor(minuteIntensitiesWide_merged$Hour))
levels(as.factor(minuteIntensitiesWide_merged$Hour_pure))
levels(as.factor(minuteIntensitiesWide_merged$AM))

## Re-order columns 
minuteIntensitiesWide_merged <- relocate(minuteIntensitiesWide_merged, c(Date, Hour, Hour_pure, AM_PM), .after= ActivityHour)
head(minuteIntensitiesWide_merged)

## Removing duplicates
total <- count(minuteIntensitiesWide_merged)
minuteIntensitiesWide_merged <- distinct(minuteIntensitiesWide_merged)
distinct <- count(minuteIntensitiesWide_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
minuteIntensitiesWide_merged %>% filter(!complete.cases(.))
minuteIntensitiesWide_merged <- na.omit(minuteIntensitiesWide_merged)
na.action(minuteIntensitiesWide_merged)

## Filtering infinite values
minuteIntensitiesWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
minuteIntensitiesWide_merged <- minuteIntensitiesWide_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- minuteIntensitiesWide_merged[sapply(minuteIntensitiesWide_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

### Checking outlier values 
boxplot.stats(minuteIntensitiesWide_merged$Intensities00)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities01)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities02)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities03)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities04)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities05)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities06)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities07)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities08)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities09)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities10)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities11)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities12)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities13)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities14)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities15)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities16)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities17)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities18)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities19)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities20)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities21)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities22)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities23)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities24)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities25)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities26)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities27)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities28)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities29)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities30)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities31)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities32)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities34)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities35)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities36)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities37)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities38)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities39)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities40)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities41)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities42)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities43)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities44)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities45)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities46)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities47)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities48)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities49)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities50)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities51)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities52)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities53)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities54)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities55)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities56)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities57)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities58)$out
boxplot.stats(minuteIntensitiesWide_merged$Intensities59)$out


## Statistical summary of minimum, maximum, average of the data frame
summary(minuteIntensitiesWide_merged)

write.csv(minuteIntensitiesWide_merged, '~/dataset_Bellabeat/Clean_tables/minuteIntensitiesWide_clean.csv', row.names = FALSE)
