install.packages('tidyverse')
library(tidyverse)


# 3. Merging weightLogInfo 3.12.-5.12.2016
weightLogInfo_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/weightLogInfo_merged.csv')
weightLogInfo_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/weightLogInfo_merged.csv')
weightLogInfo_merged <- rbind(weightLogInfo_1, weightLogInfo_2)

glimpse(weightLogInfo_merged)

## Filtering rows start with 3, 4, 5
weightLogInfo_merged%>% filter(str_detect(Date, "^3"))
weightLogInfo_merged%>% filter(str_detect(Date, "^4"))
weightLogInfo_merged%>% filter(str_detect(Date, "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
weightLogInfo_merged$Date <- str_replace(weightLogInfo_merged$Date, "^4\\S* ", "4/12/2016 ")
weightLogInfo_merged$Date <- str_replace(weightLogInfo_merged$Date, "^3\\S* ", "3/12/2016 ")
weightLogInfo_merged$Date <- str_replace(weightLogInfo_merged$Date, "^5\\S* ", "5/12/2016 ")

levels(as.factor(weightLogInfo_merged$Date))

## Formatting data types and separating date, time and am/pm values
weightLogInfo_merged$Id <- as.character(weightLogInfo_merged$Id) %>% str_trim()
weightLogInfo_merged$Date_pure <- sub(" .*", "", weightLogInfo_merged$Date)
weightLogInfo_merged$Hour <- sub(".*? ", "", weightLogInfo_merged$Date)
weightLogInfo_merged$Hour_pure <- sub(" .*", "", weightLogInfo_merged$Hour)
weightLogInfo_merged$AM_PM <- sub(".* ", "", weightLogInfo_merged$Date)

## Verifying the dates and am/pm 
levels(as.factor(weightLogInfo_merged$Date_pure))
levels(as.factor(weightLogInfo_merged$Hour))
levels(as.factor(weightLogInfo_merged$Hour_pure))
levels(as.factor(weightLogInfo_merged$AM_PM))

head(weightLogInfo_merged)


## Re-order columns 
col_order <- c("Id", "Date", "Date_pure",
               "Hour", "Hour_pure", "AM_PM", "WeightKg", "WeightPounds", "Fat", "BMI", "IsManualReport", "LogId")
weightLogInfo_merged <- weightLogInfo_merged[, col_order]
head(weightLogInfo_merged)

## Removing duplicates
total <- count(weightLogInfo_merged)
weightLogInfo_merged <- distinct(weightLogInfo_merged)
distinct <- count(weightLogInfo_merged)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
##weightLogInfo_merged %>% filter(!complete.cases(.))
##WeightLogInfo_complete <- weightLogInfo_merged %>% filter(complete.cases(.))
##head(WeightLogInfo_complete)

## Filtering infinite values
weightLogInfo_merged %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
weightLogInfo_merged <- weightLogInfo_merged %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- ## Outliers 
  ### Selecting numeric columns 
  numeric_columns <- weightLogInfo_merged[sapply(weightLogInfo_merged, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}

### Checking outlier values 
boxplot.stats(weightLogInfo_merged$BMI)$out
boxplot.stats(weightLogInfo_merged$Fat)$out
boxplot.stats(weightLogInfo_merged$WeightPounds)$out
boxplot.stats(weightLogInfo_merged$WeightKg)$out

##Checking if BMI value can be real 
filter(weightLogInfo_merged, BMI > 40)

## Statistical summary of minimum, maximum, average of the data frame
summary(weightLogInfo_merged)

write.csv(weightLogInfo_merged, '~/dataset_Bellabeat/Clean_tables/weightLogInfo_clean.csv', row.names = FALSE)
