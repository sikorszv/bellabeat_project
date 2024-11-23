# Merging heartrates 3.12-5.12.2016
heartrate_seconds_merged_1 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_1/heartrate_seconds_merged.csv')
heartrate_seconds_merged_2 <- read.csv('~/dataset_Bellabeat/Fitabase_Data_2/heartrate_seconds_merged.csv')
heartrate_all <- rbind(heartrate_seconds_merged_1, heartrate_seconds_merged_2)
## Quick glimpse in the new data frame
glimpse(heartrate_all)

## Filtering rows start with 3, 4, 5
heartrate_all %>% filter(str_detect(Time , "^3"))
heartrate_all %>% filter(str_detect(Time , "^4"))
heartrate_all %>% filter(str_detect(Time , "^5"))

## Correcting dates by substituting the correct month number by using library stringr 
heartrate_all$Time  <- str_replace(heartrate_all$Time , "^4\\S* ", "4/12/2016 ")
heartrate_all$Time  <- str_replace(heartrate_all$Time , "^3\\S* ", "3/12/2016 ")
heartrate_all$Time  <- str_replace(heartrate_all$Time , "^5\\S* ", "5/12/2016 ")

levels(as.factor(heartrate_all$Time ))

## Formatting data types and separating date, time and am/pm values
heartrate_all$Id <- as.character(heartrate_all$Id) %>% str_trim()
heartrate_all$Date <- sub(" .*", "", heartrate_all$Time )
heartrate_all$Hour <- sub(".*? ", "", heartrate_all$Time )
heartrate_all$Hour_pure <- sub(" .*", "", heartrate_all$Hour)
heartrate_all$AM_PM <- sub(".* ", "", heartrate_all$Time )

## Verifying the dates and am/pm 
levels(as.factor(heartrate_all$Date))
levels(as.factor(heartrate_all$Hour))
levels(as.factor(heartrate_all$Hour_pure))
levels(as.factor(heartrate_all$AM_PM))

## Re-order columns 
col_order <- c("Id", "Time", "Date",
               "Hour", "Hour_pure", "AM_PM", "Value")
heartrate_all <- heartrate_all[, col_order]
head(heartrate_all)

## Removing duplicates
total <- count(heartrate_all)
heartrate_all <- distinct(heartrate_all)
distinct <- count(heartrate_all)
paste('Number of removed rows: ', total-distinct)

## Finding cases with missing data
heartrate_all %>% filter(!complete.cases(.))
heartrate_all <- na.omit(heartrate_all)
na.action(heartrate_all)

## Filtering infinite values
heartrate_all %>% 
  filter_if(~is.numeric(.), all_vars(is.infinite(.)))

## Leaving out infinite values 
heartrate_all <- heartrate_all %>% 
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

## Outliers 
### Selecting numeric columns 
numeric_columns <- heartrate_all[sapply(heartrate_all, is.numeric)]
### Searching for outliers 
for (i in colnames(numeric_columns)) {
  boxplot(numeric_columns[i], show.names=TRUE)
}
### Checking outlier values 
boxplot.stats(heartrate_all$Value)$out

filter(heartrate_all, Value > 220)

## Statistical summary of minimum, maximum, average of the data frame
summary(heartrate_all)

write.csv(heartrate_all, '~/dataset_Bellabeat/Clean_tables/heartrates_clean.csv', row.names = FALSE)
