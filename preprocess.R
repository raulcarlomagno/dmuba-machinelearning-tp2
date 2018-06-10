library("data.table")
library("DataExplorer")
library("ggplot2") 
library("lubridate")

csvData <- fread("gender-classifier.csv")
csvData$tweet_id <- NULL
csvData$profileimage <- NULL
csvData$gender_gold <- NULL
csvData$profile_yn_gold <- NULL
csvData$'_unit_id' <- NULL
csvData <- csvData[csvData$profile_yn == "yes",]
csvData$profile_yn <- NULL
csvData <- csvData[csvData$gender != "unknown",]
csvData$'_unit_state' <- NULL
csvData$diff_prof_twt <- as.numeric(difftime(mdy_hms(csvData$tweet_created), mdy_hms(csvData$created), units = c("weeks")))
csvData$tweet_created <- NULL
csvData$created <- NULL
csvData$`_last_judgment_at` <- NULL
csvData$name <- NULL

table(csvData$`_golden`)
unique(csvData$`_trusted_judgments`)


str(csvData)

plot_missing(csvData)
plot_histogram(csvData)
plot_bar(csvData)
create_report(csvData)

summary(csvData$profile_yn)

unique(csvData$gender_gold)
qplot(csvData$gender_gold)

table(csvData$profile_yn_gold)
table(csvData$profile_yn_gold) / sum(table(csvData$profile_yn_gold))



