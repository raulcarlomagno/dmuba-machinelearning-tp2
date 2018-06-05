libs <- c("data.table", "DataExplorer")
sapply(libs[sapply(libs, require, character.only = TRUE, quietly = TRUE) == FALSE], install.packages)

#lubripack
sapply(libs, require, character.only = TRUE)

library(data.table)
library(DataExplorer)

libs <- c("data.table", "DataExplorer")
sapply(libs[sapply(libs, require, character.only = TRUE, quietly = TRUE) == FALSE], install.packages)

(if(!require("data.table", quietly = TRUE)) install.packages("data.table"))
(if(!require("ggplot2", quietly = TRUE)) install.packages("ggplot2"))
(if(!require("DataExplorer", quietly = TRUE)) install.packages("DataExplorer"))

csvData <- fread("gender-classifier.csv")
csvData$tweet_id <- NULL
csvData$profileimage <- NULL
csvData$gender_gold <- NULL
csvData$profile_yn_gold <- NULL
csvData$'_unit_id' <- NULL


library("lubridate")

class(mdy_hms(csvData$created))

cbind(csvData$created, mdy_hms(csvData$created))
head(cbind(csvData$created, csvData$tweet_created))

diferencia <- difftime(mdy_hms(csvData$tweet_created), mdy_hms(csvData$created), units = c("weeks"))

hist(as.numeric(diferencia))

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


