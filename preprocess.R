(if(!require("data.table", quietly = TRUE)) install.packages("data.table"))
(if(!require("ggplot2", quietly = TRUE)) install.packages("ggplot2"))
library(scales)

csvData <- fread("gender-classifier.csv")
csvData$tweet_id <- NULL
csvData$profileimage <- NULL

str(csvData)


unique(csvData$gender_gold)
qplot(csvData$gender_gold)


