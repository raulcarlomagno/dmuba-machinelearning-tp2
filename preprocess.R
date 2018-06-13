library("data.table")
library("DataExplorer")
library("ggplot2") 
library("lubridate")
#library("arules") #detach("package:arules", unload = TRUE)
library("tokenizers")
#library("stopwords") #detach("package:stopwords", unload = TRUE)
#library("tm") #detach("package:tm", unload = TRUE)
library(plyr)
library("stringr")
#library("fastrtext")

stopwords <- scan("stopwords.txt", character(), quote = "")
stopwords <- c(stopwords, scan("customstopwords.txt", character(), quote = ""))
stopwords <- c(stopwords, as.character(0:10))

csvData <- fread("gender-classifier.csv", encoding = 'UTF-8')
csvData$tweet_id <- NULL
csvData$profileimage <- NULL
csvData$gender_gold <- NULL
csvData$profile_yn_gold <- NULL
csvData$user_id <- csvData$'_unit_id'
csvData$'_unit_id' <- NULL
csvData <- csvData[csvData$profile_yn == "yes",]
csvData$profile_yn <- NULL
csvData <- csvData[csvData$gender != "unknown",]
#csvData$gender <- as.factor(csvData$gender) #hacerlo a lo ultimo si es necesario
csvData$'_unit_state' <- NULL
csvData$diff_prof_twt <- as.numeric(difftime(mdy_hms(csvData$tweet_created), mdy_hms(csvData$created), units = c("weeks")))
csvData$tweets_per_day <- round(csvData$tweet_count / as.numeric(difftime(now("UTC"), mdy_hms(csvData$created), units = c("days"))))
csvData$tweet_created <- NULL
csvData$created <- NULL
csvData$`_last_judgment_at` <- NULL
csvData$name <- NULL
csvData$tweet_coord <- NULL
csvData$tweet_location <- NULL
csvData$`_golden` <- NULL
csvData$`_trusted_judgments` <- NULL
csvData$text <- str_replace_all(csvData$text, "&amp;", "&");
csvData$gender_confidence <- csvData$`gender:confidence`
csvData$`gender:confidence` <- NULL
csvData$profile_yn_confidence <- csvData$`profile_yn:confidence`
csvData$`profile_yn:confidence` <- NULL
csvData$user_timezone <- NULL
csvData[nchar(csvData$sidebar_color) != 6, "sidebar_color"] <- NA #seteo NA cuando no tienen un color valido
csvData$sidebar_color[!is.na(csvData$sidebar_color)] <- paste0("#", csvData$sidebar_color[!is.na(csvData$sidebar_color)])
csvData[nchar(csvData$link_color) != 6, "link_color"] <- NA #seteo NA cuando no tienen un color valido
csvData$link_color[grep("\\+", csvData$link_color)] <- NA #unos tenian datos raros
csvData$link_color[!is.na(csvData$link_color)] <- paste0("#", csvData$link_color[!is.na(csvData$link_color)])


#https://i.imgur.com/PKjgfFXm.jpg
#mejorar
#hay mucho cyan por #C0DEED
#ver q onda los blancos , negros y que color setear cuando no tiene uno valido
hex2colorname <- function(hexacolor){
  if(is.na(hexacolor))
    return("indeterminado")  
  
  coloresrgb <- col2rgb(hexacolor)[]
  hue <- rgb2hsv(coloresrgb[1,],coloresrgb[2,],coloresrgb[3,])["h",1] * 360
  
  if(hue >= 345 | hue < 30)
    return("red")
  if(hue >= 30 & hue < 45)
    return("orange")
  if(hue >= 45 & hue < 90)
    return("yellow")
  if(hue >= 90 & hue < 150)
    return("green")
  if(hue >= 150 & hue < 225)
    return("cyan")
  if(hue >= 225 & hue < 270)
    return("blue")
  if(hue >= 270 & hue < 285)
    return("violet")
  if(hue >= 285 & hue < 345)
    return("magenta")
}

csvData$sidebar_color_cat <- sapply(csvData$sidebar_color, hex2colorname)
ggplot(csvData, aes(sidebar_color_cat)) +
  geom_bar(aes(fill = gender), position=position_dodge())

csvData$link_color_cat <- sapply(csvData$link_color, hex2colorname)
ggplot(csvData, aes(link_color_cat)) +
  geom_bar(aes(fill = gender), position=position_dodge())





armarVocabularios <- function (genero,columnName){
  tokens <- tokenize_tweets(csvData[csvData$gender == genero, columnName, with=FALSE][[columnName]], stopwords = stopwords)
  tokens <- unlist(tokens)
  tokens <- tokens[is.na(as.numeric(tokens))] #filtro los numeros
  tokens <- tokens[!grepl("www.|http:|https:", tokens)] #filtro urls
  tokens <- tokens[str_length(tokens) > 2] #filtro palabras menores a 3
  
  dftokens <- count(tokens)
  names(dftokens) <- c("word", "freq")
  dftokensMost <- head(dftokens[order(-dftokens$freq),], 15) #15 palabras de bow para cada genero
  dftokensMost$relative <- dftokensMost$freq / sum(dftokensMost$freq) * 100
  dftokensMost$freq <- NULL
  
  return(dftokensMost)
}

vocabularios <- lapply(unique(csvData$gender), armarVocabularios, columnName="text")
vocabularios.male <- vocabularios[[1]]
vocabularios.female <- vocabularios[[2]]
vocabularios.brand <- vocabularios[[3]]

vocabulariosBio <- lapply(unique(csvData$gender), armarVocabularios, columnName="description")
vocabulariosBio.male <- vocabulariosBio[[1]]
vocabulariosBio.female <- vocabulariosBio[[2]]
vocabulariosBio.brand <- vocabulariosBio[[3]]

#tmp <- csvData[order(csvData$gender,csvData$diff_prof_twt),]
tmp <- csvData[order(csvData$gender),]
ggplot(data=tmp,aes(x=seq_along(tmp$diff_prof_twt), y=diff_prof_twt, color=gender)) +
  geom_point( )


tmp2 <- csvData[order(csvData$gender),]
ggplot(data=tmp2,aes(x=seq_along(tmp2$tweets_per_day), y=tweets_per_day, color=gender)) +
  geom_point( )




calculateVocabularyScore <- function(tweet_tokens, vocabulary){
  return(sum(vocabulary[match(tweet_tokens, vocabulary$word), "relative"], na.rm = TRUE))
}
tweets_tokens <- tokenize_tweets(csvData$text, stopwords = stopwords)
csvData$score_vocab_brand <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.brand)
csvData$score_vocab_male <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.male)
csvData$score_vocab_female <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.female)

description_tokens <- tokenize_tweets(csvData$description, stopwords = stopwords)
csvData$score_vocabio_brand <- sapply(description_tokens, calculateVocabularyScore, vocabulary = vocabulariosBio.brand)
csvData$score_vocabio_male <- sapply(description_tokens, calculateVocabularyScore, vocabulary = vocabulariosBio.male)
csvData$score_vocabio_female <- sapply(description_tokens, calculateVocabularyScore, vocabulary = vocabulariosBio.female)

predictOnScore <- function(score_vocab_brand, score_vocab_male, score_vocab_female){
  if(score_vocab_brand > score_vocab_male & score_vocab_brand > score_vocab_female)
    return("brand")
  if(score_vocab_male > score_vocab_brand & score_vocab_male > score_vocab_female)
    return("male")
  if(score_vocab_female > score_vocab_brand & score_vocab_female > score_vocab_male)
    return("female")
  
  return(NA)
}

#gender_predicts <- mapply(predictOnScore, csvData$score_vocab_brand,csvData$score_vocab_male,csvData$score_vocab_female)
#pred <- data.frame(gender_predicts,csvData$gender)
#pred <- pred[!is.na(pred$gender_predicts),]
#table(pred)
#mean(pred$gender_predicts == pred$csvData.gender)
#na.omit(gender_predicts == csvData$gender)




csvData$sidebar_color <- NULL
csvData$link_color <- NULL
csvData$description <- NULL
csvData$text <- NULL


str(csvData)

plot_missing(csvData)
plot_histogram(csvData)
plot_bar(csvData)
plot_boxplot(csvData, by = "gender")
create_report(csvData)

summary(csvData$profile_yn)

unique(csvData$gender_gold)
qplot(csvData$gender_gold)

table(csvData$'profile_yn:confidence')
table(csvData$profile_yn_gold) / sum(table(csvData$profile_yn_gold))



set.seed(unclass(Sys.time()))
ids <- sample(nrow(csvData), replace = FALSE)
idsScore <- sample(length(ids), 400, replace = FALSE)
ids <- setdiff(ids, idsScore) #saco los id para scoring
idsTrain <- sample(length(ids), round(0.8 * length(ids)), replace = FALSE)

fwrite(csvData[idsScore,-c("gender")], "data-score.csv", quote = TRUE)
fwrite(csvData[idsTrain,-c("user_id")], "data-train.csv", quote = TRUE)
fwrite(csvData[-idsTrain,-c("user_id")], "data-test.csv", quote = TRUE)