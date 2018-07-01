library("data.table")
library("dplyr")
library("mlr")
library("pROC")
library("plotROC")

data <- fread("data.csv", data.table = FALSE, stringsAsFactors = TRUE)
glimpse(data)

dataScore <- fread("data-score.csv", data.table = FALSE, stringsAsFactors = TRUE)
rownames(dataScore) <- dataScore$user_id
dataScore$user_id <- NULL

train.set <- sample(nrow(data), size = round(0.8 * nrow(data)), replace = FALSE)
test.set <- setdiff(seq_len(nrow(data)), train.set)

task = makeClassifTask(id = "task1", data = data, target = "gender")
print(task)

learnerClasses <- c(
  "classif.randomForest",
  "classif.glmnet",
  "classif.multinom",
  "classif.kknn",
  "classif.rpart",
  "classif.lda",
  "classif.naiveBayes",
  "classif.C50",
  "classif.ctree"
)
# [1] "GLM with Lasso or Elasticnet Regularization"
# [1] "Multinomial Regression"
# [1] "k-Nearest Neighbor"
# [1] "Decision Tree"
# [1] "Linear Discriminant Analysis"
# [1] "Naive Bayes"
# [1] "C50"
# [1] "Conditional Inference Trees"

learners <- makeLearners(learnerClasses, predict.type = "prob")

#entrenamos los modelos por separado para ver su performance individual
measures <- list(acc, mmce, mlr::multiclass.aunu)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
bmr <- benchmark(learners, tasks = task, resampling = rdesc, measures = measures)
bmr
#df <- generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce))
#plotROCCurves(df)



getRocs <- function(results){
  dfRocAllIntegrated <- data.frame(value = integer(), prob = double(), learner = character(), class = character())
  
  for (i in 1:length(results)){
      lrnResult <- results[[i]]
      lrnName <- lrnResult$learner$callees[1]

      meanIter <- setNames(aggregate(truth == response ~ iter, lrnResult$pred$data, mean), c("iter", "acc"))      
      mejorIterIdx <- head(meanIter[order(-meanIter$acc),]$iter, 1)
      mejorIterData <- lrnResult$pred$data[lrnResult$pred$data$iter == mejorIter,]
      
      #print(lrnName)

      dfRocBrandTemp <- data.frame(value = ifelse(mejorIterData$truth == 'brand', 1, 0), prob = mejorIterData$prob.brand, learner = lrnName, class = 'brand')
      dfRocMaleTemp <- data.frame(value = ifelse(mejorIterData$truth == 'male', 1, 0), prob = mejorIterData$prob.male, learner = lrnName, class = 'male')
      dfRocFemaleTemp <- data.frame(value = ifelse(mejorIterData$truth == 'female', 1, 0), prob = mejorIterData$prob.female, learner = lrnName, class = 'female')
      
      dfRocAllIntegrated <- rbind(dfRocAllIntegrated, dfRocBrandTemp)
      dfRocAllIntegrated <- rbind(dfRocAllIntegrated, dfRocMaleTemp)
      dfRocAllIntegrated <- rbind(dfRocAllIntegrated, dfRocFemaleTemp)
  }
  
  return(dfRocAllIntegrated)
}

dfRocsAll <- getRocs(bmr$results$task1)

ggplot(dfRocsAll[dfRocsAll$class == 'brand',], aes(d = value, m = prob, color = learner)) +
  ggtitle("Brand vs (Male & Female)") +
  geom_roc(n.cuts = 0) +
  style_roc()

ggplot(dfRocsAll[dfRocsAll$class == 'male',], aes(d = value, m = prob, color = learner)) +
  ggtitle("Male vs (Brand & Female)") +
  geom_roc(n.cuts = 0) +
  style_roc()

ggplot(dfRocsAll[dfRocsAll$class == 'female',], aes(d = value, m = prob, color = learner)) +
  ggtitle("Female vs (Male & Brand)") +
  geom_roc(n.cuts = 0) +
  style_roc()



#creamos el modelo ensamblado
stackedLearner <- makeStackedLearner(base.learners = learners, predict.type = "prob", method = "hill.climb")
ensembleModel = train(stackedLearner, task, subset = train.set)
ensemblePred = predict(ensembleModel, task, subset = test.set)

ensemblePerf <- performance(ensemblePred, measures = list(acc, mmce, multiclass.aunu))
ensemblePerf
#measureMMCE(ensemblePred$data$truth, ensemblePred$data$response)
#measureACC(ensemblePred$data$truth, ensemblePred$data$response)

aucBrand <- measureAUC(probabilities = ensemblePred$data$prob.brand, truth = ensemblePred$data$truth,  positive = "brand")
aucMale <- measureAUC(probabilities = ensemblePred$data$prob.male, truth = ensemblePred$data$truth,  positive = "male")
aucFemale <- measureAUC(probabilities = ensemblePred$data$prob.female, truth = ensemblePred$data$truth,  positive = "female")
aucAvg <- ((aucBrand + aucMale + aucFemale) / 3)


brandRoc <- roc(ifelse(ensemblePred$data$truth == 'brand', 'brand', 'other'), ensemblePred$data$prob.brand)
brandRoc
dfRocBrand <- data.frame(value = ifelse(ensemblePred$data$truth == 'brand', 1, 0), prob = ensemblePred$data$prob.brand)
ggplot(dfRocBrand, aes(d = value, m = prob)) +
  ggtitle("Brand vs (Male & Female)") +
  annotate("text", x = .75, y = .25, label = paste("AUC =", round(brandRoc$auc, 2))) +
  geom_roc(n.cuts = 0) +
  style_roc()


maleRoc <- roc(ifelse(ensemblePred$data$truth == 'male', 'male', 'other'), ensemblePred$data$prob.male)
maleRoc
dfRocMale <- data.frame(value = ifelse(ensemblePred$data$truth == 'male', 1, 0), prob = ensemblePred$data$prob.male)
ggplot(dfRocMale, aes(d = value, m = prob)) +
  ggtitle("Male vs (Brand & Female)") +
  annotate("text", x = .75, y = .25, label = paste("AUC =", round(maleRoc$auc, 2))) +
  geom_roc(n.cuts = 0) +
  style_roc()


femaleRoc <- roc(ifelse(ensemblePred$data$truth == 'female', 'female', 'other'), ensemblePred$data$prob.female)
femaleRoc
dfRocFemale <- data.frame(value = ifelse(ensemblePred$data$truth == 'female', 1, 0), prob = ensemblePred$data$prob.female)
ggplot(dfRocFemale, aes(d = value, m = prob)) +
  ggtitle("Female vs (Brand & Male)") +
  annotate("text", x = .75, y = .25, label = paste("AUC =", round(femaleRoc$auc, 2))) +
  geom_roc(n.cuts = 0) +
  style_roc()


dfRocAll <- rbind(cbind(dfRocBrand, class = "brand"), cbind(dfRocMale, class = "male"), cbind(dfRocFemale, class = "female"))
ggplot(dfRocAll, aes(d = value, m = prob, color = class)) +
  ggtitle("Brand vs Male vs Female") +
  annotate("text", x = .75, y = .15, label = paste("Avg. AUC =", round(aucAvg, 2))) +
  annotate("text", x = .75, y = .45, label = paste("Brand AUC =", round(brandRoc$auc, 2))) +
  annotate("text", x = .75, y = .35, label = paste("Male AUC =", round(maleRoc$auc, 2))) +
  annotate("text", x = .75, y = .25, label = paste("Female AUC =", round(femaleRoc$auc, 2))) +
  geom_roc(n.cuts = 0) +
  style_roc()


#https://github.com/PhilippPro/MulticlassAUC/blob/master/MulticlassAUC.R
#library(caTools)
#colAUC(getPredictionProbabilities(ensemblePred), ensemblePred$data$truth, plotROC=TRUE)



ensembleModelScoring = train(stackedLearner, task)
ensemblePredScores = predict(ensembleModelScoring, makeClassifTask(id = "dataScore", data = dataScore, target = "gender"))
ensemblePredPerf <- performance(ensemblePredScores, measures = list(acc, mmce, multiclass.aunu))
ensemblePredPerf
dataScored <- data.frame(ID = rownames(dataScore), gender = ensemblePredScores$data$response)
write.csv(dataScored, "data-scored.csv", row.names = FALSE) 
