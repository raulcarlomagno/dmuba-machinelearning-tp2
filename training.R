library("data.table")
library("dplyr")
library("mlr")
library("pROC")
library("plotROC")

data <- fread("data.csv", data.table = FALSE, stringsAsFactors = TRUE)
glimpse(data)

train.set <- sample(nrow(data), size = round(0.8 * nrow(data)), replace = FALSE)
test.set <- setdiff(seq_len(nrow(data)), train.set)

task = makeClassifTask(id = "task1", data = data, target = "gender")
print(task)


#https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/
# #ksvmLrnr o ksvmLrnr2
learnerClasses <- c(
#  "classif.randomForest",
  "classif.glmnet",
  "classif.multinom",
  "classif.kknn",
  "classif.rpart",
  "classif.lda",
  "classif.naiveBayes",
  "classif.C50",
  "classif.ctree"
)
learners <- makeLearners(learnerClasses, predict.type = "prob")

#en la prueba final lo agregamos, tarda mucho en entrenar
#ksvmLrnr = makeLearner("classif.ksvm", predict.type = "prob")
### Tune wrapper for ksvm
#rdesc.inner = makeResampleDesc("Holdout")
#ps = makeParamSet(
#  makeDiscreteParam("C", 2^(-1:1))
#)
#ctrl = makeTuneControlGrid()
#ksvmLrnr2 = makeTuneWrapper(ksvmLrnr, rdesc.inner, ms, ps, ctrl)

#entrenamos los modelos por separado para ver su performance individual
measures <- list(acc, mmce, mlr::multiclass.aunu)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
bmr <- benchmark(learners, tasks = task, resampling = rdesc, measures = measures)
bmr
#df <- generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce))
#plotROCCurves(df)



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



