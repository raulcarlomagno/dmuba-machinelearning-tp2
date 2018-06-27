library("data.table")
library("dplyr")
library("mlr")

data <- fread("data.csv", data.table = FALSE, stringsAsFactors = TRUE)

#por ahora hago clasificacion binaria, cuando sea multiclass,  poner stringasfactors = TRUE en fread
#data[data$gender == 'male' | data$gender == 'female', 'gender'] <- 'human'
#data$sidebar_color_cat <- as.factor(data$sidebar_color_cat)
#data$link_color_cat <- as.factor(data$link_color_cat)

glimpse(data)

train.set <- sample(nrow(data), size = round(0.8 * nrow(data)))
test.set <- setdiff(seq_len(nrow(data)), train.set)

#defino la tarea de clasifiacion, hacerla multiclass despues
task = makeClassifTask(id = "task1", data = data, target = "gender")
print(task)

#measures disponibles
#https://mlr-org.github.io/mlr-tutorial/release/html/measures/index.html
#https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf
#fpr (False positive rate): Percentage of misclassified observations in the positive class
#tpr (True positive rate): Percentage of correctly classified observations in the positive class
#mmce (Mean misclassification error): Defined as: mean(response != truth)
#acc (Accuracy): Defined as: mean(response == truth)

#https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/
# #ksvmLrnr o ksvmLrnr2
learnerClasses <- c(
#  "classif.randomForest",
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

measureMMCE(ensemblePred$data$truth, ensemblePred$data$response)
measureACC(ensemblePred$data$truth, ensemblePred$data$response)
measureAUC(probabilities = ensemblePred$data$prob.brand, truth = data[test.set, "gender"],  positive = "brand")
measureAUC(probabilities = ensemblePred$data$prob.male, truth = data[test.set, "gender"],  positive = "male")
measureAUC(probabilities = ensemblePred$data$prob.female, truth = data[test.set, "gender"],  positive = "female")

library(pROC)
prueba <- multiclass.roc(response = ensemblePred$data$truth, predictor = as.numeric(ensemblePred$data$response))
prueba
#plot(prueba$rocs[1], print.auc=TRUE)
