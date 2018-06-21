library("data.table")
library("dplyr")
library("mlr")

data <- fread("data.csv", data.table = FALSE)

#por ahora hago clasificacion binaria, cuando sea multiclass,  poner stringasfactors = TRUE en fread
data[data$gender == 'male' | data$gender == 'female', 'gender'] <- 'human'
data$sidebar_color_cat <- as.factor(data$sidebar_color_cat)
data$link_color_cat <- as.factor(data$link_color_cat)

glimpse(data)

#defino la tarea de clasifiacion, hacerla multiclass despues
task = makeClassifTask(data = data, target = "gender")
print(task)

#measures disponibles https://mlr-org.github.io/mlr/articles/tutorial/devel/measures.html
#fpr (False positive rate): Percentage of misclassified observations in the positive class
#tpr (True positive rate): Percentage of correctly classified observations in the positive class
#mmce (Mean misclassification error): Defined as: mean(response != truth)
#acc (Accuracy): Defined as: mean(response == truth)


train.set <- sample(nrow(data), size = round(2/3 * nrow(data)))
test.set <- setdiff(seq_len(nrow(data)), train.set)

#https://mlr-org.github.io/mlr-tutorial/release/html/integrated_learners/
# #ksvmLrnr o ksvmLrnr2
learnerClasses <- c(
  #"classif.randomForest",
  "classif.kknn",
  "classif.rpart",
  "classif.lda",
  "classif.logreg",
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
measures <- list(auc, mmce)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)
bmr <- benchmark(learners, tasks = task, resampling = rdesc, measures = measures)
bmr
df <- generateThreshVsPerfData(bmr, measures = list(fpr, tpr, mmce))
plotROCCurves(df)


#creamos el modelo ensamblado
stackedLearner <- makeStackedLearner(base.learners = learners, predict.type = "prob", method = "hill.climb")
ensembleModel = train(stackedLearner, task, subset = train.set)
ensemblePred = predict(ensembleModel, task, subset = test.set)

measureMMCE(data[test.set, "gender"], ensemblePred$data$response)
#o
measureACC(data[test.set, "gender"], ensemblePred$data$response)

