library(mlr3)
library(mlr3spatiotempcv)
library(ggplot2)

library(blockCV)
library(sf)
library(R6)
library(paradox)
library(data.table)


# ResamplingSpCVBlock - No paramter

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)

# ResamplingSpCVBlock - range + checkerboard

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4, range=500, selection = "checkerboard")
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)

# ResamplingSpCVBlock - rows + cols + systematic

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4, rows = 10, cols = 10, selection = "systematic")
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)

# ResamplingSpCVBlock - rows + cols + range

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4, rows = 10, cols = 10, range=1000)
rcv$instantiate(task)


# ResamplingSpCVEnv - All features

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVEnv$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)

# ResamplingSpCVEnv - feature selection

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVEnv$new()
rcv$param_set$values = list(folds = 4, features=c("dem"))
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)

# ResamplingSpCVEnv - Wrong feature

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVEnv$new()
rcv$param_set$values = list(folds = 4, features=c("abc"))
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task)


# ResampleSpCVKmeans - No parameter
task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVCoords$new()
rcv$instantiate(task)

resample(task, learner, rcv)

autoplot(rcv, task)

# ResampleSpCVKmeans - folds
task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVCoords$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task)


# ResampleSpCVBuffer - range
task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBuffer$new()
rcv$param_set$values = list(range = 1000)
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task, 1)

# ResampleSpCVBuffer - No parameter
task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBuffer$new()
rcv$instantiate(task)

resampling = resample(task, learner, rcv)

autoplot(rcv, task, 1)


# Vizualiation - Multiplot all folds

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task, fold_id=c(1,2,3,4))

# Vizualiation - Multiplot

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task, fold_id=c(1,2,3))

# Vizualiation - Too many folds

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task, fold_id=c(1,2,3,4,5))

# Vizualiation - One fold

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task, fold_id=1)

# Vizualiation - All folds one plot

task = mlr3::mlr_tasks$get("ecuador")
learner = mlr_learners$get("classif.rpart")

rcv = ResamplingSpCVBlock$new()
rcv$param_set$values = list(folds = 4)
rcv$instantiate(task)

autoplot(rcv, task)

