# 00-Util.R
# !/usr/bin/env Rscript
#
# BINF6970 Final Exam 2021
#
# @author: Syed Ali, Kassaye Belay, Lisa Hoeg, Liam Lalonde
library(tidyverse)
library(stringi)

multi_roc_list <- function(roc.list, roc.names = NA) {
  # Plots multiple ROCs from a list

  data <- data.frame()

  for (i in seq_along(roc.list)) {
    r <- roc.list[[i]]
    d <- r$data
    if (is.na(roc.names)) {
      d$Group <- paste0("ROC_", i)
    } else {
      d$Group <- roc.names[[i]]
    }
    data <- rbind(data, d)
  }

  names(data) <- c("TPR", "FPR", "Model")

  p <- ggplot(data, aes(FPR, TPR, color = Model)) +
    geom_line(size = 2, alpha = 0.7) +
    labs(
      title = "ROC curve",
      x = "False Positive Rate (1-Specificity)",
      y = "True Positive Rate (Sensitivity)"
    )

  p
}

modefunc <- function(x) {
  # The mode function to get the most occuring values in all the data sets.
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if (sum(tabresult == max(tabresult)) > 1) themode <- 0
  return(themode)
}

boot_strap <- function(data, n) {
  # This function bootstraps the dataset using n number of resamples and returnsa  list of dataframes.
  resamples <- vector(mode = "list", length = n)
  for (i in seq_len(n)) {
    resamples[[i]] <- data[sample(seq_len(nrow(data)), nrow(data), replace = TRUE), ]
  }
  return(resamples)
}

var_imp_plot <- function(data, num.vars = NA, rename = TRUE) {
  # Variable importance plot. Df must be Feature | Importance

  df.vimp <- data %>%
    drop_na() %>%
    mutate_if(is.factor, as.character)
  names(df.vimp) <- c("var", "imp")

  if (!(is.na(num.vars))) {
    tn <- num.vars
  } else {
    tn <- nrow(df.vimp)
  }

  # Rename the features from XN to the actual labels, skip if already done. Including Interactions
  if (rename) {
    df.vimp$var <- remap_features(as.character(df.vimp$var))
  }

  df.vimp %>%
    arrange(imp, var) %>%
    mutate(var = factor(var, levels = var)) %>%
    tail(tn) %>%
    ggplot() +
    geom_segment(aes(x = var, y = 0, xend = var, yend = imp),
      size = 1.5, alpha = 0.7
    ) +
    geom_point(aes(x = var, y = imp, col = var),
      size = 4, show.legend = F
    ) +
    coord_flip() +
    xlab("Feature") +
    ylab("Importance")
}


get_perf <- function(actual, pred) {
  # Takes the actual, and predicted values and returns a df of summary stats
  cm <- table(actual, pred)
  accuracy <- sum(diag(cm)) / sum(cm)
  precision <- posPredValue(pred, actual, positive = "M")
  recall <- sensitivity(pred, actual, positive = "M")
  fscore <- (2 * precision * recall) / (precision + recall)
  data.frame(accuracy = accuracy, precision = precision, recall = recall, fscore = fscore)
}


create_label_mappings <- function(write = FALSE) {
  # This procedure just saves a file mapping XN to the actual column names from the data set
  attributes <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_pts", "symmetry", "fractal_dim")
  bcw_names <- paste0(rep(attributes, 3), c(rep("_mean", 10), rep("_se", 10), rep("_worst", 10)))
  xn <- paste0("X", seq(1:30))
  df.labels <- data.frame(X = xn, N = bcw_names)

  if (write) {
    write.csv(df.labels, "./data/label_mappings.csv", row.names = FALSE)
  }

  return(df.labels)
}

remap_features <- function(feats) {
  # This function takes a vector of features and switches the labels to/from XN to the actual names

  # Mappings
  df.map <- create_label_mappings()

  # Check which direction to swap
  if (feats %in% df.map$X) {
    from <- df.map$X
    to <- df.map$N
  } else {
    from <- df.map$N
    to <- df.map$X
  }

  # What seperates features
  char.sep <- ":"

  for (i in 1:length(feats)) {
    for (j in 1:nrow(df.map)) {
      f <- unlist(str_split(feats[[i]], char.sep))
      m <- mapvalues(f, from = from, to = to, warn_missing = FALSE)
      p <- paste(m, collapse = ":")
      feats[[i]] <- p
    }
  }

  return(feats)
}



draw_confusion_matrix <- function(cm, classes = c("M", "B"), title = "Confusion Matrix") {
  # This function to plot confusion matrixes from caret models wass adapted from Cybernetic.
  # https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package

  # Define Classes
  Class1 <- classes[[1]]
  Class2 <- classes[[2]]

  layout(matrix(c(1, 1, 2)))
  par(mar = c(2, 2, 2, 2))
  plot(c(100, 345), c(300, 450), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  title(title, cex.main = 2)

  # create the matrix
  rect(150, 430, 240, 370, col = "#3F97D0")
  text(195, 435, Class1, cex = 1.2)
  rect(250, 430, 340, 370, col = "#F7AD50")
  text(295, 435, Class2, cex = 1.2)
  text(125, 370, "Predicted", cex = 1.3, srt = 90, font = 2)
  text(245, 450, "Actual", cex = 1.3, font = 2)
  rect(150, 305, 240, 365, col = "#F7AD50")
  rect(250, 305, 340, 365, col = "#3F97D0")
  text(140, 400, Class1, cex = 1.2, srt = 90)
  text(140, 335, Class2, cex = 1.2, srt = 90)

  # add in the cm results
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex = 1.6, font = 2, col = "white")
  text(195, 335, res[2], cex = 1.6, font = 2, col = "white")
  text(295, 400, res[3], cex = 1.6, font = 2, col = "white")
  text(295, 335, res[4], cex = 1.6, font = 2, col = "white")

  # add in the specifics
  plot(c(100, 0), c(100, 0), type = "n", xlab = "", ylab = "", main = "DETAILS", xaxt = "n", yaxt = "n")
  text(10, 85, names(cm$byClass[1]), cex = 1.2, font = 2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex = 1.2)
  text(30, 85, names(cm$byClass[2]), cex = 1.2, font = 2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex = 1.2)
  text(50, 85, names(cm$byClass[5]), cex = 1.2, font = 2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex = 1.2)
  text(70, 85, names(cm$byClass[6]), cex = 1.2, font = 2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex = 1.2)
  text(90, 85, names(cm$byClass[7]), cex = 1.2, font = 2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex = 1.2)

  # add in the accuracy information
  text(30, 35, names(cm$overall[1]), cex = 1.5, font = 2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex = 1.4)
  text(70, 35, names(cm$overall[2]), cex = 1.5, font = 2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex = 1.4)
}

###################
### End of File ###
###################


# 01-Problem1.R
# !/usr/bin/env Rscript
#
# BINF6970 Final Exam 2021
#
# @author: Syed Ali, Kassaye Belay, Lisa Hoeg, Liam Lalonde

library(tidyverse)
library(randomForest)
library(caret)
library(doParallel)
library(ggplot2)
library(ggROC)
library(plyr)
library(rpart)
library(glmnet)
library(pROC)
library(MLeval)
library(ggpubr)


## -------------------------------
### Load Data Sets
## -------------------------------

# Setup for running in paralell
num.cores <- (detectCores() - 1)

# Breast cancer data set:
df.trg <- read_csv("./data/trainset.csv") %>%
  mutate_if(is.character, as.factor)

df.tst <- read_csv("./data/testset.csv") %>%
  mutate_if(is.character, as.factor)

# Using the same control for all models
fit.control <- trainControl(method = "cv", number = 10, savePredictions = TRUE, classProbs = TRUE, summaryFunction = twoClassSummary)

## -------------------------------
### Net Elastic Model
## -------------------------------

# Train the model
set.seed(1212)

# Parallel
registerDoParallel(cores = num.cores)
fit.tuned <- train(status ~ .,
  data = df.trg,
  method = "glmnet",
  standardize = TRUE,
  metric = "ROC", preProcess = c("scale", "center"),
  trControl = fit.control,
  tuneLength = 10
)

# With quadratics
fit.tuned.2 <- train(status ~ (.^2),
  data = df.trg,
  method = "glmnet",
  trControl = fit.control,
  standardize = TRUE,
  metric = "ROC", preProcess = c("scale", "center"),
  tuneLength = 10
)
stopImplicitCluster()

# Save the models, Stats will be calculated later
models.net <- list(method = "net", fit1 = fit.tuned, fit2 = fit.tuned.2)

## -------------------------------
### Random Forest Model
## -------------------------------

set.seed(1212)

# Grid search hyper param tuning
cv.grid <- expand.grid(.mtry = (1:30))

# Train the model Normal and Quadratic effects
registerDoParallel(cores = num.cores)
fit.tuned <- train(status ~ .,
  data = df.trg,
  trControl = fit.control,
  method = "rf",
  metric = "ROC",
  tuneGrid = cv.grid
)
fit.tuned.2 <- train(status ~ (.^2),
  data = df.trg,
  trControl = fit.control,
  method = "rf",
  metric = "ROC",
  tuneGrid = cv.grid
)
stopImplicitCluster()

# Save
models.rf <- list(method = "rf", fit1 = fit.tuned, fit2 = fit.tuned.2)

## -------------------------------
### Pruned & Bagged CART
## -------------------------------

# Train the model
set.seed(1212)

# Train the model (Normal)
registerDoParallel(cores = num.cores)
fit.tuned <- train(status ~ .,
  data = df.trg,
  method = "treebag", # bagging
  trControl = fit.control,
  nbagg = 100,
  metric = "ROC",
  control = rpart.control(minsplit = 10, cp = 0.02), # pruning
  importance = TRUE
)

# Train the model (Quadratics)
fit.tuned.2 <- train(status ~ (.^2),
  data = df.trg,
  method = "treebag", # bagging
  trControl = fit.control,
  nbagg = 100,
  metric = "ROC",
  control = rpart.control(minsplit = 10, cp = 0.02), # pruning
  importance = TRUE
)
stopImplicitCluster()

# Save
models.cart <- list(method = "cart", fit1 = fit.tuned, fit2 = fit.tuned.2)

## -------------------------------
### KNN Model
## -------------------------------

set.seed(1212)

registerDoParallel(cores = num.cores)

# Normal
fit.tuned <- train(status ~ .,
  data = df.trg,
  method = "knn", trControl = fit.control,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneLength = 20
)

# Quadratic
fit.tuned.2 <- train(status ~ (.^2),
  data = df.trg,
  method = "knn", trControl = fit.control,
  metric = "ROC",
  preProcess = c("center", "scale"),
  tuneLength = 20
)

# Also 2 more models for feature selection instead of predicition

# Set cross-validation parameters for feature selection
rctrl1 <- rfeControl(
  method = "cv",
  number = 3,
  returnResamp = "all",
  functions = caretFuncs,
  saveDetails = TRUE
)

# Train the model (Normal)
fit.tuned.feat <- rfe(status ~ .,
  data = df.trg,
  sizes = c(5, 10, 15, 20),
  method = "knn",
  preProcess = c("center", "scale"),
  trControl = trainControl(
    method = "cv",
    classProbs = TRUE
  ),
  tuneGrid = data.frame(k = 3:15),
  rfeControl = rctrl1
)


# Train the model for feature selection (Quadratics)
fit.tuned.feat.2 <- rfe(status ~ (.^2),
  data = df.trg,
  sizes = c(20, 30),
  method = "knn",
  preProcess = c("center", "scale"),
  trControl = trainControl(
    method = "cv",
    classProbs = TRUE
  ),
  tuneGrid = data.frame(k = 3:15),
  rfeControl = rctrl1
)

stopImplicitCluster()

# Save. Note KNN has an extra 3rd model and 4th item in the list
models.knn <- list(method = "knn", fit1 = fit.tuned, fit2 = fit.tuned.2, fit3 = fit.tuned.feat, fit4 = fit.tuned.feat.2)


## -------------------------------
### Calculate summary stats
## -------------------------------

# Combine all models into 1 list
models.all <- c(
  list(models.net),
  list(models.rf),
  list(models.cart),
  list(models.knn)
)

# data frame to store summary of results
df.perf <- data.frame()

# Also store the rocs
list.rocs <- list()

# Collate stats on the models
for (model in models.all) {
  # The method, net, rf, etc
  method <- model[[1]]

  # The normal non quadratic model
  model.1 <- model[[2]]

  # The quadratic model
  model.2 <- model[[3]]

  # Predictions
  pred.1 <- predict(model.1, newdata = df.tst, type = "prob")
  pred.2 <- predict(model.2, newdata = df.tst, type = "prob")

  # Calculate metrics
  eval.1 <- evalm(data.frame(pred.1, df.tst$status))
  eval.2 <- evalm(data.frame(pred.2, df.tst$status))

  # Extract metrics
  metrics <- row.names(eval.1$stdres$Group1)

  model.1.metrics <- eval.1$stdres$Group1$Score
  model.2.metrics <- eval.2$stdres$Group1$Score

  # Combine
  df.metrics <- data.frame(metric = metrics, norm = model.1.metrics, quad = model.2.metrics, method = method)

  # Convert to long
  df.merge <- df.metrics %>%
    pivot_longer(cols = c("norm", "quad"), names_to = "mode", values_to = "value")

  # Add to df
  df.perf <- rbind(df.perf, df.merge)

  list.rocs <- c(list.rocs, list(list(method = method, eval1 = eval.1, eval2 = eval.2)))
}

# Save to disk
df.perf %>% write_csv("./data/p1_perfourmance.csv")

# String as factors
df.perf <- df.perf %>%
  mutate_if(is.character, as.factor)


## -------------------------------
### Collate Variable Importance
## -------------------------------

# data frame to store summary of results
df.vimp <- data.frame()

# Same as above
for (model in models.all) {
  method <- model[[1]]

  # KNN is special and must use the 3rd and fourth models instead
  if (method == "knn") {
    model.1 <- model[[4]]
    model.2 <- model[[5]]

    vimp.1 <- varImp(model.1) %>% rownames_to_column()
    vimp.2 <- varImp(model.2) %>% rownames_to_column()
  } else {
    model.1 <- model[[2]]
    model.2 <- model[[3]]

    vimp.1 <- varImp(model.1)$importance %>% rownames_to_column()
    vimp.2 <- varImp(model.2)$importance %>% rownames_to_column()
  }

  # Calculate feature importance
  vimps <- full_join(vimp.1, vimp.2, by = "rowname")
  names(vimps) <- c("feat", "norm", "quad")

  # Convert to long
  df.merge <- vimps %>%
    pivot_longer(cols = c("norm", "quad"), names_to = "mode", values_to = "import") %>%
    mutate(method = method) %>%
    select(feat, method, mode, import)

  # Add to df
  df.vimp <- rbind(df.vimp, df.merge)
}

# Save to disk
df.vimp %>% write_csv("./data/p1_features.csv")

# String as factors
df.vimp <- df.vimp %>%
  mutate_if(is.character, as.factor)

## -------------------------------
### Plotting
## -------------------------------

## Feature Importance

df.vimp %>%
  filter(method == "net") %>%
  filter(mode == "norm") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "net") %>%
  filter(mode == "quad") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "rf") %>%
  filter(mode == "norm") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "rf") %>%
  filter(mode == "quad") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "cart") %>%
  filter(mode == "norm") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "cart") %>%
  filter(mode == "quad") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "knn") %>%
  filter(mode == "norm") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)

df.vimp %>%
  filter(method == "knn") %>%
  filter(mode == "quad") %>%
  select(feat, import) %>%
  var_imp_plot(., num.vars = 30, rename = TRUE)


# Performance summaries


df.perf %>%
  filter(metric == "AUC-ROC") %>%
  ggplot(aes(y = value, x = method, fill = mode)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # Theme/Formatting
  scale_fill_discrete(name = "Mode", labels = c("Normal", "Quadratic")) +
  ylab("Accuracy") +
  scale_x_discrete(name = "Model", labels = c("Cart", "KNN", "Net Elastic", "Random Forest")) +
  theme(axis.title.x = element_blank())


# A list of all models (1 list)
models.resample <- list(
  "Net Elastic (Normal)" = models.all[[1]]$fit1,
  "Random Forest (Normal)" = models.all[[2]]$fit1,
  "CART (Normal)" = models.all[[3]]$fit1,
  "KNN (Normal)" = models.all[[4]]$fit1,
  "Net Elastic (Quadratic)" = models.all[[1]]$fit2,
  "Random Forest (Quadratic)" = models.all[[2]]$fit2,
  "CART (Quadratic)" = models.all[[3]]$fit2,
  "KNN (Quadratic)" = models.all[[4]]$fit2
)

# Resample
models.resample <- resamples(models.resample)
bwplot(models.resample, metric = "ROC")

# Individual methods plots


## ROC PLOTS
ggarrange(
  # Net
  multi_roc_list(
    list(
      list.rocs[[1]]$eval1$roc,
      list.rocs[[1]]$eval2$roc
    ), c("Normal", "Quadratic")
  ) + ggtitle("A. Net Elastic") + theme(axis.title.x = element_blank()),

  # Rf
  multi_roc_list(
    list(
      list.rocs[[2]]$eval1$roc,
      list.rocs[[2]]$eval2$roc
    ), c("Normal", "Quadratic")
  ) + ggtitle("B. Random Forest") + theme(axis.title.x = element_blank(), axis.title.y = element_blank()),
  # Cart
  multi_roc_list(
    list(
      list.rocs[[3]]$eval1$roc,
      list.rocs[[3]]$eval2$roc
    ), c("Normal", "Quadratic")
  ) + ggtitle("C. CART"),
  # Knn
  multi_roc_list(
    list(
      list.rocs[[4]]$eval1$roc,
      list.rocs[[4]]$eval2$roc
    ), c("Normal", "Quadratic")
  ) + ggtitle("D. KNN") + theme(axis.title.y = element_blank()),
  common.legend = TRUE, ncol = 2, nrow = 2, legend = "bottom"
)

# ROC plot with all
multi_roc_list(list(
  list.rocs[[1]]$eval1$roc,
  list.rocs[[1]]$eval2$roc,
  list.rocs[[2]]$eval1$roc,
  list.rocs[[2]]$eval2$roc,
  list.rocs[[3]]$eval1$roc,
  list.rocs[[3]]$eval2$roc,
  list.rocs[[4]]$eval1$roc,
  list.rocs[[4]]$eval2$roc
), roc.names = c(
  "Net Elastic (Normal)",
  "Net Elastic (Quadratic)",
  "Random Forest (Normal)",
  "Random Forest (Quadratic)",
  "CART (Normal)",
  "CART (Quadratic)",
  "KNN (Normal)",
  "KNN (Quadratic"
))


# Confusion matrices
actual <- df.tst$status

# Net Elastic Normal
pred <- predict(models.all[[1]]$fit1, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "Net Elastic (Normal)")

# Net Elastic Quad
pred <- predict(models.all[[1]]$fit2, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "Net Elastic (Quadratic)")

# RF Normal
pred <- predict(models.all[[2]]$fit1, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "Random Forest (Normal)")

# RF Quad
pred <- predict(models.all[[2]]$fit2, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "Random Forest (Quadratic)")

# CART Normal
pred <- predict(models.all[[3]]$fit1, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "CART (Normal)")

# CART Quad
pred <- predict(models.all[[3]]$fit2, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "CART (Quadratic)")

# KNN Normal
pred <- predict(models.all[[4]]$fit1, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "KNN (Normal)")

# KNN Quad
pred <- predict(models.all[[4]]$fit2, newdata = df.tst)
draw_confusion_matrix(confusionMatrix(data = pred, reference = actual), title = "KNN (Quadratic)")

## KNN

# Check best parameters
fits.knn <- models.all[[4]]

# Top variables
top.var.knn <- fits.knn$fit3$optVariables
top.var.knn.2 <- fits.knn$fit4$optVariables
# Best k values
fits.knn$fit3$fit$results[which.max(fits.knn$fit3$fit$results[, 2]), ]
fits.knn$fit4$fit$results[which.max(fits.knn$fit4$fit$results[, 2]), ]

# Visualize error by number of neighbors
plot(fits.knn$fit3$fit)
plot(fits.knn$fit4$fit)

## Net elastic

# Check best parameters
fits <- models.all[[1]]
fit.tuned <- fits$fit1
fit.tuned.2 <- fits$fit2

# Get best params
fit.tuned$results[which(rownames(fit.tuned$results) == rownames(fit.tuned$bestTune)), ]
fit.tuned.2$results[which(rownames(fit.tuned.2$results) == rownames(fit.tuned.2$bestTune)), ]

plot(fit.tuned)
plot(fit.tuned.2)


# Random Forest
fits <- models.all[[2]]
fit.tuned <- fits$fit1
fit.tuned.2 <- fits$fit2

# Best
fit.tuned$results[which(rownames(fit.tuned$results) == rownames(fit.tuned$bestTune)), ]
fit.tuned.2$results[which(rownames(fit.tuned.2$results) == rownames(fit.tuned.2$bestTune)), ]
plot(fit.tuned)
plot(fit.tuned.2)

###################
### End of File ###
###################


# 02a-Problem2.R
# !/usr/bin/env Rscript
#
# BINF6970 Final Exam 2021
#
# @author: Syed Ali, Kassaye Belay, Lisa Hoeg, Liam Lalonde

#### Uploading the Packages ####
library(glmnet)
library(coefplot)
library(ggplot2)
library(randomForest)
library(pROC)
library(tidyverse)
library(caret)


# Load in Data
df.trg <- read_csv("./data/trainset.csv") %>%
  mutate_if(is.character, as.factor)

df.tst <- read_csv("./data/testset.csv") %>%
  mutate_if(is.character, as.factor)

# Full data set
Merge.Cancer <- bind_rows(df.trg, df.tst)


# There are 30 varables, and 569 observations in the Dataset and organized by Samples status where B = Benign and M = Malignant  ####

Merge.Cancer <- rbind(Cancer.test, Cancer.train)

attributes <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_pts", "symmetry", "fractal_dim")
bcw_names <- paste0(rep(attributes, 3), c(rep("_mean", 10), rep("_se", 10), rep("_worst", 10)))
colnames(Merge.Cancer) <- c("status", bcw_names)

#### Preparing the Data for Lasso Regression #####

Merge.Cancer$status <- as.factor(Merge.Cancer$status) #### Converting the status to factors

x <- model.matrix(status ~ ., Merge.Cancer)[, -1] # Modelling as a matrix for all the variables
y <- ifelse(Merge.Cancer$status == "B", 0, 1) # Converitng The B's to 0 and M's 1

##### Lasso Regression Cross Validation #####

Lasso.Can <- cv.glmnet(x, y, alpha = 1, nfolds = 10, family = "binomial", type.measure = "auc") # Cross validation 10 fold lasso regression.

plot(Lasso.Can) # Plotting the cross validation (lasso)

lambda <- sort(Lasso.Can$lambda) # Extracting the lamda values

wh <- which(lambda >= Lasso.Can$lambda.min & lambda <= Lasso.Can$lambda.1se) # Determing which values have similar predictive strength.

sps.set <- lambda[wh] # Extracting the values of similar predictive strength

prd.sps.set <- predict(Lasso.Can, newx = x, s = Lasso.Can$lambda.min) # Predicting the response variable for lambda values of similar predictive strength
auc(y, prd.sps.set)

#### Fitting the glm model with lasso using optimized lambda values ####

cvxg <- glmnet(x, y, lambda = Lasso.Can$lambda, standardize = TRUE) # glm fitting with lasso model. Standerdization has been set to TRUE for variables to standerdized. Think of it as scaling the values.
coefpath(cvxg) # L1 plot
plot(cvxg, xvar = "lambda")

#### Fitting the glm model with lasso using lamda.min, determining the coefficients and standerdizing them ####

BestModel_lasso <- glmnet(x, y, lambda = Lasso.Can$lambda.min, standardize = TRUE) # glm fit with lasso regression for best model which is lambda.min. Variables is standardize
coefplot(BestModel_lasso, sort = "magnitude")

# Adapted from Carets VarImp() function
beta <- predict(BestModel_lasso, s = Lasso.Can$lambda.min, type = "coef")
if (is.list(beta)) {
  out <- do.call("cbind", lapply(beta, function(x) x[, 1]))
  out <- as.data.frame(out)
} else {
  out <- data.frame(Overall = beta[, 1])
}
out <- abs(out[rownames(out) != "(Intercept)", , drop = FALSE])
out

#### Making the coefficients plot using ggplot ####

new.standerd <- as.data.frame(out) # reformatting as a data.frame as it turn to matrix class # adding the column names back
colnames(new.standerd) <- "New"

#### Making the Importance plot using ggplot ####

ggplot(new.standerd, aes(x = reorder(rownames(new.standerd), New), y = New, color = as.factor(rownames(new.standerd)))) +
  geom_point() +
  geom_segment(aes(x = rownames(new.standerd), xend = rownames(new.standerd), y = 0, yend = New)) +
  scale_color_discrete(name = "rownames(new.standerd)") +
  ylab("Importance Plot ") +
  xlab("Variables") +
  coord_flip() +
  ggtitle("Ordinary Lasso Regression - Variable Importance Plot") +
  theme(legend.position = "none") ### removing the legend

### Random Forest ####
set.seed(1212) # Set seed to get same sample every time
Random.c <- randomForest(status ~ ., data = Merge.Cancer, ntree = 1000, proximity = TRUE, importance = TRUE)
# random forest with proximity and importance set to true to get their visual plots as well
plot(Random.c) # the more trees you make the reduction in error
varImpPlot(Random.c, main = "Random Forest Variable Importance Plot") # plot the importance and node impurity plots
summary(Random.c)

###################
### End of File ###
###################


# 02b-Bootstrap.R
# !/usr/bin/env Rscript
#
# BINF6970 Final Exam 2021
#
# @author: Syed Ali, Kassaye Belay, Lisa Hoeg, Liam Lalonde

library(tidyverse)
library(glmnet)
library(caret)


# Parallelization
library(doParallel)
library(foreach)
num.cores <- (detectCores() - 1)

# Load in Data
df.trg <- read_csv("./data/trainset.csv") %>%
  mutate_if(is.character, as.factor)

df.tst <- read_csv("./data/testset.csv") %>%
  mutate_if(is.character, as.factor)

# Full data set
data <- bind_rows(df.trg, df.tst)

# Number of boots
num.boots <- 1000

# Resample
set.seed(1212)
resamples <- boot_strap(data, num.boots)

# Empty list of length n. STATIC SIZED
list.lassos <- vector(mode = "list", length = num.boots)
list.vimps <- vector(mode = "list", length = num.boots)

# Run in parallel
cl <- makeCluster(num.cores)
registerDoParallel(cl, resamples) # Need to export data to the cluster

# Iterate through each bag in PARALLEL.
list.vimps <- foreach(i = 1:num.boots, .packages = c("caret", "glmnet", "tibble")) %dopar% {

  # Our sample is now resamples[[i]]
  d <- resamples[[i]]

  # Create a lasso model using caret
  fit <- train(status ~ .,
    data = resamples[[i]],
    method = "glmnet", lambda = 0,
    tuneGrid = expand.grid(alpha = 1, lambda = 0)
  )

  # Do stuff for each of the 1000 and put in list below
  vimp <- rownames_to_column(varImp(fit)$importance)
  names(vimp) <- c("var", "imp")

  # Finally return to foreach
  vimp
}

stopCluster(cl)


# Create a data frame with X1-30 as index, and the respective rank values of each of the 1000 resamples

# Will be used to create the ranked dataframe
list.ranks <- vector(mode = "list", length = num.boots)

# Iterate through each of the 1000 samples
for (i in 1:num.boots) {
  d <- list.vimps[[i]]

  # Get Ranks
  r <- d %>%
    mutate(id = 1:nrow(d)) %>%
    group_by(var) %>%
    arrange(desc(imp)) %>%
    ungroup() %>%
    mutate(rank = 1:nrow(d)) %>%
    group_by(var) %>%
    arrange(id) %>%
    ungroup() %>%
    mutate(rank = ifelse(imp == 0, NA, rank))

  # Add to list
  list.ranks[[i]] <- r$rank
}

# Now combine all 1000 into 1 df
df.ranks <- cbind(
  as.data.frame(list.vimps[[1]]$var),
  as.data.frame(do.call(cbind, list.ranks))
)

# Rename columns
names(df.ranks) <- c("variable", paste0("bag_", 1:(ncol(df.ranks) - 1)))


# Next remove all features that have NAs in any of the 1000 resamples. This will leave us with only the common ones.
df.ranks.common <- df.ranks %>%
  select(where(function(x) any(!is.na(x)))) %>%
  drop_na()

# This took a few hours so Save to file for loading
df.ranks.common %>% write_csv("./data/p2_ranking_all.csv")
save.image("./data/p2_ranking_all.RData")


#### Lasso with 1000 bootstrap samples ####

# Build the bootstrap data set, this took a few hours so only do this once and load form disk

Lass.boot <- read.csv("./data/p2_ranking_all.csv") # reading in 1000 bootstrapped samples file

Lass.boot.1 <- Lass.boot[, c(-1)] # removing the variable column to get all numerical columns

sd <- apply(Lass.boot, 1, sd, na.rm = TRUE) # calculating the standard deviation

row.names(Lass.boot.1) <- c("smoothness", "compactness", "concavity", "concave_pts", "symmetry", "fractal_dim", "compactness_se", "concavity_se", "concave_pts_se", "symmetry_se", "fractal_dim_se", "smoothness_worst", "compactness_worst", "concavity_worst", "concave_pts_worst", "symmetry_worst", "fractal_dim_worst") # assigning the variables according to variables seen.

New.model <- apply(Lass.boot.1, 1, modefunc) # The mode function is applied on the data frame.

New.model <- as.data.frame(New.model) # The New.Model object is converted back to class data.frame

New.model <- New.model %>% mutate(sd = sd) # The standard deviation are added to a new column important for error plots generated at the bottom

colnames(New.model) <- "Inter_Model" # Changing the column name for mode values

# The bottom ggplot is for showing the variable coefficients

ggplot(New.model, aes(x = reorder(rownames(New.model), Inter_Model), y = Inter_Model, color = as.factor(rownames(New.model)))) +
  geom_point() +
  geom_segment(aes(x = rownames(New.model), xend = rownames(New.model), y = 0, yend = Inter_Model)) +
  scale_color_discrete(name = "rownames(New.model)") +
  ylab("Coefficient Plot") +
  xlab("Variables") +
  coord_flip() +
  ggtitle("Boot-strapped Lasso Regression - Coefficient Plot") +
  theme(legend.position = "none") ### removing the legend

# The bottom ggplot is for showing the coefficients as bar plot with standard deviations error lines on top of each bar

ggplot(New.model, aes(x = rownames(New.model), y = Inter_Model, fill = rownames(New.model))) +
  geom_bar(stat = "identity", color = "black", position = position_dodge()) +
  geom_errorbar(aes(ymin = Inter_Model, ymax = Inter_Model + sd),
    width = .2,
    position = position_dodge(.9)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(fill = "Variables") +
  ylab("Variable Values") +
  ggtitle("Error Plot of Variables From Boot-Strapped Data")

###################
### End of File ###
###################


# 03-Problem3.R
# !/usr/bin/env Rscript
#
# BINF6970 Final Exam 2021
#
# @author: Syed Ali, Kassaye Belay, Lisa Hoeg, Liam Lalonde

library(tidyverse)
library(data.table)
library(ggplot2)
library(GGally)
library(ggfortify)

# Load the Data set
women_dat <- read.csv("./data/women_dat.csv")
str(women_dat)

## Visualize the
ggpairs(as.data.frame(women_dat[, -1]))

## i. Obtain the sample correlation matrix R for these data and determine its eigenvalue and eigenvectors.
# The sample correlation matrix
## S <- var(women_dat[,-1]) # sample covariance matrix
wcor <- cor(women_dat[, -1])
round(wcor, 2)

# Plot cor
p.mat <- round(cor_pmat(women_dat[, -1]), 2)
ggcorrplot(wcor,
  hc.order = TRUE,
  p.mat = p.mat,
  lab = TRUE
)

## The eigenvalues and eigenvector for women_dat data set
eigen(wcor)
#######################
lambda1 <- eigen(wcor)$val # eigen values
lambda1
e1 <- eigen(wcor)$vec # eigen vectors
e1

# The correlation matrix R is the covariance matrix of the standard data. Yj =ejX is the j-th principal component of the the standard data, where ej is the eigenvector associated to the j-th egigenvalue in decreasing order, of the standard data covariance matrix R. X is standardized random variable.
# Eigenvalues Using princomp() and wcor
princomp(covmat = wcor)
eigen(wcor)$values
princomp(covmat = wcor)$sdev^2

# Using princomp() with original data set
princomp(women_dat[, -1])$sdev^2

## Use prcomp wcor*(N-1)/N
prcomp(women_dat[, -1])$sdev^2

# ii. Determine the first two principal components for the standardized variables. Prepare a table showing the principal components, and their individual and cumulative percentage contribution to the total (standardized) sample variance. Comment on the results.

## First, using eigen() directly:
eigen.vals <- eigen(wcor)$values
eigen.vcs <- eigen(wcor)$vectors ## columns are eigenvectors
wmat <- matrix(NA, ncol = 7, nrow = 9)
wmat[8, ] <- eigen.vals

## cumulative sum of PCA variances
wmat[9, ] <- cumsum(eigen.vals / sum(eigen.vals) * 100)
rownames(wmat) <- c(colnames(women_dat[, -1]), "lambda.hat", "cumulative %")
colnames(wmat) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
wmat[1:7, 1:7] <- eigen.vcs
round(wmat, 3)

# Therefore, the first two principal components are
# Y1 = -0.378X1 - 0.383X2 - 0.368X3 - 0.395X4 - 0.389X5 - 0.376X6 - 0.355X7
# Y2 = -0.407X1 - 0.414X2 - 0.459X3 + 0.161X4 + 0.309X5 + 0.423X6 + 0.389X7

corxy <- matrix(rep(0, 49), ncol = 7)
for (j in 1:7) {
  for (k in 1:7) {
    corxy[j, k] <- lambda1[j] * e1[j, k]
  }
}
# the trace of correlation matrix is always equals to the dimension of the random vector
for (i in 1:7) {
  cpct[i] <- sum(lambda1[1:i]) / 7
}
cpct

# iii)	Interpret the leading two principal components obtained in Part ii.

# Interpret the leading two PCAs
wmat[1:7, 1:2] ## First two PCAs

# The first two PCs explain almost 92% of the total variance. That means the two PCs can make
# a good measurement for comparing the performance between different countries. The correlation
# between the first PC and the standard variables are all negative, while the standardized variables
# are essentially normalized records by seconds, which means ...

### The Scree PLOT
plot(as.ts(eigen.vals), ylab = "lambda-hat", xlab = "PC", main = " The Scree Plot")

# The scree plot has an 'elbow' at PC2- indicating that eigenvalues after PC2 are relatively small
# and also somewhat similar magnitudes. We can therefore pick the first two PCs as describing most
# of the variability the data. This is also evident from the fact that PC1 and PC2 explain about
# 92% of the total variability.

## plot the PC's

plot(pca.scores2[, 1], pca.scores2[, 2], main = " PC1  Vs.  PC2")
round(cor(pca.scores2), 2)
ggpairs(as.data.frame(pca.scores2))

# Computing PC Scores via princomp ()
pca.dat <- princomp(women_dat[, -1], cor = FALSE, scores = TRUE)
attributes(pca.dat)

pca.scores2 <- pca.dat$scores
pc.scores3 <- pc.dat$loadings
dim(pca.scores2)
diag(var((pca.scores2)))

## Plotting PCAs using ggfortify (based on ggplot2)

wdf <- women_dat[, -1]
autoplot(prcomp(wdf)) ## plot via ggfortify
## We can identify the species as well
autoplot(prcomp(wdf), data = women_dat, colour = "Country")
## How about identifying the data points (row names)
autoplot(prcomp(wdf), data = women_dat, colour = "Country", label = TRUE, label.size = 3)
## We visualize the eigenvectors as well!
autoplot(prcomp(wdf), data = women_dat, colour = "Country", loadings = TRUE)
## Let's add variable labels
autoplot(prcomp(wdf),
  data = women_dat, colour = "Country",
  loadings = TRUE, loadings.colour = "blue",
  loadings.label = TRUE, loadings.label.size = 3
)
## How about the 'biplot' function?
biplot(prcomp(scale(wdf)))

# iv)	Rank the nations based on their score on the first principal component. Does this ranking
# correspond with your intuitive notion of athletic excellence for the various countries? List top
# ten ranked nations and the bottom five ranked nations.
pc <- prcomp(women_dat[, -1], scale = TRUE, center = TRUE)
pc
pc1 <- pc[[2]][1:7]
pc2 <- pc[[2]][8:14]
pc.frame <- as.data.frame(rbind(pc1, pc2), row.names = c("PC1", "PC2"))
names(pc.frame) <- names(women_dat[, -1])

rankings <- scale(as.matrix(women_dat[, -1]), center = pc$center, scale = pc$scale) %*% pc1
countries <- women_dat$Country
df.rank <- data.frame(country = countries, rank = rankings)
data.table(arrange(df.rank, desc(rank)))

###################
### End of File ###
###################
