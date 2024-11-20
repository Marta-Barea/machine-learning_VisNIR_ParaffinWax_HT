# Load Required Libraries
library(doParallel)
library(readxl)
library(prospectr)
library(caret)
library(dplyr)
library(graphics)
library(ggplot2)

# Enable Parallelization
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Load Data
pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_HT_PW.xlsx",
                      sheet = "HT_Class")

# Apply First Derivative and Savitzky-Golay Smoothing
pw_data$Sample <- as.factor(pw_data$Sample)
sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

# Partition Data into Training and Testing Sets
set.seed(537)
intrain <- createDataPartition(y = pw_sg$Sample, p = 0.7, list = FALSE)
pw_train <- pw_sg[intrain,]
pw_test <- pw_sg[-intrain,]

# Define Class Weights for SVM
class_weights <- table(pw_train$Sample)
inverse_weights <- 1 / class_weights
normalized_weights <- inverse_weights / sum(inverse_weights)
weights_vector <- as.numeric(normalized_weights)

# Hyperparameter Tuning and Training
set.seed(537)
trctrl_1 <- trainControl(method = "cv", number = 10)
gridradial_1 <- expand.grid(sigma = c(2^(seq(-10, 10, 0.5))), 
                            C = c(2^(seq(-10, 10, 0.5))))

start_time_1 <- Sys.time()

svm_weighted <- train(
  Sample ~ ., 
  data = pw_train, 
  method = "svmRadial",
  trControl = trctrl_1,
  tuneGrid = gridradial_1,
  weights = weights_vector[pw_train$Sample],
  metric = "Accuracy"
)

total_time_1 <- Sys.time() - start_time_1
print(total_time_1)

# Best SVM Model
print(svm_weighted)
print(svm_weighted$finalModel)
filter(svm_weighted[['results']], 
       C == svm_weighted[["bestTune"]][["C"]], 
       sigma == svm_weighted[["bestTune"]][["sigma"]])

# Final SVM Model
set.seed(537)
gridradial_2 <- expand.grid(
  sigma = svm_weighted[["bestTune"]][["sigma"]], 
  C = svm_weighted[["bestTune"]][["C"]]
)
start_time_2 <- Sys.time()

best_svm <- train(
  Sample ~ .,                         
  data = pw_train,
  method = "svmRadial",
  trControl = trctrl_1,
  tuneGrid = gridradial_2,
  weights = weights_vector[pw_train$Sample],
  metric = "Accuracy"
)

total_time_2 <- Sys.time() - start_time_2
print(total_time_2)

# Predictions on Training Data
training_error <- predict(best_svm, newdata = pw_train[,-1], type = "raw") 
cmatrix_training <- confusionMatrix(training_error, as.factor(pw_train$Sample))
print(cmatrix_training)

# Predictions on Testing Data
pred_model <- predict(best_svm, newdata = pw_test[,-1])
cmatrix <- table(prediction = pred_model, reference = pw_test$Sample)
print(cmatrix)

prop_table <- prop.table(cmatrix)
print(prop_table)
rounded_table <- round(prop.table(cmatrix, 1) * 100, 2)
print(rounded_table)

cmatrix_test <- confusionMatrix(pred_model, as.factor(pw_test$Sample))
print(cmatrix_test)

# Contour Plot of the SVM Model
accuracy_gridsearch <- matrix((svm_weighted[["results"]][["Accuracy"]] * 100), ncol = 41, nrow = 41)
kappa_gridsearch <- matrix(svm_weighted[["results"]][["Kappa"]], ncol = 41, nrow = 41)

cost_expression <- expression(log[2] ~ C)
gamma_expression <- expression(log[2] ~ σ)

contour_accuracy <- filled.contour(
  x = c(seq(-10, 10, length.out = 41)),
  y = c(seq(-10, 10, length.out = 41)),
  z = as.matrix(accuracy_gridsearch),
  color.palette = colorRampPalette(c("#FDE725FF", "#287D8EFF", "#440154FF", "#73D055FF", "#404788FF")), 
  plot.title = title(
    main = "Accuracy", sub = "", 
    xlab = cost_expression, 
    ylab = gamma_expression,
  ),
  plot.axes = {
    axis(1, seq(-10, 10, 1), cex.axis = 1, las = 2)
    axis(2, seq(-10, 10, 1), cex.axis = 1, las = 2)
  }
)

contour_kappa <- filled.contour(
  x = c(seq(-10, 10, length.out = 41)),
  y = c(seq(-10, 10, length.out = 41)),
  z = as.matrix(kappa_gridsearch),
  color.palette = colorRampPalette(c("#FDE725FF", "#287D8EFF", "#440154FF", "#73D055FF", "#404788FF")), 
  plot.title = title(
    main = "Kappa", sub = "", 
    xlab = cost_expression, 
    ylab = gamma_expression,
  ),
  plot.axes = {
    axis(1, seq(-10, 10, 1), cex.axis = 1, las = 2)
    axis(2, seq(-10, 10, 1), cex.axis = 1, las = 2)
  }
)


# Stop Parallelization
stopCluster(cl)