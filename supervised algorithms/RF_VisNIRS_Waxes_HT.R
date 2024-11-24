# Load Required Libraries
library(doParallel)
library(readxl)
library(prospectr)
library(caret)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggiraphExtra)
library(ranger)

# Load Parallelization
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Load Data
pw_data <- read_excel("~/Documents/Doctorado/Tesis Doctoral/Investigación Cepsa/Vis-NIR/XDS-NIR_FOSS/Estudio según Tipo de Parafina e Hidrotratamiento/NIRS_HT_PW.xlsx",
                      sheet = "HT_Class")

# First derivative and Savitzky Golay Smoothing
pw_data$Sample <- as.factor(pw_data$Sample)

sgvec <- savitzkyGolay(X = pw_data[,-1], p = 3, w = 11, m = 1)
pw_sg <- cbind.data.frame(Sample = pw_data$Sample, sgvec)

# Data partition
set.seed(537)

intrain <- createDataPartition(y = pw_sg$Sample, 
                               p = 0.7, 
                               list = FALSE)
pw_train <- pw_sg[intrain,]
pw_test <- pw_sg[-intrain,]

# Ensure valid column names
colnames(pw_train) <- make.names(colnames(pw_train))
colnames(pw_test) <- make.names(colnames(pw_test))

# Calculate Class Weights
class_counts <- table(pw_train$Sample)
inverse_weights <- 1 / class_counts
normalized_weights <- inverse_weights / sum(inverse_weights)
weights_vector <- as.numeric(normalized_weights[pw_train$Sample])

# Define parameter grid for ranger
rf_grid <- expand.grid(
  mtry = c(sqrt(ncol(pw_train[,-1]))),             # Example values for mtry
  splitrule = c("gini"),            # Default splitrule for classification
  min.node.size = c(1, 5, 10)       # Example values for min.node.size
)

ntrees <- c(seq(2, 100, 2))         # Define a sequence for the number of trees
params <- expand.grid(ntrees = ntrees)

store_maxnode <- vector("list", nrow(params))

# Hyperparameter tuning with weights
set.seed(537)
trctrl <- trainControl(method = "cv", number = 10)

start_time_1 <- Sys.time()

for(i in 1:nrow(params)) {
  ntree <- params[i, 1]
  set.seed(537)
  rf_model <- train(
    Sample ~ ., 
    data = pw_train,
    method = "ranger",                 # Using ranger to allow case weights
    metric = "Accuracy",
    tuneGrid = rf_grid,                # Passing the required grid
    trControl = trctrl,
    num.trees = ntree,                 # Setting the number of trees dynamically
    importance = "permutation",
    case.weights = weights_vector      # Adding weights for class balancing
  )
  store_maxnode[[i]] <- rf_model
}

names(store_maxnode) <- paste("ntrees:", params$ntrees)

rf_results <- resamples(store_maxnode)
rf_results

lapply(store_maxnode, 
       function(x) x$results[x$results$Accuracy == max(x$results$Accuracy),])

total_time_1 <- Sys.time() - start_time_1
print(total_time_1)

# Accuracy vs ntrees
rf_plot <- cbind.data.frame(Accuracy = colMeans(rf_results$values[, c(seq(2, 100, 2))]),
                            Ntrees = c(seq(1, 100, 2)))
rf_plot <- as.vector(rf_plot)
accuracy <- (rf_plot$Accuracy) * 100
ntrees <- rf_plot$Ntrees

plot(x = ntrees,
     y = accuracy,
     type = "b", 
     pch = 19, 
     lty = 2,
     col = "#287D8EFF",
     xlab = "Number of decision trees",
     ylab = "Accuracy (%) (5-Fold CV)")

# Final RF model with ranger using weights
set.seed(537)

start_time_2 <- Sys.time()

best_rf_weighted <- ranger(
  formula = Sample ~ ., 
  data = pw_train, 
  num.trees = 100, 
  mtry = c(sqrt(ncol(pw_train[,-1]))),                           # Best mtry based on tuning results
  splitrule = "gini",
  min.node.size = 5,                  # Best min.node.size based on tuning results
  case.weights = weights_vector,
  importance = 'permutation',
  classification = TRUE
)

total_time_2 <- Sys.time() - start_time_2
print(total_time_2)

saveRDS(best_rf_weighted, "~/Documents/GitHub/machine-learning_VisNIR_ParaffinWax_HT/App/weighted_rf.rds")

# Train set performance for Weighted RF
pred_train_weighted <- predict(best_rf_weighted, pw_train)$predictions
cmatrix_train_weighted <- confusionMatrix(as.factor(pred_train_weighted), pw_train$Sample)
print(cmatrix_train_weighted)

# Test set performance for Weighted RF
pred_test_weighted <- predict(best_rf_weighted, pw_test)$predictions
cmatrix_test_weighted <- confusionMatrix(as.factor(pred_test_weighted), pw_test$Sample)
print(cmatrix_test_weighted)

# Variable Importance
var_imp <- importance(best_rf_weighted)

# Convert to DataFrame for visualization
var_imp_df <- data.frame(
  Variable = names(var_imp),
  Importance = var_imp
)

# Sort by descending importance
var_imp_df <- var_imp_df[order(var_imp_df$Importance, decreasing = TRUE), ]

print(var_imp_df)

# Remove "X" prefix from labels on the vertical axis
var_imp_df$Variable <- gsub("^X", "", var_imp_df$Variable)

# Create the plot with a gradient of colors
ggplot(var_imp_df[1:10, ], aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradientn(colors = c("#FDE725FF", "#287D8EFF", "#440154FF", "#73D055FF", "#404788FF")) +
  labs(title = "Variable Importance (Top 10)",
       x = "Variables",
       y = "Importance") +
  theme_light() +
  theme(legend.position = "right")

# Stop Parallelization
stopCluster(cl)