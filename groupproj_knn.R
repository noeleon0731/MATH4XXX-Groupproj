library(class)

getwd()
data <- read.csv("bctissue_data.csv")
data$id <- NULL #remove ID column to prevent training error
data$X <- NULL #remove the weird column

#split 80/20 for training. x = features, y = labels
set.seed(1)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))

#normalize dataset and exclude diagnosis
scaled.data <- scale(data[, -1], center = TRUE, scale = TRUE) # [, -1] ignores the diagnosis
diagnoses <- data[, 1]

train.x <- scaled.data[train_indices, ]
test.x <- scaled.data[-train_indices, ]

train.y <- diagnoses[train_indices]
test.y <- diagnoses[-train_indices]


#pick a starting K, run KNN, check accuracy. repeat.
#testing odd K values from 1 to 45.
k_values <- seq(1,45, by = 2)
error_rates <- c()

for (k in k_values) {
  knn.pred <- knn(train=train.x,
                  test=test.x,
                  cl=train.y,
                  k=k)
  error_rates <- c(error_rates, mean(knn.pred != test.y))
}

#determining K with lowest error
best_k <- k_values[which.min(error_rates)]
plot(k_values, error_rates, type = "b", main = "Error Rate vs K Value")


#Selected K=5 as my best K. 
best_pred <- knn(train=scaled.data,
                 test=scaled.data,
                 cl=diagnoses,
                 k=5)

#summary of model performance on full dataset
conf_matrix <- table(Predicted = best_pred, Actual = diagnoses)
print(conf_matrix)

  
#overall accuracy of model
sum(diag(conf_matrix)) / sum(conf_matrix)
