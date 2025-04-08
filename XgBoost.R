#Install XGBoost package
install.packages(c("data.table", "xgboost"), type = "source")
install.packages(c("caret"), type = "source")

#load the library
library(xgboost)

#Load the data
data("Boston", package="MASS")

#Split the data into training and testing sets
set.seed(123)
train_indices<-sample(1:nrow(Boston), 0.8*nrow(Boston))
train_data<-Boston[train_indices,]
test_data<-Boston[-train_indices,]

#Separate features
train_matrix<-as.matrix(train_data[, -14])
train_label<- train_data$medv

test_matrix<-as.matrix(test_data[, -14])
test_label<-test_data$medv

# Create DMatrix objects for training and testing data
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Define parameters for the model
params <- list(
  booster = "gbtree",  #Which booster to use. Can be tree based or linear                 
  objective = "reg:squarederror",  # the learning task and the corresponding learning objective.      
  eval_metric = "rmse",       #Evaluation metrics for validation data, You can use multiple           
  eta = 0.1,      #Step size shrinkage used in update to prevent overfitting. Range 0-1                      
  max_depth = 6,  #Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit.                      
  subsample = 0.8,   #Subsample ratio of the training instances, occures once every iteration                  
  colsample_bytree = 0.8   #This is a family of parameters for subsampling of columns.Subsampling occurs once for every tree constructed.            
)

#see documentation here: https://xgboost.readthedocs.io/en/latest/parameter.html

# Define parameters for the model from Grid Search
params <- list(
  booster = "gbtree",                   
  objective = "reg:squarederror",        
  eval_metric = "rmse",                  
  eta = 0.1,                            
  max_depth = 3,                       
  subsample = 0.8,                     
  colsample_bytree = 1,
  gamma= 0.2, #Minimum loss reduction required to make a further partition on a leaf node of the tree. The larger gamma is, the more conservative the algorithm will be.
  min_child_weight = 1
)

# Train the model
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, watchlist = list(train = dtrain))

# Predict on test data
predictions <- predict(xgb_model, dtest)

# Print a few predictions
head(predictions)

# Evaluate model performance
mse <- mean((test_label - predictions)^2)
rmse <- sqrt(mse)

cat("Mean Squared Error:", mse, "\n")
cat("Root Mean Squared Error:", rmse, "\n")

# Feature importance
importance <- xgb.importance(model = xgb_model)

# Plot feature importance
xgb.plot.importance(importance)