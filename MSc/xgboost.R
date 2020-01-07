
if(!require(xgboost)) {
  install.packages("xgboost")
  library(xgboost)
}
library(magrittr)
library(tidyverse)
library(Matrix)
if(!require(Ckmeans.1d.dp)){
  install.packages("Ckmeans.1d.dp")
  library(Ckmeans.1d.dp)
}

xgb.doit <- function(data, formula, seed=1234, xgb_params=NULL){
  set.seed(seed)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(.7,.3))
  train <- data[ind == 1,]
  test <- data[ind == 2,]
  dependant_var <- all.vars(formula)[1]
  
  # One-hot encoding
  train.m <- sparse.model.matrix(formula, train)
  train_label <- as.integer(train[,dependant_var])-1
  train_matrix <- xgb.DMatrix(data = as.matrix(train.m), label = train_label)
  
  test.m <- sparse.model.matrix(formula, test)
  test_label <- as.integer(test[,dependant_var])-1
  test_matrix <- xgb.DMatrix(data = as.matrix(test.m), label = test_label)
  
  # Parameters
  nc <- length(unique(train_label))
  if(is.null(xgb_params)) {
    xgb_params <- list("objective" = "multi:softprob",
                       "eval_metric" = "mlogloss",
                       "num_class" = nc)
  }
  
  
  watchlist <- list(train = train_matrix, test = test_matrix)
  
  # eXtreme Gradient Boosting Model
  best_model <- xgb.train(params = xgb_params,
                          data = train_matrix,
                          nrounds = 100,
                          watchlist = watchlist)
  
  
  # Plotting the error
  e <- data.frame(best_model$evaluation_log)
  
  e.plot <- ggplot(melt(e, id.vars = "iter"), aes(x=iter)) + 
    geom_line(aes(y=value, color = variable)) +
    xlab("Iteration") + ylab("Error") + ggtitle("Error (train versus test)")
  
  # Importance
  #e[e$test_mlogloss == min(e$test_mlogloss),]
  imp <- xgb.importance(colnames(train_matrix), model = best_model)
  imp.plot <- xgb.ggplot.importance(imp)
  
  # Prediction and Confusion
  pred <- predict(best_model, newdata = test_matrix)
  pred <- matrix(pred, nrow = nc, ncol = length(pred)/nc) %>%
    t() %>% 
    data.frame() %>% 
    mutate(label = test_label, max_prob = max.col(., "last")-1)
  
  #table(Prediction = pred$max_prob, Actual = pred$label)
  confusion.plot <- ggplot(pred, aes(y=max_prob, x=label)) + 
    geom_bin2d(bins=nc) +
    stat_bin2d(bins=nc, geom = "text", aes(label=..count..)) + 
    xlab("Actual") + ylab("Predicted") + ggtitle("Confusion")
  
  result.plot <- grid.arrange(e.plot, imp.plot, confusion.plot, ncol=2, top=textGrob(dependant_var, gp=gpar(fontsize=20, font=3)))
  result.plot
  return(c(best_model, result.plot))
}

