#test cross validation functions

library(dplyr)
data("auto")
m <- lm(MPG ~ Price, sample_n(auto, 50))
p <- auto %>% data.frame(predicted=predict(m, newdata=.), actual=.$MPG) %>% mutate(error=predicted-actual) %>% group_by(Model) %>% .

f <- MPG ~ Price
test[[f[[2]]]]
ksize <- nrow(auto)%/%5
error <- array(dim = c(1,5))
for (i in 1:5) {
  test <- filter(auto, row_number()%/%ksize == i)
  train <- filter(auto, row_number()%/%ksize != i)
  model <- lm(MPG ~Price, train)
  error[i] <- mean((predict(model, test) - test[[f[[2]]]])**2)
}
print(mean(error))



cv.kfold <- function(data, model.function, formula, test.function=predict, splits=5){
  ksize <- nrow(data)/splits
  mse <- array(dim = c(1,splits))
  for (i in 1:splits) {
    test <- filter(data, row_number()%/%ksize == i)
    train <- filter(data, row_number()%/%ksize != i)
    model <- model.function(formula, train)
    mse[i] <- mean((test.function(model, test) - test[[formula[[2]]]])**2)
  }
  return(mean(mse))
}

cv.kfold(auto, model.function=lm, formula = MPG ~ Price, test.function = predict, splits = 5)

cv.loo <- function(data, model.function, formula, test.function=predict) {
  return(cv.kfold(data, model.function, formula, test.function, splits = nrow(data)))
}

cv.loo(auto, model.function=lm, formula = MPG ~ Price, test.function = predict)
