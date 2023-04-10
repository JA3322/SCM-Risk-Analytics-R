#Logistic regression

#1. Logistic Regression - 16 mins - Retrain this model (Approach use entire dataset, mark categorical columns)
#2. Training Logistic regression model took  1.45688  mins - (FS)
#3. Training Logistic regression model took  0.01872322  mins - (PCA)
#4. Training Logistic regression model took  0.1681477  mins - (LRS)
{
  time_3 <- Sys.time()
  scm_log_reg <- glm(Late_delivery_risk~., data=scm_train_data, family = binomial)
  time_4 <- Sys.time()
  cat("\nTraining Logistic regression model took ", difftime(time_4, time_3, units = "mins"), " mins\n")
}

#save(scm_log_reg, file = paste("./Models/", stored_model_filenames$lr, ".RData", sep = ""))
#save(scm_log_reg, file = paste("./Models/LR_Model_Significant_Variables.RData", sep = ""))
#save(scm_log_reg, file = paste("./Models/", stored_model_filenames$lr_fs, ".RData", sep = ""))
#save(scm_log_reg, file = paste("./Models/", stored_model_filenames$lr_pca, ".RData", sep = ""))
#save(scm_log_reg, file = paste("./Models/", stored_model_filenames$lr_lrs, ".RData", sep = ""))


load(paste("./Models/", stored_model_filenames$lr, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$lr_fs, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$lr_pca, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$lr_lrs, ".RData", sep = ""))

lm_scm_log_reg_summary <- summary(scm_log_reg)

{
  is.prone <- function(x) is.factor(x) | is.character(x)
  id <- sapply(scm_test_data, is.prone)
  unique_from_test <- lapply(scm_test_data[id], unique)
}

scm_log_reg$xlevels$Order.Country <- union(scm_log_reg$xlevels$Order.Country, unique_from_test$Order.Country)

lm_scm_prediction <- predict(scm_log_reg, newdata = scm_test_data, type='response')
  
scm_test_data[lm_scm_prediction>0.5,"LogisticRegression_Prediction"] <- 1
scm_test_data[lm_scm_prediction<0.5,"LogisticRegression_Prediction"] <- 0

scm_test_data$LogisticRegression_Prediction <- as.factor(scm_test_data$LogisticRegression_Prediction)
  
lr_results <- get_results_for_model(scm_test_data$LogisticRegression_Prediction, scm_test_data$Late_delivery_risk)
  
lr_results$important_variables <- as.list(names(summary(scm_log_reg)$coefficients[,4][summary(scm_log_reg)$coefficients[,4] <= 0.05]))
  

rm(list = c("scm_log_reg", "lm_scm_log_reg_summary", "lm_scm_prediction", "LogisticRegression_CM", "LogisticRegression_ROC", "logistic_regression_results", "lr_results"))
