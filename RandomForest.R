#Random Forest Model

#Remove the categorical columns that have more than 53 categories
rf_scm_data <- select_if(lr_sig_scm_data, is_low_factor_cols)

set.seed(2321)
split <- sample.split(rf_scm_data$Late_delivery_risk, SplitRatio = 0.80)
  
rf_scm_train_data <- subset(rf_scm_data, split == TRUE)
rf_scm_test_data <- subset(rf_scm_data, split == FALSE)

#1. Training Random Forest took 2.610676  mins
#2. Training Random Forest took  1.042995  mins (FS)
#3. Training Random Forest took  3.40434  mins (PCA)
#4. Training Random Forest took  1.211402  mins (LRS)
{
  time_1 <- Sys.time()
  randomforest_model <- randomForest(Late_delivery_risk ~., data = rf_scm_train_data, ntree = 350)
  time_2 <- Sys.time()
  cat("\nTraining Random Forest took ", difftime(time_2, time_1, units = "mins"), " mins\n")
}

#save(randomforest_model, file = paste("./Models/", stored_model_filenames$rf, ".RData", sep = ""))
#save(randomforest_model, file = paste("./Models/", stored_model_filenames$rf_fs, ".RData", sep = ""))
#save(randomforest_model, file = paste("./Models/", stored_model_filenames$rf_pca, ".RData", sep = ""))
#save(randomforest_model, file = paste("./Models/", stored_model_filenames$rf_lrs, ".RData", sep = ""))

load(paste("./Models/", stored_model_filenames$rf, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$rf_fs, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$rf_pca, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$rf_lrs, ".RData", sep = ""))


randomforest_predictions <- predict(randomforest_model, newdata = rf_scm_test_data)

rf_results <- get_results_for_model(as.factor(randomforest_predictions), rf_scm_test_data$Late_delivery_risk)

rf_results_df <- data.frame(model_name = "Support Vector Machine", accuracy = rf_results[["Accuracy"]], sensitivity = rf_results[["Sensitivity"]], specificity = rf_results[["Specificity"]], recall = rf_results[["Recall"]], precision = rf_results[["Precision"]], f1_score = rf_results[["F1 Score"]], auc = rf_results[["Area Under Curve"]], mcc = rf_results[["MCC"]])

results <- rbind(results, rf_results_df)  

rm(list = c("rf_results", "rf_results_df", "rf_scm_data", "rf_scm_train_data", "rf_scm_test_data", "randomforest_predictions"))
