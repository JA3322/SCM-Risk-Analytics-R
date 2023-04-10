#Support Vector Machine
#Convert Data columns To Numeric
scm_data <- lapply(scm_data, convert_col_to_numeric)
scm_data <- data.frame(scm_data)

scm_train_data <- subset(scm_data, split == TRUE)
scm_test_data <- subset(scm_data, split == FALSE)

#Convert fs_scm_data columns to columns
fs_scm_data <- lapply(fs_scm_data, convert_col_to_numeric)
fs_scm_data <- data.frame(fs_scm_data)

scm_train_data <- subset(fs_scm_data, split == TRUE)
scm_test_data <- subset(fs_scm_data, split == FALSE)

#Convert fs_scm_data columns to columns
lr_sig_scm_data <- lapply(lr_sig_scm_data, convert_col_to_numeric)
lr_sig_scm_data <- data.frame(lr_sig_scm_data)

scm_train_data <- subset(lr_sig_scm_data, split == TRUE)
scm_test_data <- subset(lr_sig_scm_data, split == FALSE)


#1. The SVM Model took  73.55308  mins to build
#2. The SVM Model took  19.17164  mins to build (FS) - Retrain
#3. The SVM Model took  18.5549  mins to build (PCA)
#4. The SVM Model took  7.354097  mins to build (LRS) - Retrain
{
  time_1 <- Sys.time()
  svm_classifier = svm(formula = Late_delivery_risk~., data = scm_train_data, type = "C-classification", kernel = "radial")
  time_2 <- Sys.time()
  cat("\nThe SVM Model took ", difftime(time_2, time_1, units = "mins"), " mins to build\n")
}

#save(svm_classifier, file = paste("./Models/", stored_model_filenames$svm, ".RData", sep = ""))
#save(svm_classifier, file = paste("./Models/", stored_model_filenames$svm_fs, ".RData", sep = ""))
#save(svm_classifier, file = paste("./Models/", stored_model_filenames$svm_pca, ".RData", sep = ""))
#save(svm_classifier, file = paste("./Models/", stored_model_filenames$svm_lrs, ".RData", sep = ""))


load(paste("./Models/", stored_model_filenames$svm, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$svm_fs, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$svm_pca, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$svm_lrs, ".RData", sep = ""))



svm_prediction <- predict(svm_classifier, scm_test_data)
  
svm_results <- get_results_for_model(svm_prediction, scm_test_data$Late_delivery_risk)

svm_results_df <- data.frame(model_name = "Support Vector Machine", accuracy = svm_results[["Accuracy"]], sensitivity = svm_results[["Sensitivity"]], specificity = svm_results[["Specificity"]], recall = svm_results[["Recall"]], precision = svm_results[["Precision"]], f1_score = svm_results[["F1 Score"]], auc = svm_results[["Area Under Curve"]], mcc = svm_results[["MCC"]])

results <- rbind(results, svm_results_df)