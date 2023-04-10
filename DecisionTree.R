#Decision Tree

#1. Training Decision Tree model took  0.1028873  mins
#2. Training Decision Tree model took  0.03605917  mins (FS)
#3. Training Decision Tree model took  0.05639593  mins (PCA)
#4. Training Decision Tree model took  0.0249679  mins (LRS)
{
  time_1 <- Sys.time()
  scm_decision_tree <- rpart(formula = Late_delivery_risk~., data = scm_train_data)
  time_2 <- Sys.time()
  cat("\nTraining Decision Tree model took ", difftime(time_2, time_1, units = "mins"), " mins\n")
}
  
#save(scm_decision_tree, file = paste("./Models/", stored_model_filenames$dt, ".RData", sep = ""))
#save(scm_decision_tree, file = paste("./Models/", stored_model_filenames$dt_fs, ".RData", sep = ""))
#save(scm_decision_tree, file = paste("./Models/", stored_model_filenames$dt_pca, ".RData", sep = ""))
#save(scm_decision_tree, file = paste("./Models/", stored_model_filenames$dt_lrs, ".RData", sep = ""))



load(paste("./Models/", stored_model_filenames$dt, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$dt_fs, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$dt_pca, ".RData", sep = ""))
load(paste("./Models/", stored_model_filenames$dt_lrs, ".RData", sep = ""))


rpart.plot(scm_decision_tree)

decision_tree_prediction <- predict(scm_decision_tree, scm_test_data)

decision_tree_pred_output_map <- ifelse(decision_tree_prediction[,1] > 0.50, 0, 1)
  
dt_results <- get_results_for_model(as.factor(decision_tree_pred_output_map), scm_test_data$Late_delivery_risk)
  
dt_results$important_variables <- list()
  
for (name in names(scm_decision_tree$variable.importance)){
  dt_results$important_variables[name] <- scm_decision_tree$variable.importance[[name]]
}

dt_results_df <- data.frame(model_name = "Support Vector Machine", accuracy = dt_results[["Accuracy"]], sensitivity = dt_results[["Sensitivity"]], specificity = dt_results[["Specificity"]], recall = dt_results[["Recall"]], precision = dt_results[["Precision"]], f1_score = dt_results[["F1 Score"]], auc = dt_results[["Area Under Curve"]], mcc = dt_results[["MCC"]])

results <- rbind(results, dt_results_df)  

rm(list = c("scm_decision_tree", "dt_results", "decision_tree_prediction", "decision_tree_pred_output_map", "dt_results_df", "name"))
