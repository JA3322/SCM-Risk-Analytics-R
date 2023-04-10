required_packages <- c("tidyverse", "DataExplorer", "caret", "caTools", "e1071", "DAAG", "party", "rpart", "rpart.plot", "mlbench", "pROC", "tree", "keras", "fastDummies", "tensorflow", "ggplot2", "dplyr", "gridExtra", "umap", "randomForest", "mltools")

file_paths <- list(data = "./Data/DataCoSupplyChainDataset.csv", data_desc = "./Data/DescriptionDataCoSupplyChain_Updated.csv")

images_directory <-list(root = "Images/", histogram = "Images/Histograms/", boxplot = "Images/Boxplots/", barplot = "Images/Barplots/", outlier_barplots = "Images/Boxplots/OutlierTreatment/", relation = "Images/Relation_Study/")

image_filenames <- list(plot_missing = "Missing_Data_Plot", pca_analysis = "PCA_Analysis", final_result = "Result", correlation_plot = "Correlation_Plot", pearson_cor = "Pearson_Correlation", strong_pearson = "Strong_Pearson_Correlation", cat_chisq = "Categorical_Strong_Correlation_with_Dependant")

stored_model_filenames <- list(
  rf_model_dm = "RF_Model_for_DM", 
  lr ="Logistic_Regression_Model", lr_fs = "LogisticRegression_FS_Model", lr_pca ="Logistic_Regression_PCA_Model", lr_lrs ="Logistic_Regression_LRS_Model",
  svm = "SupportVectorMachine_Model", svm_fs = "SupportVectorMachine_FS_Model", svm_pca = "SupportVectorMachine_PCA_Model", svm_lrs = "SupportVectorMachine_LRS_Model",
  dt = "Decision_Tree_Model", dt_fs = "Decision_Tree_FS_Model", dt_pca = "Decision_Tree_PCA_Model", dt_lrs = "Decision_Tree_LRS_Model",
  rf = "Random_Forest_Model", rf_fs = "Random_Forest_FS_Model", rf_pca = "Random_Forest_PCA_Model", rf_lrs = "Random_Forest_LRS_Model",
  nn_weights = "NeuralNetwork_Model_Weights", nn_weights_fs = "NeuralNetwork__FS_Model_Weights", nn_weights_pca = "NeuralNetwork_Model_PCA_Weights", nn_weights_lrs = "NeuralNetwork_LRS_Model_Weights"
  )

results <- data.frame(model_name = c(), accuracy = c(), sensitivity = c(), specificity = c(), recall = c(), precision = c(), f1_score = c(), auc = c(), mcc = c())

save_image <- function(plot_command, image_name, path, width, height) {
  if(missing(width)) {
    width = 400
  }
  if(missing(height)){
    height = 400
  }
  
  png(paste(path,image_name,".png", sep = ""), height = height, width = width)
  eval(parse(text = plot_command))
  dev.off()
}

winsorization <- function(data, p1, p2) {
  x <- data
  qnt <- quantile(x, probs = c(.25, .75), na.rm = F)
  qnt
  caps <- quantile(x, probs = c(p1, p2), na.rm = F)
  caps
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  data <- x
  return(data)
}

#temp <- lapply(temp, convert_col_to_numeric)
convert_col_to_numeric <- function(col) {
  if(is.factor(col)) {
    col <- as.numeric(col)
  }
  return(col)
}

convert_char_col_to_numeric <- function(col) {
  if(is.character(col)) {
    col <- as.numeric(col)
  }
  return(col)
}

is_low_factor_cols <- function(col) {
  if(is.factor(col)) {
    if(nlevels(col) <= 53) {
      return(T)
    } else {
      return(F)
    }
  } else {
    return(T)
  }
}

get_results_for_model <- function(predictions, actual_value) {
  
  confusionMatrix <- confusionMatrix(table(predictions, actual_value))
  
  modelRoc <- roc(as.numeric(predictions), as.numeric(actual_value))
  
  results <- confusionMatrix$overall["Accuracy"]
  
  results <- append(results, confusionMatrix$byClass[c("Sensitivity", "Specificity", "Recall", "Precision")])
  
  #F1 score
  #Formula;- F1 Score = 2 * (Precision * Recall) / (Precision + Recall)
  
  results[["F1 Score"]] <- 2*(results[["Precision"]] * results[["Recall"]])/(results[["Precision"]] + results[["Recall"]])
  
  results[["Area Under Curve"]] <- modelRoc$auc
  
  results[["MCC"]] <- mcc(preds = predictions, actuals = actual_value)
  
  return(results)
}

