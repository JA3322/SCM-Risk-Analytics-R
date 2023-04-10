time_1 <- Sys.time()

#Feature Selection with Random Forest

#Train Random Forest
scm_data_dm <- select_if(scm_data, is_low_factor_cols)

#Random Forest Model for DM took  3.404233  mins to build
{
  time_3 <- Sys.time()
  randomForest_model_for_dm <-randomForest(Late_delivery_risk~.,data=scm_data_dm, importance=TRUE,ntree=300)
  time_4 <- Sys.time()
  cat("\nRandom Forest Model for DM took ", difftime(time_4, time_3, units = "mins"), " mins to build\n")
}
save(randomForest_model_for_dm, file = paste("./Models/", stored_model_filenames$rf_model_dm, ".RData", sep = ""))

load(paste("./Models/", stored_model_filenames$rf_model_dm, ".RData", sep = ""))

#Evaluate variable importance
importance_of_features = importance(randomForest_model_for_dm, type=1)
importance_of_features <- data.frame(predictors=rownames(importance_of_features),importance_of_features)

# Order the predictor levels by importance
importance_of_features.sort <- arrange(importance_of_features,desc(MeanDecreaseAccuracy))
importance_of_features.sort$predictors <- factor(importance_of_features.sort$predictors,levels=importance_of_features.sort$predictors)
  
# Select the top 20 predictors
importance_of_features.15 <- importance_of_features.sort[1:15,]
  
# Plot Important Variables
imgname <- "Importance of variables from Random Forest Feature Engineering"
plotcommand <- "varImpPlot(randomForest_model_for_dm, type=1, main = \"Random Forest Important Predictors\")"
save_image(plot_command = plotcommand, image_name = imgname, path = images_directory$root, width = 500, height = 500)
  
# Subset data with 20 independent and 1 dependent variables
fs_scm_data = cbind(Late_delivery_risk = scm_data_dm$Late_delivery_risk, scm_data[,which(colnames(scm_data) %in% importance_of_features.15$predictors)])
  
rm(list = c("importance_of_features", "imgname", "importance_of_features.15", "importance_of_features.sort", "randomForest_model_for_dm", "scm_data_dm"))


#PCA

scm_data_converted <- scm_data
scm_data_converted <- lapply(scm_data_converted, convert_col_to_numeric)
scm_data_converted <- data.frame(scm_data_converted)
  
pca_scm <- prcomp(scm_data_converted[, -which(colnames(scm_data_converted) %in% c("Late_delivery_risk"))], scale = TRUE)
  
#The First 15 Principle Components capture 95% variation in the data
pca_scm_data <- pca_scm$x[,1:15]
  
pca_scm_data <- data.frame(pca_scm_data)
pca_scm_data$Late_delivery_risk <- scm_data$Late_delivery_risk
  
plotcommand ="plot(summary(pca_scm)$importance[3,], xlab = \"No. of Principle Component\", ylab = \"Proportion of variance captured\")"
save_image(plot_command = plotcommand, image_name = image_filenames$pca_analysis, path = images_directory$root, width = 800, heigh = 400)
  
rm(list = c("scm_data_converted", "pca_scm", "plotcommand"))

time_2 <- Sys.time()
cat("\nDimensionality Reduction took ", difftime(time_2, time_1, units = "mins"), " mins\n")