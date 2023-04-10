
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

source("Utils.R")
source("Package_Handling.R")

scm_data <- read.csv(file_paths$data, header = TRUE)

scm_data_desc <- read.csv(file_paths$data_desc, header = TRUE)

#Data Preprocessing takes - approx 1.4 mins
source("Data_Preprocessing.R")
write.csv(scm_data, file = "./Data/Data_processed.csv", row.names = F)

#To skip Data Preprocessing and read directly preprocessed data
scm_data <- read.csv("./Data/Data_processed.csv", header = TRUE)

#Make as factor Categorical columns for scm_data
{
  scm_data$Type <- as.factor(scm_data$Type)
  scm_data$Late_delivery_risk <- as.factor(scm_data$Late_delivery_risk)
  scm_data$Category.Id <- as.factor(scm_data$Category.Id)
  scm_data$Customer.Country <- as.factor(scm_data$Customer.Country)
  scm_data$Customer.Segment <- as.factor(scm_data$Customer.Segment)
  scm_data$Customer.State <- as.factor(scm_data$Customer.State)
  scm_data$Department.Id <- as.factor(scm_data$Department.Id)
  scm_data$Market <- as.factor(scm_data$Market)
  scm_data$Order.Country <- as.factor(scm_data$Order.Country)
  scm_data$Order.Region <- as.factor(scm_data$Order.Region)
  scm_data$Order.Status <- as.factor(scm_data$Order.Status)
  scm_data$Product.Card.Id <- as.factor(scm_data$Product.Card.Id)
  scm_data$Shipping.Mode <- as.factor(scm_data$Shipping.Mode)
}

#Significant Columns for Logistic Regression
significant_columns <- c("Type", "Days.for.shipping..real.", "Days.for.shipment..scheduled.", "Longitude", "Order.Status", "Shipping.Mode")
lr_sig_scm_data <- scm_data[, which(colnames(scm_data) %in% significant_columns)]
lr_sig_scm_data$Late_delivery_risk <- scm_data$Late_delivery_risk


#Dimensionality Reduction took  6.053014  mins
source("DimensionalityReduction.R")

write.csv(fs_scm_data, file = "./Data/FeatureSelection_SCM_Data.csv", row.names = F)
write.csv(pca_scm_data, file = "./Data/PCA_SCM_Data.csv", row.names = F)

fs_scm_data <- read.csv("./Data/FeatureSelection_SCM_Data.csv", header = TRUE)
pca_scm_data <- read.csv("./Data/PCA_SCM_Data.csv", header = TRUE)

#Make as factor Categorical columns for fs_scm_data
{
  fs_scm_data$Type <- as.factor(fs_scm_data$Type)
  fs_scm_data$Late_delivery_risk <- as.factor(fs_scm_data$Late_delivery_risk)
  fs_scm_data$Customer.Segment <- as.factor(fs_scm_data$Customer.Segment)
  fs_scm_data$Customer.State <- as.factor(fs_scm_data$Customer.State)
  fs_scm_data$Market <- as.factor(fs_scm_data$Market)
  fs_scm_data$Order.Region <- as.factor(fs_scm_data$Order.Region)
  fs_scm_data$Order.Status <- as.factor(fs_scm_data$Order.Status)
  fs_scm_data$Shipping.Mode <- as.factor(fs_scm_data$Shipping.Mode)
}

#Make Late_delivery_risk as factor for pca_scm_data
pca_scm_data$Late_delivery_risk <- as.factor(pca_scm_data$Late_delivery_risk)



set.seed(2321)
split <- sample.split(scm_data$Late_delivery_risk, SplitRatio = 0.80)

scm_train_data <- subset(scm_data, split == TRUE)
scm_test_data <- subset(scm_data, split == FALSE)

#To build the models
'
source("LogisticRegression.R")
source("SVM.R")
source("DecisionTree.R")
source("RandomForest.R")
source("NeuralNetwork.R")
'

