#Neural Network

scm_train_data <- lapply(scm_train_data, convert_col_to_numeric)
scm_test_data <- lapply(scm_test_data, convert_col_to_numeric)

scm_train_data <- data.frame(scm_train_data)
scm_test_data <- data.frame(scm_test_data)

"
R marks factor from 1, neural nets need factors (dependant variable) to start from 0
In Neural Net change Late_delivery_risk to 0,1
"

scm_train_data$Late_delivery_risk <- scm_train_data$Late_delivery_risk - 1
scm_test_data$Late_delivery_risk <- scm_test_data$Late_delivery_risk - 1

X_train <- scm_train_data %>% select(-Late_delivery_risk) %>% scale()
y_train <- to_categorical(scm_train_data$Late_delivery_risk)
X_test <- scm_test_data %>% select(-Late_delivery_risk) %>% scale()
y_test <- to_categorical(scm_test_data$Late_delivery_risk)


scm_neural_net_model <- keras_model_sequential()

scm_neural_net_model %>% layer_dense(units = 256, activation = 'relu', input_shape =  ncol(X_train)) %>% layer_dropout(rate = 0.4) %>% layer_dense(units = 75, activation = 'relu') %>% layer_dropout(rate = 0.3) %>% layer_dense(units = 2, activation = 'sigmoid')

history <- scm_neural_net_model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))

#Change the wieghts to correct directory
#model_weight_filename <- paste("./Models/NeuralNet_Weights/", stored_model_filenames$nn_weights, ".ckpt", sep = "")
#model_weight_filename <- paste("./Models/NeuralNet_FS_Weights/", stored_model_filenames$nn_weights_fs, ".ckpt", sep = "")
#model_weight_filename <- paste("./Models/NeuralNet_PCA_Weights/", stored_model_filenames$nn_weights_pca, ".ckpt", sep = "")
#model_weight_filename <- paste("./Models/NeuralNet_LRS_Weights/", stored_model_filenames$nn_weights_lrs, ".ckpt", sep = "")



checkpoint_callback <- callback_model_checkpoint(filepath = model_weight_filename, save_weights_only = T, verbose = 1)

#1. Training the Neural Network took 31 mins(for 20 epochs)
#2. Training the Neural Network took  7.783294  mins (FS) (for 10 epochs)
#3. Training the Neural Network took  16.41918  mins (PCA) (for 10 epochs)
#4. Training the Neural Network took  16  mins (LRS) (for 10 epochs)
{
  time_1 <- Sys.time()
  scm_neural_net_model %>% fit(X_train, y_train, epochs = 10, batch_size = 5, validation_split = 0.3, callbacks = list(checkpoint_callback))
  time_2 <- Sys.time()
  cat("\nTraining the Neural Network took ", difftime(time_2, time_1, units = "mins"), " mins\n")
}


#model_weight_filename <- paste("./Models/NeuralNet_Weights/", stored_model_filenames$nn_weights, ".ckpt", sep = "")
#model_weight_filename <- paste("./Models/NeuralNet_FS_Weights/", stored_model_filenames$nn_weights_fs, ".ckpt", sep = "")
#model_weight_filename <- paste("./Models/NeuralNet_PCA_Weights/", stored_model_filenames$nn_weights_pca, ".ckpt", sep = "")
model_weight_filename <- paste("./Models/NeuralNet_LRS_Weights/", stored_model_filenames$nn_weights_lrs, ".ckpt", sep = "")


checkpoint_dir = fs::path_dir(model_weight_filename)

latest_model_weights <- tf$train$latest_checkpoint(checkpoint_dir)

scm_neural_net_model <- keras_model_sequential()

scm_neural_net_model %>% layer_dense(units = 256, activation = 'relu', input_shape =  ncol(X_train)) %>% layer_dropout(rate = 0.4) %>% layer_dense(units = 75, activation = 'relu') %>% layer_dropout(rate = 0.3) %>% layer_dense(units = 2, activation = 'sigmoid')

history <- scm_neural_net_model %>% compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))

load_model_weights_tf(scm_neural_net_model, latest_model_weights)



neural_net_predictions <- scm_neural_net_model %>% predict(X_test) %>% k_argmax()

get_results_for_model(factor(neural_net_predictions, levels=min(scm_test_data$Late_delivery_risk):max(scm_test_data$Late_delivery_risk)),factor(scm_test_data$Late_delivery_risk, levels=min(scm_test_data$Late_delivery_risk):max(scm_test_data$Late_delivery_risk)))



rm(list = c("scm_data_nn", "scm_data_nn.training", "scm_data_nn.test", "X_test", "X_train", "y_test", "y_train", "neural_net_predictions"))