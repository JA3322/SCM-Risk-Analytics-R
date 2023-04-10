
time_1 <- Sys.time()

#Plot Missing Data
{
  plot_command <- "plot_missing(scm_data)"
  save_image(plot_command = plot_command, image_name = image_filenames$plot_missing, path = images_directory$root, width = 500, height = 1000)
}

#NA Column, Single Factor Column removal
{
  #NA values removal
  #Remove Order.Zipcode an NA Column
  #Remove Product.Description an NA Column
  remove_cols <- c("Order.Zipcode", "Product.Description")
  scm_data <- scm_data[, -which(colnames(scm_data) %in% remove_cols)]
  scm_data_desc <- scm_data_desc[!(scm_data_desc$FIELDS %in% remove_cols),]
  
  
  #Remove single factor columns
  temp <- dplyr::mutate_all(scm_data, as.factor) #Find columns that act as single factor columns
  remove_cols <- which(lapply(temp, nlevels) == 1)
  scm_data <- scm_data[,-remove_cols]
  scm_data_desc <- scm_data_desc[!(scm_data_desc$FIELDS %in% names(remove_cols)),]
  
  
  #Remove Redundant and unnecessary columns
  
  #Product.Image contains url info,
  #Category.Name - redundant with Category ID,
  #Customer.Fname, Customer.Id, Customer.Lname - factor type with too many levels,
  #Department.Name - redundant with Department ID,
  #Order.Customer.Id, Order.Id, Order.Item.Id - factor type with too many levels,
  #Delivery.Status has high correlation with Late_delivery_risk, distorts the model as we want to find the variables that influence Late_delivery_risk
  #Customer.Street (7458 factors), Customer.Zipcode (995 factors), Order.City (3597 factors), Order.State (1089 factors) are factors with high number of levels,
  #Product.Name is redundant with Prodcut.Card.Id,
  #Product.Category.Id is redundant with Category.Id,
  #Order.Item.Cardprod.Id is redundant with Prodcut.Card.Id
  
  
  remove_cols <- c("Product.Image", "Category.Name", "Customer.Fname", "Customer.Id", "Customer.Lname", "Department.Name", "Order.Customer.Id", "Order.Id", "Order.Item.Id", "Delivery.Status", "Order.City", "Customer.Street", "Customer.Zipcode", "Order.City", "Order.Item.Cardprod.Id", "Order.State","Product.Category.Id", "Product.Name")
  scm_data <- scm_data[, -which(colnames(scm_data) %in% remove_cols)] #Product Image is not necessary
  scm_data_desc <- scm_data_desc[!(scm_data_desc$FIELDS %in% remove_cols),]
  
  rm(list = c("temp", "remove_cols"))
}

#Handle Datetype variables
{
  #Convert the date variables to Datetype
  scm_data$order.date..DateOrders. <-  strptime(scm_data$order.date..DateOrders., format = "%m/%d/%Y %H:%M")
  scm_data$shipping.date..DateOrders. <-  strptime(scm_data$shipping.date..DateOrders., format = "%m/%d/%Y %H:%M")
  
  #Date variables converted to UNIX time
  scm_data$order.date..DateOrders. <- as.numeric(scm_data$order.date..DateOrders.)
  scm_data$shipping.date..DateOrders. <- as.numeric(scm_data$shipping.date..DateOrders.)
}

#Treat NA Values
{
  #Zipcode has missing values - Replace with mode
  customer_zipcode_mode <- which.max(table(scm_data$Customer.Zipcode))
  scm_data[is.na(scm_data$Customer.Zipcode),]$Customer.Zipcode <- names(customer_zipcode_mode)
  rm("customer_zipcode_mode")
}

#Make as factor Categorical columns
{
  scm_data$Type <- as.factor(scm_data$Type)
  #scm_data$Delivery.Status <- as.factor(scm_data$Delivery.Status)
  scm_data$Late_delivery_risk <- as.factor(scm_data$Late_delivery_risk)
  scm_data$Category.Id <- as.factor(scm_data$Category.Id)
  scm_data$Customer.City <- as.factor(scm_data$Customer.City)
  scm_data$Customer.Country <- as.factor(scm_data$Customer.Country)
  scm_data$Customer.Segment <- as.factor(scm_data$Customer.Segment)
  scm_data$Customer.State <- as.factor(scm_data$Customer.State)
  #scm_data$Customer.Street <- as.factor(scm_data$Customer.Street)
  #scm_data$Customer.Zipcode <- as.factor(scm_data$Customer.Zipcode)
  scm_data$Department.Id <- as.factor(scm_data$Department.Id)
  scm_data$Market <- as.factor(scm_data$Market)
  #scm_data$Order.City <- as.factor(scm_data$Order.City)
  scm_data$Order.Country <- as.factor(scm_data$Order.Country)
  #scm_data$Order.Item.Cardprod.Id <- as.factor(scm_data$Order.Item.Cardprod.Id)
  scm_data$Order.Region <- as.factor(scm_data$Order.Region)
  #scm_data$Order.State <- as.factor(scm_data$Order.State)
  scm_data$Order.Status <- as.factor(scm_data$Order.Status)
  scm_data$Product.Card.Id <- as.factor(scm_data$Product.Card.Id)
  #scm_data$Product.Category.Id <- as.factor(scm_data$Product.Category.Id)
  #scm_data$Product.Name <- as.factor(scm_data$Product.Name)
  scm_data$Shipping.Mode <- as.factor(scm_data$Shipping.Mode)
}

#Correlation Plot
{
  scm_numeric_columns <- select_if(scm_data, is.numeric)
  correlation_result <- cor(scm_numeric_columns, method="pearson")
  plot_command <- "corrplot::corrplot(correlation_result, method= \"number\", order = \"hclust\")"
  save_image(plot_command = plot_command, image_name = image_filenames$correlation_plot, path = images_directory$root, width = 800, height = 800)
  rm(list = c("correlation_result"))
}

#To get Pearson correlation coefficient of the variables in the dataset
{
  scm_num_colums <- ncol(scm_numeric_columns)
  pearson_correlation_table <- data.frame()
  strong_correlation_coeffs <- data.frame()
  names_scm_numeric_columns <- colnames(scm_numeric_columns)
  for(col_index in 1:scm_num_colums){
    if(col_index == scm_num_colums){
      pearson_correlation_table[col_index, col_index] <- 1
      break
    }
    curr_col_corr <- c()
    pearson_correlation_table[col_index, col_index] <- 1
    for(col_index_2 in (col_index+1):scm_num_colums){
      corr_coeff <- cor(scm_numeric_columns[,col_index], scm_numeric_columns[,col_index_2], method = "pearson")
      if(abs(corr_coeff) > 0.7) {
        strong_correlation_coeffs <- rbind(strong_correlation_coeffs,c(names_scm_numeric_columns[col_index],names_scm_numeric_columns[col_index_2],corr_coeff))
      }
      curr_col_corr <- append(curr_col_corr, corr_coeff)
      pearson_correlation_table[col_index, col_index_2] <- corr_coeff
      pearson_correlation_table[col_index_2, col_index] <- corr_coeff
    }
  }
  colnames(pearson_correlation_table) <- colnames(scm_numeric_columns)
  rownames(pearson_correlation_table) <- colnames(scm_numeric_columns)
  colnames(strong_correlation_coeffs) <- c("Variable 1", "Variable 2", "Pearson Correlation")
  rm(list = c("col_index", "col_index_2", "scm_num_colums", "names_scm_numeric_columns", "curr_col_corr", "corr_coeff"))
}

#To export Pearson Correlation table
{
  
  p_table <-tableGrob(pearson_correlation_table)
  plot_command <- "grid.arrange(p_table)"
  save_image(plot_command = plot_command, image_name = image_filenames$pearson_cor, path = images_directory$root, width = 2820, height = 400)
  
  p_table <-tableGrob(strong_correlation_coeffs)
  plot_command <- "grid.arrange(p_table)"
  save_image(plot_command = plot_command, image_name = image_filenames$strong_pearson, path = images_directory$root, width = 520, height = 400)
  
  write.csv(pearson_correlation_table, file = "./Data/PearsonCorrelation.csv")
  rm(list = c("p_table", "pearson_correlation_table", "strong_correlation_coeffs", "scm_numeric_columns"))
}

#Remove Highly correlated numeric columns
{
  remove_columns <- c("Order.Item.Product.Price", "shipping.date..DateOrders.", "Order.Item.Total", "Sales" ,"Order.Item.Profit.Ratio", "Sales.per.customer")
  scm_data <- scm_data[, -which(colnames(scm_data) %in% remove_columns)]
  scm_data_desc <- scm_data_desc[!(scm_data_desc$FIELDS %in% remove_columns),]
  rm("remove_columns")
}

#Categorical Correlation - Pearson chi square
{
  scm_categorical_columns <- select_if(scm_data, is.factor)
  scm_categorical_columns <- scm_categorical_columns[,!(colnames(scm_categorical_columns) == "Late_delivery_risk")]
  names_scm_categorical <- colnames(scm_categorical_columns)
  scm_cat_colums <- ncol(scm_categorical_columns)
  strong_correlation_result <- data.frame()
  for(col_index in 1:(scm_cat_colums-1)){
    if(col_index == scm_cat_colums){
      break
    }
    for(col_index_2 in (col_index+1):scm_cat_colums){
      chi_sq_result <- chisq.test(scm_categorical_columns[,col_index], scm_categorical_columns[,col_index_2], correct = FALSE)
      if(chi_sq_result$p.value < 0.05) {
        strong_correlation_result <- rbind(strong_correlation_result, c(names_scm_categorical[col_index], names_scm_categorical[col_index_2]))
      }
    }
  }
  colnames(strong_correlation_result) <- c("Variable 1", "Variable 2")
  rm(list = c("col_index", "col_index_2", "chi_sq_result"))
}

#Categorical Correlation - Pearson chi square (with Dependant Variable)
{
  strong_correlation_result_dependant <- data.frame()
  for(col_index in 1:(scm_cat_colums)){
    if(col_index == scm_cat_colums){
      break
    }
    chi_sq_result <- chisq.test(scm_data$Late_delivery_risk, scm_categorical_columns[,col_index], correct = FALSE)
    if(chi_sq_result$p.value < 0.05) {
      strong_correlation_result_dependant <- rbind(strong_correlation_result_dependant, c("Late_delivery_risk", names_scm_categorical[col_index]))
    }
  }
  colnames(strong_correlation_result_dependant) <- c("Variable 1", "Variable 2")
  rm(list = c("col_index", "scm_categorical_columns", "names_scm_categorical", "chi_sq_result", "scm_cat_colums"))
}

#To export Chi-Squared Correlation table
{
  p_table <- tableGrob(strong_correlation_result_dependant)
  plot_command <- "grid.arrange(p_table)"
  save_image(plot_command = plot_command, image_name = image_filenames$cat_chisq, path = images_directory$root, width = 258, height = 250)
  write.csv(strong_correlation_result, file = "./Data/CategoricalCorrelation.csv")
  write.csv(strong_correlation_result_dependant, file = "./Data/CategoricalCorrelation_with_Dependant.csv")
  rm(list = c("p_table", "plot_command", "strong_correlation_result", "strong_correlation_result_dependant"))
}


#Analyze and Save Histogram of all continuous columns
{
  scm_numeric_columns <- select_if(scm_data, is.numeric)
  for(index in 1:ncol(scm_numeric_columns)) {
    data <- scm_numeric_columns[,index]
    column_name <- colnames(scm_numeric_columns)[index]
    scm_description_record <- scm_data_desc[scm_data_desc$FIELDS == column_name,]
    x_label <- scm_description_record$DESCRIPTION
    imgname <- paste("Histogram of", scm_description_record$Name, sep =" ")
    plotcommand <- "hist(data, main=image_name, xlab=x_label)"
    save_image(plot_command = plotcommand, image_name = imgname, path = images_directory$histogram, width = 500, height = 500)
  }
}

#Analyze and Save Boxplots of all continuous columns
{
  for(index in 1:ncol(scm_numeric_columns)){
    data <- scm_numeric_columns[,index]
    column_name <- colnames(scm_numeric_columns)[index]
    scm_description_record <- scm_data_desc[scm_data_desc$FIELDS == column_name,]
    x_label <- scm_description_record$DESCRIPTION
    imgname <- paste("Boxplot of", scm_description_record$Name, sep = " ")
    plotcommand <- "boxplot(data, main = image_name, xlab = x_label, horizontal = TRUE)"
    save_image(plot_command = plotcommand, image_name = imgname, path = images_directory$boxplot, width = 500, height = 500)
  }
}

#Analyze and Save Barplots of all categorical columns
{
  scm_categorical_columns <- select_if(scm_data, is.factor)
  for(index in 1:ncol(scm_categorical_columns)){
    data <- scm_categorical_columns[,index]
    column_name <- colnames(scm_categorical_columns)[index]
    scm_description_record <- scm_data_desc[scm_data_desc$FIELDS == column_name,]
    x_label <- scm_description_record$DESCRIPTION
    imgname <- paste("Barplot of", scm_description_record$Name, sep = " ")
    plotcommand <- "barplot(table(data), col = rainbow(nlevels(data)),  main = image_name, xlab = x_label)"
    save_image(plot_command = plotcommand, image_name = imgname, path = images_directory$barplot, width = 700, height = 700)
  }
  rm(list = c("scm_numeric_columns", "scm_categorical_columns", "scm_description_record", "column_name", "data", "imgname", "index", "plotcommand", "x_label"))
}

#Outliers Treatment
{
  #Do not apply outlier treatment to Latitude as it does not make sense
  outlier_columns <- c("Benefit.per.order", "Order.Item.Discount", "Order.Profit.Per.Order", "Product.Price")
  outlier_q_level <- c(2, 1, 2, 1)
  outlier_column_index <- which(colnames(scm_data) %in% outlier_columns)
  iter = 1
  for(index in outlier_column_index) {
    data <- scm_data[, index]
    if(outlier_q_level[iter] == 1) {
      data <- winsorization(data = data, p1 = 0.05, p2 = 0.95)
    } else if(outlier_q_level[iter] == 2) {
      data <- winsorization(data = data, p1 = 0.10, p2 = 0.90)
    }
    column_name <- colnames(scm_data)[index]
    scm_description_record <- scm_data_desc[scm_data_desc$FIELDS == column_name,]
    x_label <- scm_description_record$DESCRIPTION
    imgname <- scm_description_record$Name
    plotcommand <- "boxplot(data, main = image_name, xlab = x_label, horizontal = TRUE)"
    save_image(plot_command = plotcommand, image_name = imgname, path = images_directory$outlier_barplots, width = 500, height = 500)
    iter <- iter + 1
    
    #Save the Winsorization applied data to original dataset
    scm_data[,outlier_column_index] <- data
  }
  rm(list = c("column_name", "index", "data", "imgname", "outlier_q_level", "outlier_column_index", "outlier_columns", "plotcommand", "x_label", "iter"))
}

time_2 <- Sys.time()
cat("\nData Preprocessing took ", difftime(time_2, time_1, units = "mins"), " mins\n")