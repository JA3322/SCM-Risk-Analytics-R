#Some packages require user confirmation to install
#Install all required packages
{
  time_1 <- Sys.time()
  installed_pakage_list <- installed.packages()
  packages_not_installed <- required_packages[!required_packages %in% installed_pakage_list]
  
  if(length(packages_not_installed) > 0){
    install.packages(packages_not_installed)
  }
  
  time_2 <- Sys.time()
  cat("\nPackage installation took ", difftime(time_2, time_1, units = "mins"), " mins\n")
  
  #Clean envirnoment
  rm(list = c("installed_pakage_list", "packages_not_installed"))
}

#To load all the required packages
{
  time_1 <- Sys.time()
  currently_loaded_packages <- (.packages())
  packages_not_loaded <- required_packages[!required_packages %in% currently_loaded_packages]
  package_load_result <- c()
  
  if(length(packages_not_loaded) > 0){
    #Handle if any package not loaded properly
    package_load_result <- lapply(packages_not_loaded, require, character.only = TRUE)
  }
  
  if(FALSE %in% package_load_result){
    cat("\nError: Some packages not loaded properly\n")
    print(packages_not_loaded[package_load_result == FALSE])
  }
  
  time_2 <- Sys.time()
  cat("\nPackage Loading took ", difftime(time_2, time_1, units = "mins"), " mins\n")
  
  #Clean envirnoment
  rm(list = c("currently_loaded_packages", "packages_not_loaded", "package_load_result", "required_packages"))
}