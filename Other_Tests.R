#Test to check if Product.Card.Id is redundant with Product.Name
non_unique_list <- list()
for(carid in unique(scm_data$Product.Card.Id)) {
  if(length(unique(scm_data[scm_data$Product.Card.Id == carid,]$Product.Name)) > 1) {
    non_unique_list <- append(non_unique_list, carid)
  }
}

#Test to check if Category.Id is redundant with Product.Category.Id
non_unique_list <- list()
for(cid in unique(scm_data$Category.Id)) {
  if(length(unique(scm_data[scm_data$Category.Id == cid,]$Product.Category.Id)) > 1) {
    non_unique_list <- append(non_unique_list, cid)
  }
}

#Test to check if Order.Item.Cardprod.Id is redundant with Product.Card.Id
non_unique_list <- list()
for(cid in unique(scm_data$Order.Item.Cardprod.Id)) {
  if(length(unique(scm_data[scm_data$Order.Item.Cardprod.Id == cid,]$Product.Card.Id)) > 1) {
    non_unique_list <- append(non_unique_list, cid)
  }
}


