class_check <- function(data, c_s) {
  if (is.numeric(data[,c_s])){
    result <- mean(data[,c_s])
  }
  else {
    result <- table(data[,c_s]) 
    }
  return(result)
}

  
 my_new_fun <- function(data, r_s, c_s, split_by=1, fun=class_check) {
     new_data <-  data[r_s, c_s]
     data_list <- split(new_data, split_by)
     calculated <- lapply(data_list, class_check)
     return(list(new_data, calculated))
}   
 my_new_fun(iris, 1:20, 1:5, split_by = iris$Species, fun=class_check)
 #the results that I get with the new function are weird, I think I made some mistake, but I don't know where
 