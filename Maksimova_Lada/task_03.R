class_check <- function(data) {
  if (is.numeric(data)) {
    result <- mean(matrix(data))
  }
  else {
    result <- table(data) 
  }
  return(result)
}


my_new_fun <- function(data, r_s, c_s, split_by=1, fun=mean) {
  new_data <-  data[r_s, c_s]
  data_list <- split(new_data, split_by)
  calculated_values <- (lapply(data_list, class_check))
  print(calculated_values)
}


my_new_fun(iris, 1:20, 1:5, split_by = iris$Species, fun=class_check)

 #the results that I get with the new function are weird, I think I made some mistake, but I don't know where...
 
