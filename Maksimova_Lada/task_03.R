class_check <- function(c_s) {
  if (is.numeric(c_s)) {
    result <- mean(as.matrix(c_s))
  }
  else {
    result <- table(c_s) 
  }
  return(result)
}


my_new_fun <- function(data, r_s, c_s) {
  new_data <-  data[r_s, c_s, drop=F]
  calculated_values <- lapply(new_data, class_check)
 calculated_values <- append(calculated_values, list(new_data = new_data), 0)
  return(calculated_values)
}

my_new_fun(iris, 1:10, 1:5)

my_new_fun(trees, 1:20, 1)

my_new_fun(mtcars, 1:10, 1:5)
 
