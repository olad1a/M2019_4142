#my function

basic_calculations <- function (data, r_s, c_s) {
  subset <- data[r_s, c_s]
  my_list <- list(subset)
  for (i in 1:length(subset)){
    if (is.numeric(subset[ , i])){
      result1 <- mean(subset[ , i])
      my_list <- append(my_list, result1) 
    } else {
      result2 <- table(subset[ , i])
      my_list <- append(my_list, result2)
    }}
  print(my_list)} 

#examples 

#1
basic_calculations(iris, 1:15, 1:5)

#2
basic_calculations(iris, c(1, 35, 67, 106, 145), c(1, 2, 5))

#tree
basic_calculations(trees, c(1:5, 8, 21), 1:3)
