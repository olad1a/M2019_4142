library(tidyverse)

data_check <- function(data, r_s, c_s) {
  
  #Subsetting data
  new_data <- data %>%
    select(c_s) %>% 
    slice(r_s)
  
  #If numerical
  num_val <- new_data %>% 
    summarise_if(is.numeric, mean)
  
  #If non-numerical
  non_num <- new_data %>% 
    group_by_if(negate(is.numeric))%>%
    count()
  
  return(list(new_data, num_val, non_num))
}

data_check(iris, 46:64, 5)
