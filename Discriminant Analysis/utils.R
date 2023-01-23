# Functions to load the data and to print digits

library(readr)


Load_data <- function(data_dir) {
  # Function to read the data
  raw_data <- data.matrix(read_csv(data_dir, col_names = FALSE))
  x_data <- raw_data[,-1]
  y_data <- raw_data[,1]
  return(list(x = x_data, y = y_data))
}





