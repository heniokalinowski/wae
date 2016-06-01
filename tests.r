#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")
source("knn.r")

startingVector <- c(1, 2, 3, 4)
normalized <- normalize(startingVector)
denormalized <- denormalize(normalized)

if(!all(startingVector == denormalized)) 
{
  warning("normalization test failed")
} 


tested_fun <- function(x) return(sin(x))
input_raw <- t(seq(0,3.5,length=50))
output_raw <- apply(input_raw, 2, tested_fun)
num_iter <- 200
err_fun <- function(x) return(x * x)
do_plot <- TRUE
pop_size <- 100

learning_set_indices <- sample(1:length(input_raw), floor(length(input_raw) * 0.7))
learning_set_indices <- learning_set_indices[order(learning_set_indices)]

learning_input_raw <- t(input_raw[learning_set_indices])
learning_output_raw <- t(output_raw[learning_set_indices])

test_input_raw <- t(input_raw[-learning_set_indices])
test_output_raw <- t(output_raw[-learning_set_indices])

num_neighbors <- 4
knn_test(learning_input_raw, learning_output_raw, test_input_raw, test_output_raw, num_neighbors, err_fun, do_plot)

net_norm <- run_test(learning_input_raw, learning_output_raw, num_iter, err_fun, pop_size, do_plot)
test_result(net_norm, test_input_raw, test_output_raw, err_fun, do_plot)
