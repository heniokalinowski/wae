#!/usr/bin/env Rscript

source("normalization.r")
source("main.r")

startingVector <- c(1, 2, 3, 4)
normalized <- normalize(startingVector)
denormalized <- denormalize(normalized)

if(!all(startingVector == denormalized)) 
{
  warning("normalization test failed")
} 


tested_fun <- function(x) return(sin(x)*x)
input_raw <- t(seq(2,20,length=20))
output_raw <- apply(input_raw, 2, tested_fun)
num_iter <- 100
err_fun <- function(x) return(x * x)
do_plot <- TRUE

net_norm <- run_test(input_raw, output_raw, num_iter, err_fun, do_plot)

input_test <- t(seq(1, 8, length=20))
output_test <- apply(input_test, 2, tested_fun)
res <- test_result(net_norm, input_test, output_test, err_fun, do_plot)
