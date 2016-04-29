#!/usr/bin/env Rscript
source("neural.r")


input_mat <- gen_matrix(2, 10)
output_mat <- gen_matrix(11, 2)
input <- t(c(0.5, 1, 2, 3, 4))
output <- feed_forward(input_mat, output_mat, input)
print(output)
